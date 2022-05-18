"""
Numerically solve model with shifting CBC focus
"""

cd("/Users/julianashwin/Documents/GitHub/CBC_spillovers/model/")

"""
Load data and necessary packages
"""
## Packages
using DataFrames, CSV, Random, GLM, Distributions, LaTeXStrings, Suppressor
using Plots, StatsPlots, StatsBase, Plots.PlotMeasures, TableView, LinearAlgebra
using JuMP, Ipopt, ProgressMeter
gr()


"""
Define global options
"""
function reset_par()
    par = Dict(:σ_ϵ1 => 1.0, # Variance of first fundamental b shock
    :σ_ϵ2 => 1.0, # Variance of second fundamental b shock
    :σ_ν1 => 1.0, # Variance of first CB b signal
    :σ_ν2 => 1.0, # Variance of second CB b signall
    :σ_η1 => 1.0, # Variance of first public b signal
    :σ_η2 => 1.0, # Variance of second public b signal
    )
    return par
end

global par = reset_par()
global s = [0.0, 0.0]
global ŝ = [0.0, 0.0]
global a = [0.5 0.5]




"""
Function to solve CB's problem
"""
function solve_model(par::Dict{Symbol,Float64}, s::Array{Float64,1})
    # Conditional expectation of shocks
    μ̄1 = (0. + par[:σ_ϵ1]/*(par[:σ_ϵ1]+par[:σ_ν1])*s[1]) ::Float64
    μ̄2 = (0. + par[:σ_ϵ2]/*(par[:σ_ϵ2]+par[:σ_ν2])*s[2]) ::Float64
    # Conditional variance
    Σ̄1 = (par[:σ_ϵ1] - par[:σ_ϵ1]^2/(par[:σ_ϵ2]+par[:σ_ν2])) ::Float64
    Σ̄2 = (par[:σ_ϵ2] - par[:σ_ϵ2]^2/(par[:σ_ϵ2]+par[:σ_ν2])) ::Float64
    # Define variables
    model = Model(Ipopt.Optimizer)
    @variable(model, a1 >= 0., start = 0.5)
    @variable(model, a2 >= 0., start = 0.5)
    @variable(model, λ1, start = 0.0)
    @variable(model, λ2, start = 0.0)
    # Constraints
    @NLconstraint(model, λ1 == par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1] + (1-a1)^2*par[:σ_η1]))
    @NLconstraint(model, λ2 == par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2] + (1-a2)^2*par[:σ_η2]))
    @constraint(model, a1 + a2 == 1.0)
    # Objective
    @NLobjective(model, Min,
        ((λ1- (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*s[1]^2 +
            λ1^2*(1-a1)^2*par[:σ_η1] + par[:σ_ϵ1]*(1 - par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) +
        ((λ2-(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*s[2]^2 +
            λ2^2*(1-a2)^2*par[:σ_η2] + par[:σ_ϵ2]*(1 - par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) )

    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    #value(λ1)
    #value(λ2)
    # Store key values
    μ = [μ̄1, μ̄2] ::Array{Float64,1}
    Σ = [Σ̄1, Σ̄2] ::Array{Float64,1}
    attention = [value(a1), value(a2)] ::Array{Float64,1}

    return attention, μ, Σ

end


"""
Function that simulates the system
"""
function simulate_model(par::Dict{Symbol,Float64}, sim_df::DataFrame)

    ## Draw the correlated shocks
    nruns = nrow(sim_df) ::Int64
    ϵ1 = rand(Normal(0.0,par[:σ_ϵ1]), nruns) ::Array{Float64,1}
    ϵ2 = rand(Normal(0.0,par[:σ_ϵ2]), nruns) ::Array{Float64,1}
    sim_df.ϵ1 = ϵ1::Array{Float64,1}
    sim_df.ϵ2 = ϵ2::Array{Float64,1}
    sim_df.η1 = sqrt(par[:σ_η1])*randn(nruns) ::Array{Float64,1}
    sim_df.η2 = sqrt(par[:σ_η2])*randn(nruns) ::Array{Float64,1}
    # Populate dataframe with private signals
    sim_df.s1 = sim_df.ϵ1 + sqrt(par[:σ_ν1])*randn(nruns)::Array{Float64,1}
    sim_df.s2 = sim_df.ϵ2 + sqrt(par[:σ_ν2])*randn(nruns)::Array{Float64,1}

    prog = Progress(nruns, 1)
    for ii in 1:nruns
        # Solve central bank c's communication problem
        s = [sim_df.s1[ii], sim_df.s2[ii]] ::Array{Float64,1}
        a, μ, Σ  = solve_model(par, s)
        sim_df.a1[ii] = a[1]
        sim_df.a2[ii] = a[2]
        sim_df.μ1[ii] = μ[1]
        sim_df.μ2[ii] = μ[2]
        sim_df.Σ1[ii] = Σ[1]
        sim_df.Σ2[ii] = Σ[2]
        # Generate the public signals for central bank c
        sim_df.ŝ1[ii] = sim_df.μ1[ii] + (1.0 - a[1])*sqrt(par[:σ_η1])*randn()
        sim_df.ŝ2[ii] = sim_df.μ2[ii] + (1.0 - a[2])*sqrt(par[:σ_η2])*randn()

        next!(prog)
    end

    return sim_df
end


"""
Function to assess sensitivity of a_b1 to various parameters
"""
function sensitivity_analysis(param, prange; nruns = 1000)

    colvars = [:ϵ1, :ϵ2, :η1, :η2, :s1, :s2, :ŝ1, :ŝ2, :a1, :a2, :μ1, :μ2,
        :Σ1, :Σ2, :σ_ϵ1, :σ_ϵ2, :σ_ν1, :σ_ν2, :σ_η1, :σ_η2]
    ## Create df
    sim_df = DataFrame(repeat([0.],nruns*length(prange), length(colvars)), colvars)

    sim_df[:,param] = repeat(prange, inner = nruns)
    # Loop over parameter values
    for pp in prange
        # Set parameter values
        par = reset_par()
        par[param] = pp
        # Simulate "nruns" times
        display(println("Simulating for ", string(param), " = ", string(pp)))
        obs = sim_df[:,param] .== pp

        sim_df[obs,:] = simulate_model(par, sim_df[obs,:])
    end

    return sim_df
end



"""
Distribution of a_b1|s_b1
"""
## Variables to store
colvars = [:ϵ1, :ϵ2, :η1, :η2, :s1, :s2, :ŝ1, :ŝ2, :a1, :a2, :μ1, :μ2,
    :Σ1, :Σ2, :σ_ϵ1, :σ_ϵ2, :σ_ν1, :σ_ν2, :σ_η1, :σ_η2]
## Create df
sim_df = DataFrame(repeat([0.],100000, length(colvars)), colvars)
## Simulate
par = reset_par()
sim_df = simulate_model(par, sim_df)
CSV.write("model_results/sim_df.csv",sim_df)
# Plot heatmap
plt_s1b = histogram2d(sim_df.s1, sim_df.a1, nbins=100, weights = repeat([1/nrow(sim_df)],nrow(sim_df)),
    xlabel = raw"$s_{i,t}$", ylabel = raw"$a_{i,t}$", c=:blues)
display(plt_s1b)
savefig("model_figs/attention_signal.pdf")

"""
How do variances affect the average attention given to a_b1?
"""
## σ_ϵb1
σ_ϵ1_df = sensitivity_analysis(:σ_ϵ1, 0.1:0.1:3, nruns = 50000)
σ_ϵ1_meds = combine(groupby(σ_ϵ1_df,:σ_ϵ1),:a1=>median=>:a1)
#CSV.write("model_results/sigma_epsilon_df.csv",σ_ϵ1_df)
plt_ϵ1 = histogram2d(σ_ϵ1_df.σ_ϵ1, σ_ϵ1_df.a1, nbins=(0.1:0.1:3,0:0.025:1),
    weights = repeat([1/nrow(σ_ϵ1_df)],nrow(σ_ϵ1_df)),
    xlabel = raw"$\sigma^2_{\epsilon,i}/\sigma^2_{\epsilon,j}$", ylabel = raw"$a_{i,t}$", c=:blues)
plt_ϵ1 = plot!(σ_ϵ1_meds.σ_ϵ1, σ_ϵ1_meds.a1, label = false, color = :black)
display(plt_ϵ1)
savefig("model_figs/attention_epsilon.pdf")

## σ_νb1
σ_ν1_df = sensitivity_analysis(:σ_ν1, 0.1:0.1:3, nruns = 50000)
σ_ν1_meds = combine(groupby(σ_ν1_df,:σ_ν1),:a1=>median=>:a1)
#CSV.write("model_results/sigma_nu_df.csv",σ_ν1_df)
plt_ν1 = histogram2d(σ_ν1_df.σ_ν1, σ_ν1_df.a1, nbins=(0.1:0.1:3,0:0.025:1),
    weights = repeat([1/nrow(σ_ν1_df)],nrow(σ_ν1_df)),
    xlabel = raw"$\sigma^2_{\nu,i}/\sigma^2_{\nu,j}$", ylabel = raw"$a_{i,t}$", c=:blues)
plt_ν1 = plot!(σ_ν1_meds.σ_ν1, σ_ν1_meds.a1, label = false, color = :black)
display(plt_ν1)
savefig("model_figs/attention_nu.pdf")

## σ_ηb1
σ_η1_df = sensitivity_analysis(:σ_η1, 0.1:0.1:3, nruns = 50000)
σ_η1_meds = combine(groupby(σ_η1_df,:σ_η1),:a1=>median=>:a1)
#CSV.write("model_results/sigma_eta_df.csv",σ_η1_df)
plt_η1 = plot(σ_η1_df.σ_η1, σ_η1_df.a1,  nbins=(0.1:0.1:3,0:0.025:1),
    weights = repeat([1/nrow(σ_η1_df)],nrow(σ_η1_df)),ylabel = raw"$a_{i,t}$",
    xlabel = raw"$\sigma^2_{\eta,i}/\sigma^2_{\eta,j}$")
plt_η1 = plot!(σ_η1_meds.σ_η1, σ_η1_meds.a1, label = false, color = :black)
display(plt_η1)
savefig("model_figs/attention_nu.pdf")

## σ_bc1
σ_bc1_df = sensitivity_analysis(:σ_bc1, 0.0:0.05:0.95, nruns = 50000)
CSV.write("model_results/sigma_bc_df.csv",σ_bc1_df)
plt_σ_bc1 = plot(σ_bc1_df.param, σ_bc1_df.a1, legend = false, ylabel = raw"$a_{b,i,t}$",
    xlabel = raw"$\sigma_{bc,i}$")

"""
How do variances affect the co-movement between
"""
## σ_νc
σ_bc_df = cor_analysis([:σ_bc1, :σ_bc2], 0.0:0.05:0.95, par, nruns = 200000)
CSV.write("model_results/sigma_bc_cor_df.csv",σ_νc_df)
plt_σ_bc_cor = plot(σ_bc_df.param1, σ_bc_df.Cor_ab1ac1, legend = false, #ylim = (0.0, 0.25),
    ylabel = raw"$Cor(a_{b,i,t},a_{c,i,t})$", xlabel = raw"$\sigma_{bc}$")

## σ_νc
σ_νc_df = cor_analysis([:σ_νc1, :σ_νc2], 0.1:0.1:3, par, nruns = 200000)
CSV.write("model_results/sigma_nuc_df.csv",σ_νc_df)
plt_σ_νc = plot(σ_νc_df.param1, σ_νc_df.ab1, legend = false, ylabel = raw"$a_{b,i,t}$",
    xlabel = raw"$\sigma_{c,\nu}$")
plt_σ_νc_cor = plot(σ_νc_df.param1, σ_νc_df.Cor_ab1ac1, legend = false, ylim = (0.0, 0.25),
    ylabel = raw"$Cor(a_{b,i,t},a_{c,i,t})$", xlabel = raw"$\sigma_{c,\nu}$")
## σ_ηc
σ_ηc_df = cor_analysis([:σ_ηc1, :σ_ηc2], 0.1:0.1:3, par, nruns = 200000)
CSV.write("model_results/sigma_etac_df.csv",σ_ηc_df)
plt_σ_ηc = plot(σ_ηc_df.param1, σ_ηc_df.ab1, legend = false, ylabel = raw"$a_{b,i,t}$",
    xlabel = raw"$\sigma_{c,\eta}$")
plt_σ_ηc_cor = plot(σ_ηc_df.param1, σ_ηc_df.Cor_ab1ac1, legend = false, ylim = (0.0, 0.25),
    ylabel = raw"$Cor(a_{b,i,t},a_{c,i,t})$", xlabel = raw"$\sigma_{c,\eta}$")




"""
Combine plots
"""
## Options
ticksize = 6
## One bank example
plot(layout = (2,2))
histogram2d!(sim_df.s_b1, sim_df.a_b1, nbins=100, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$s_{1,t}$", xtickfontsize = ticksize, ytickfontsize = ticksize,
    subplot = 1)
plot!(σ_ϵb1_df.param, σ_ϵb1_df.a1, legend = false, ylabel = raw"$\mathbb{E} a_{1,t}$",
    xlabel = raw"$\sigma^2_{\epsilon,1}$", xtickfontsize = ticksize, ytickfontsize = ticksize,
    subplot = 2)
plot!(σ_νb1_df.param, σ_νb1_df.a1, legend = false, ylabel = raw"$\mathbb{E} a_{1,t}$",
    xlabel = raw"$\sigma^2_{\nu,1}$", xtickfontsize = ticksize, ytickfontsize = ticksize,
    subplot = 3)
plot!(σ_ηb1_df.param, σ_ηb1_df.a1, legend = false, ylabel = raw"$\mathbb{E} a_{1,t}$",
    xlabel = raw"$\sigma^2_{\eta,1}$", xtickfontsize = ticksize, ytickfontsize = ticksize,
    subplot = 4)
# Save
savefig("model_figs/attention_sensitivity.pdf")


## Co-movement example
plot(layout = (2,3))
plot!(σ_bc1_df.param, σ_bc1_df.a1, legend = false, ylabel = raw"$\mathbb{E} a_{b,1,t}$",
    xlabel = raw"$\sigma_{bc,1}$", subplot = 1)
plot!(σ_bc_df.param1, σ_bc_df.Cor_ab1ac1, legend = false, #ylim = (0.0, 0.25),
    ylabel = raw"$Cor(a_{b,1,t},a_{c,1,t})$", xlabel = raw"$\sigma_{bc}$", subplot = 4)
plot!(σ_νc_df.param1, σ_νc_df.ab1, legend = false, ylabel = raw"$\mathbb{E} a_{b,1,t}$",
    xlabel = raw"$\sigma^2_{c,\nu}$", subplot = 2)
plot!(σ_νc_df.param1, σ_νc_df.Cor_ab1ac1, legend = false, ylim = (0.0, 0.25),
    ylabel = raw"$Cor(a_{b,1,t},a_{c,1,t})$", xlabel = raw"$\sigma^2_{c,\nu}$", subplot = 5)
plot!(σ_ηc_df.param1, σ_ηc_df.ab1, legend = false, ylabel = raw"$\mathbb{E} a_{b,1,t}$",
    xlabel = raw"$\sigma^2_{c,\eta}$", subplot = 3)
plot!(σ_ηc_df.param1, σ_ηc_df.Cor_ab1ac1, legend = false, ylim = (0.0, 0.25),
    ylabel = raw"$Cor(a_{b,1,t},a_{c,1,t})$", xlabel = raw"$\sigma^2_{c,\eta}$", subplot = 6)
# Save
savefig("model_figs/comovement_sensitivity.pdf")



"""
End of script
"""
