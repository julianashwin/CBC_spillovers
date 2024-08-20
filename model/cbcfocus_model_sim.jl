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
    :σ_ζ1 => 1.0, # Variance of first private sector b signal
    :σ_ζ2 => 1.0, # Variance of second private sector b signal
    )
    return par
end

global par = reset_par()
global s = [0.0, 0.0]
global ŝ = [0.0, 0.0]
global a = [0.5 0.5]




"""
Function to solve CB's problem, without private signal
"""
function solve_model_noq(par::Dict{Symbol,Float64}, s::Array{Float64,1})
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
Function to solve CB's problem, with private signal
"""
function solve_model_q(par::Dict{Symbol,Float64}, s::Array{Float64,1})
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
    @variable(model, λq1, start = 0.0)
    @variable(model, λq2, start = 0.0)
    @variable(model, λs1, start = 0.0)
    @variable(model, λs2, start = 0.0)
    # Constraints
    @NLconstraint(model, λq1 == (par[:σ_ϵ1]*(par[:σ_ν1] + (1-a1)^2))/
        (par[:σ_ϵ1]*par[:σ_ζ1] + (par[:σ_ϵ1] + par[:σ_ζ1]) * (par[:σ_ν1] + (1-a1)^2)))
    @NLconstraint(model, λq2 == (par[:σ_ϵ2]*(par[:σ_ν2] + (1-a2)^2))/
        (par[:σ_ϵ2]*par[:σ_ζ2] + (par[:σ_ϵ2] + par[:σ_ζ2]) * (par[:σ_ν2] + (1-a2)^2)))
    @NLconstraint(model, λs1 == (par[:σ_ϵ1]*par[:σ_ζ1])/
        (par[:σ_ϵ1]*par[:σ_ζ1] + (par[:σ_ϵ1] + par[:σ_ζ1]) * (par[:σ_ν1] + (1-a1)^2)))
    @NLconstraint(model, λs2 == (par[:σ_ϵ2]*par[:σ_ζ2])/
        (par[:σ_ϵ2]*par[:σ_ζ2] + (par[:σ_ϵ2] + par[:σ_ζ2]) * (par[:σ_ν2] + (1-a2)^2)))
    @constraint(model, a1 + a2 == 1.0)
    # Objective
    @NLobjective(model, Min,
        ( ( ((λs1 - (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*s[1]^2 +
            λq1 * (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) * ((λq1 - 2) *
            (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) + 2 * λs1) * s[1]^2 +
            λq1^2 * par[:σ_ζ1] +
            λs1^2 * (1-a1)^2 +
            (λq1-1) ^ 2 * par[:σ_ϵ1]*(1 - par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1]))
            )
        +
          ( ((λs2 - (par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*s[2]^2 +
            λq2 * (par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) * ((λq2 - 2) *
            (par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) + 2 * λs2) * s[2]^2 +
            λq2^2 * par[:σ_ζ2] +
            λs2^2 * (1-a2)^2 +
            (λq2-1) ^ 2 * par[:σ_ϵ2]*(1 - par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2]))
          )
        )
    )

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
    lambdas = [value(λs1), value(λs2), value(λq1), value(λq2)] ::Array{Float64,1}

    return attention, μ, Σ, lambdas

end


"""
Function that simulates the system without private signal
"""
function simulate_model_noq(par::Dict{Symbol,Float64}, sim_df::DataFrame)

    ## Draw the correlated shocks
    nruns = nrow(sim_df) ::Int64
    ϵ1 = rand(Normal(0.0,par[:σ_ϵ1]), nruns) ::Array{Float64,1}
    ϵ2 = rand(Normal(0.0,par[:σ_ϵ2]), nruns) ::Array{Float64,1}
    sim_df.ϵ1 = ϵ1::Array{Float64,1}
    sim_df.ϵ2 = ϵ2::Array{Float64,1}
    #sim_df.η1 = sqrt(par[:σ_η1])*randn(nruns) ::Array{Float64,1}
    #sim_df.η2 = sqrt(par[:σ_η2])*randn(nruns) ::Array{Float64,1}
    # Populate dataframe with private signals
    sim_df.s1 = sim_df.ϵ1 + sqrt(par[:σ_ν1])*randn(nruns)::Array{Float64,1}
    sim_df.s2 = sim_df.ϵ2 + sqrt(par[:σ_ν2])*randn(nruns)::Array{Float64,1}

    prog = Progress(nruns, 1)
    for ii in 1:nruns
        # Solve central bank c's communication problem
        s = [sim_df.s1[ii], sim_df.s2[ii]] ::Array{Float64,1}
        a, μ, Σ  = solve_model_noq(par, s)
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
Function that simulates the system with private signal
"""
function simulate_model_q(par::Dict{Symbol,Float64}, sim_df::DataFrame)

    ## Draw the correlated shocks
    nruns = nrow(sim_df) ::Int64
    ϵ1 = rand(Normal(0.0,par[:σ_ϵ1]), nruns) ::Array{Float64,1}
    ϵ2 = rand(Normal(0.0,par[:σ_ϵ2]), nruns) ::Array{Float64,1}
    sim_df.ϵ1 = ϵ1::Array{Float64,1}
    sim_df.ϵ2 = ϵ2::Array{Float64,1}
    sim_df.η1 = sqrt(par[:σ_η1])*randn(nruns) ::Array{Float64,1}
    sim_df.η2 = sqrt(par[:σ_η2])*randn(nruns) ::Array{Float64,1}
    sim_df.ζ1 = sqrt(par[:σ_ζ1])*randn(nruns) ::Array{Float64,1}
    sim_df.ζ2 = sqrt(par[:σ_ζ2])*randn(nruns) ::Array{Float64,1}
    # Populate dataframe with private signals
    sim_df.s1 = sim_df.ϵ1 + sqrt(par[:σ_ν1])*randn(nruns)::Array{Float64,1}
    sim_df.s2 = sim_df.ϵ2 + sqrt(par[:σ_ν2])*randn(nruns)::Array{Float64,1}

    prog = Progress(nruns, 1)
    for ii in 1:nruns
        # Solve central bank c's communication problem
        s = [sim_df.s1[ii], sim_df.s2[ii]] ::Array{Float64,1}
        a, μ, Σ, λ  = solve_model_q(par, s)
        sim_df.a1[ii] = a[1]
        sim_df.a2[ii] = a[2]
        sim_df.μ1[ii] = μ[1]
        sim_df.μ2[ii] = μ[2]
        sim_df.Σ1[ii] = Σ[1]
        sim_df.Σ2[ii] = Σ[2]
        sim_df.λs1[ii] = λ[1]
        sim_df.λs2[ii] = λ[2]
        sim_df.λq1[ii] = λ[3]
        sim_df.λq2[ii] = λ[4]
        # Generate the public signals for central bank c
        sim_df.ŝ1[ii] = sim_df.s1[ii] + (1.0 - a[1])*sim_df.η1[ii]
        sim_df.ŝ2[ii] = sim_df.s2[ii] + (1.0 - a[2])*sim_df.η1[ii]
        sim_df.q1[ii] = sim_df.ϵ1[ii] + sim_df.ζ1[1]
        sim_df.q2[ii] = sim_df.ϵ2[ii] + sim_df.ζ2[1]

        next!(prog)
    end

    return sim_df
end

"""
Function to assess sensitivity of a_b1 to various parameters
"""
function sensitivity_analysis(param, prange; nruns = 1000)

    colvars = [:ϵ1, :ϵ2, :η1, :η2, :ζ1, :ζ2, :s1, :s2, :ŝ1, :ŝ2, :q1, :q2,
        :a1, :a2, :μ1, :μ2, :Σ1, :Σ2, :λs1, :λs2, :λq1, :λq2,
        :σ_ϵ1, :σ_ϵ2, :σ_ν1, :σ_ν2, :σ_η1, :σ_η2, :σ_ζ1, :σ_ζ2]


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

        sim_df[obs,:] = simulate_model_q(par, sim_df[obs,:])
    end

    return sim_df
end



"""
Distribution of a_b1|s_b1
"""
## Variables to store
colvars = [:ϵ1, :ϵ2, :η1, :η2, :ζ1, :ζ2, :s1, :s2, :ŝ1, :ŝ2, :q1, :q2,
    :a1, :a2, :μ1, :μ2, :Σ1, :Σ2, :λs1, :λs2, :λq1, :λq2,
    :σ_ϵ1, :σ_ϵ2, :σ_ν1, :σ_ν2, :σ_η1, :σ_η2, :σ_ζ1, :σ_ζ2]
## Create df
sim_df = DataFrame(repeat([0.],1000000, length(colvars)), colvars)
## Simulate
par = reset_par()
#par[:σ_ζ1] = 2;par[:σ_ζ2] = 2;
sim_df = simulate_model_q(par, sim_df)
sim_df.s1_bins = round.(sim_df.s1, digits = 1)
sort!(sim_df, :s1)
sim_meds = combine(groupby(sim_df,:s1_bins),:a1=>median=>:a1)
sim_meds = sim_meds[abs.(sim_meds.s1_bins) .<= 5.0,:]
#CSV.write("model_results/sim_df.csv",sim_df)
# Plot heatmap
plt_s1b = histogram2d(sim_df.s1, sim_df.a1, nbins=100, weights = repeat([1/nrow(sim_df)],nrow(sim_df)),
    xlabel = raw"$s_{i,t}$", ylabel = raw"$a_{i,t}$", c=:blues)
plt_s1b = plot!(sim_meds.s1_bins, sim_meds.a1, label = false, color = :black)
plot!(colorbar = false, xlims = (-6,6), ylims = (0.,1.), size = (400,300))
display(plt_s1b)
savefig("model_figs/attention_signal.pdf")


histogram2d(sim_df.q1, sim_df.a1, nbins=100, weights = repeat([1/nrow(sim_df)],nrow(sim_df)),
    c=:blues)



"""
How do variances affect the average attention given to a_b1?
"""
## σ_ϵb1
σ_ϵ1_df = sensitivity_analysis(:σ_ϵ1, 0.0:0.1:3, nruns = 50000)
σ_ϵ1_meds = combine(groupby(σ_ϵ1_df,:σ_ϵ1),:a1=>median=>:a1)
#CSV.write("model_results/sigma_epsilon_df.csv",σ_ϵ1_df)
plt_ϵ1 = histogram2d(σ_ϵ1_df.σ_ϵ1, σ_ϵ1_df.a1, nbins=(0.0:0.1:3,0:0.025:1),
    weights = repeat([1/nrow(σ_ϵ1_df)],nrow(σ_ϵ1_df)),
    xlabel = raw"$\sigma^2_{\epsilon,i}/\sigma^2_{\epsilon,j}$", ylabel = raw"$a_{i,t}$", c=:blues)
plt_ϵ1 = plot!(σ_ϵ1_meds.σ_ϵ1, σ_ϵ1_meds.a1, label = false, color = :black)
plot!(colorbar = false, ylims = (0.,1.), size = (400,300))
display(plt_ϵ1)
savefig("model_figs/attention_epsilon.pdf")

## σ_νb1
σ_ν1_df = sensitivity_analysis(:σ_ν1, 0.0:0.1:3, nruns = 50000)
σ_ν1_meds = combine(groupby(σ_ν1_df,:σ_ν1),:a1=>median=>:a1)
#CSV.write("model_results/sigma_nu_df.csv",σ_ν1_df)
plt_ν1 = histogram2d(σ_ν1_df.σ_ν1, σ_ν1_df.a1, nbins=(0.0:0.1:3,0:0.025:1),
    weights = repeat([1/nrow(σ_ν1_df)],nrow(σ_ν1_df)),
    xlabel = raw"$\sigma^2_{\nu,i}/\sigma^2_{\nu,j}$", ylabel = raw"$a_{i,t}$", c=:blues)
plt_ν1 = plot!(σ_ν1_meds.σ_ν1, σ_ν1_meds.a1, label = false, color = :black)
plot!(colorbar = false, ylims = (0.,1.), size = (400,300))
display(plt_ν1)
savefig("model_figs/attention_nu.pdf")

## σ_ζ1
σ_ζ1_df = sensitivity_analysis(:σ_ζ1, 0.0:0.1:3.0, nruns = 50000)
σ_ζ1_meds = combine(groupby(σ_ζ1_df,:σ_ζ1),:a1=>median=>:a1_med,
    :q1=>var=>:q1_var, :ζ1=>var=>:ζ1_var, :λq1=>median=>:λq1_med)

σ_ζ1_df.λq1

plt_ζ1 = histogram2d(σ_ζ1_df.σ_ζ1, σ_ζ1_df.a1,  nbins=(0.0:0.1:3,0:0.025:1),
    weights = repeat([1/nrow(σ_ζ1_df)],nrow(σ_ζ1_df)),
    xlabel = raw"$\sigma^2_{\zeta,i}/\sigma^2_{\zeta,j}$",ylabel = raw"$a_{i,t}$", c=:blues)
plt_ζ1 = plot!(σ_ζ1_meds.σ_ζ1, σ_ζ1_meds.a1, label = false, color = :black)
plot!(colorbar = false, ylims = (0.,1.), size = (400,300))
display(plt_ζ1)
savefig("model_figs/attention_zeta.pdf")



"""
End of script
"""
