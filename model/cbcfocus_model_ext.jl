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
    par = Dict(:σ_ϵb1 => 1.0, # Variance of first fundamental b shock
    :σ_ϵb2 => 1.0, # Variance of second fundamental b shock
    :σ_νb1 => 1.0, # Variance of first CB b signal
    :σ_νb2 => 1.0, # Variance of second CB b signall
    :σ_ηb1 => 1.0, # Variance of first public b signal
    :σ_ηb2 => 1.0, # Variance of second public b signal
    :σ_ϵc1 => 1.0, # Variance of first fundamental c shock
    :σ_ϵc2 => 1.0, # Variance of second fundamental c shock
    :σ_νc1 => 1.0, # Variance of first CB c signal
    :σ_νc2 => 1.0, # Variance of second CB c signal
    :σ_ηc1 => 1.0, # Variance of first public c signal
    :σ_ηc2 => 1.0, # Variance of second public c signal
    :σ_bc1 => 0.0, # Covariance of first fundamental across b and c
    :σ_bc2 => 0.0, # Covariance of second fundamental across b and c
    )
    return par
end

global par = reset_par()
global s_b = [0.0, 0.0]
global s_c = [0.0, 0.0]
global ŝ_b = [0.0, 0.0]
global ŝ_c = [0.0, 0.0]
global a_b = [0.5 0.5]
global a_c = [0.5 0.5]

"""
Define distribution of ϵ_a, s_a and ŝ_b
"""
# covariance of ϵ_a1, s_a1, ŝ_b1
Σ_b1 = [(par[:σ_ϵb1]) (par[:σ_ϵb1]) (par[:σ_bc1])
    (par[:σ_ϵb1]) (par[:σ_ϵb1]+par[:σ_νb1]) (par[:σ_bc1])
    (par[:σ_bc1]) (par[:σ_bc1]) (par[:σ_ϵc1]+par[:σ_νc1]+(1-a_c[1])^2*par[:σ_ηc1])]
# covariance of ϵ_a2, s_a2, ŝ_b2
Σ_b2 = [(par[:σ_ϵb2]) (par[:σ_ϵb2]) (par[:σ_bc2])
    (par[:σ_ϵb2]) (par[:σ_ϵb2]+par[:σ_νb2]) (par[:σ_bc2])
    (par[:σ_bc2]) (par[:σ_bc2]) (par[:σ_ϵc2]+par[:σ_νc2]+(1-a_c[2])^2*par[:σ_ηc2])]



"""
Function to solve CB c's problem (does not observe ŝ_b)
"""
function solve_model_c(par::Dict{Symbol,Float64}, s_c::Array{Float64,1})

    Σ_c1 = [(par[:σ_ϵc1]) (par[:σ_ϵc1])
        (par[:σ_ϵc1]) (par[:σ_ϵc1]+par[:σ_νc1])] ::Array{Float64,2}
    Σ_c2 = [(par[:σ_ϵc2]) (par[:σ_ϵc2])
        (par[:σ_ϵc2]) (par[:σ_ϵc2]+par[:σ_νc2])] ::Array{Float64,2}
    # Partition these covariance matrices
    Σ11_c1 = Matrix(transpose(hcat(Σ_c1[1,1]))) ::Array{Float64,2}
    Σ12_c1 = Matrix(transpose(hcat(Σ_c1[1,2]))) ::Array{Float64,2}
    Σ21_c1 = hcat(Σ_c1[1,2]) ::Array{Float64,2}
    Σ22_c1 = hcat(Σ_c1[2,2]) ::Array{Float64,2}
    Σ11_c2 = Matrix(transpose(hcat(Σ_c2[1,1]))) ::Array{Float64,2}
    Σ12_c2 = Matrix(transpose(hcat(Σ_c2[1,2]))) ::Array{Float64,2}
    Σ21_c2 = hcat(Σ_c2[1,2]) ::Array{Float64,2}
    Σ22_c2 = hcat(Σ_c2[2,2]) ::Array{Float64,2}
    # Conditional expectation of shocks
    μ̄_1 = ([0.] + Σ12_c1*inv(Σ22_c1)*[s_c[1]])[1] ::Float64
    μ̄_2 = ([0.] + Σ12_c2*inv(Σ22_c2)*[s_c[2]])[1] ::Float64
    # Conditional variance
    Σ̄_1 = (Σ11_c1 - Σ12_c1*inv(Σ22_c1)*Σ21_c1)[1] ::Float64
    Σ̄_2 = (Σ11_c2 - Σ12_c2*inv(Σ22_c2)*Σ21_c2)[1] ::Float64

    # Define variables
    model = Model(Ipopt.Optimizer)
    @variable(model, a1 >= 0., start = 0.0)
    @variable(model, a2 >= 0., start = 0.0)
    @variable(model, λ1, start = 0.0)
    @variable(model, λ2, start = 0.0)
    # Constraints
    @NLconstraint(model, λ1 == par[:σ_ϵc1]/(par[:σ_ϵc1] + par[:σ_νc1] + (1-a1)^2*par[:σ_ηc1]))
    @NLconstraint(model, λ2 == par[:σ_ϵc2]/(par[:σ_ϵc2] + par[:σ_νc2] + (1-a2)^2*par[:σ_ηc2]))
    @constraint(model, a1 + a2 == 1.0)
    # Objective
    @NLobjective(model, Min,
        ((λ1- (par[:σ_ϵc1]/(par[:σ_ϵc1] + par[:σ_νc1])))^2)*s_c[1]^2 +
            λ1^2*(1-a1)^2*par[:σ_ηc1] + par[:σ_ϵc1]*(1 - par[:σ_ϵc1]/(par[:σ_ϵc1] + par[:σ_νc1])) +
        ((λ2-(par[:σ_ϵc2]/(par[:σ_ϵc2] + par[:σ_νc2])))^2)*s_c[2]^2 +
            λ2^2*(1-a2)^2*par[:σ_ηc2] + par[:σ_ϵc2]*(1 - par[:σ_ϵc2]/(par[:σ_ϵc2] + par[:σ_νc2])) )

    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    #value(λ1)
    #value(λ2)
    # Store key values
    μ = [μ̄_1, μ̄_2] ::Array{Float64,1}
    Σ = [Σ̄_1, Σ̄_2] ::Array{Float64,1}
    attention = [value(a1), value(a2)] ::Array{Float64,1}

    return attention, μ, Σ

end


"""
Function to solve CB b's problem
"""
function solve_model_b(par::Dict{Symbol,Float64}, s_b::Array{Float64,1}, ŝ_c::Array{Float64,1}, a_c::Array{Float64,1})

    Σ_b1 = [(par[:σ_ϵb1]) (par[:σ_ϵb1]) (par[:σ_bc1])
        (par[:σ_ϵb1]) (par[:σ_ϵb1]+par[:σ_νb1]) (par[:σ_bc1])
        (par[:σ_bc1]) (par[:σ_bc1]) (par[:σ_ϵc1]+par[:σ_νc1]+(1-a_c[1])^2*par[:σ_ηc1])] ::Array{Float64,2}
    Σ_b2 = [(par[:σ_ϵb2]) (par[:σ_ϵb2]) (par[:σ_bc2])
        (par[:σ_ϵb2]) (par[:σ_ϵb2]+par[:σ_νb2]) (par[:σ_bc2])
        (par[:σ_bc2]) (par[:σ_bc2]) (par[:σ_ϵc2]+par[:σ_νc2]+(1-a_c[2])^2*par[:σ_ηc2])] ::Array{Float64,2}
    # Partition these covariance matrices
    Σ11_b1 = Matrix(transpose(hcat(Σ_b1[1,1]))) ::Array{Float64,2}
    Σ12_b1 = Matrix(transpose(hcat(Σ_b1[1,2:3]))) ::Array{Float64,2}
    Σ21_b1 = hcat(Σ_b1[1,2:3]) ::Array{Float64,2}
    Σ22_b1 = Σ_b1[2:3,2:3] ::Array{Float64,2}
    Σ11_b2 = Matrix(transpose(hcat(Σ_b2[1,1]))) ::Array{Float64,2}
    Σ12_b2 = Matrix(transpose(hcat(Σ_b2[1,2:3]))) ::Array{Float64,2}
    Σ21_b2 = hcat(Σ_b2[1,2:3]) ::Array{Float64,2}
    Σ22_b2 = Σ_b2[2:3,2:3] ::Array{Float64,2}
    # Conditional expectations coefficients
    ω_b1 = Σ12_b1*inv(Σ22_b1)
    ω_b2 = Σ12_b2*inv(Σ22_b2)
    # Conditional expectations
    μ̄_1 = ([0.] + ω_b1*[s_b[1]; ŝ_c[1]])[1] ::Float64
    μ̄_2 = ([0.] + ω_b2*[s_b[2]; ŝ_c[2]])[1] ::Float64
    # Conditional variance
    Σ̄_1 = (Σ11_b1 - Σ12_b1*inv(Σ22_b1)*Σ21_b1)[1] ::Float64
    Σ̄_2 = (Σ11_b2 - Σ12_b2*inv(Σ22_b2)*Σ21_b2)[1] ::Float64

    model = Model(Ipopt.Optimizer)
    @variable(model, a1 >= 0., start = 0.5)
    @variable(model, a2 >= 0., start = 0.5)
    @variable(model, λ1, start = 0.0)
    @variable(model, λ2, start = 0.0)
    # Constraints
    @NLconstraint(model, λ1 == (ω_b1[1]*par[:σ_ϵb1] + ω_b1[2]*par[:σ_bc1])/
        (ω_b1[1]^2*(par[:σ_ϵb1] + par[:σ_νb1] + (1-a1)^2*par[:σ_ηb1]) +
        2*ω_b1[1]*ω_b1[2]*(par[:σ_bc1] + (1 - a1)^2*par[:σ_ηb1]) +
        ω_b1[2]^2*(par[:σ_ϵc1] + par[:σ_νc1] + (1-a_c[1])^2*par[:σ_ηc1] + (1-a1)^2*par[:σ_ηb1])))
    @NLconstraint(model, λ2 == (ω_b2[1]*par[:σ_ϵb2] + ω_b2[2]*par[:σ_bc2])/
        (ω_b2[1]^2*(par[:σ_ϵb2] + par[:σ_νb2] + (1-a2)^2*par[:σ_ηb2]) +
        2*ω_b2[1]*ω_b2[2]*(par[:σ_bc2] + (1 - a2)^2*par[:σ_ηb2]) +
        ω_b2[2]^2*(par[:σ_ϵc2] + par[:σ_νc2] + (1-a_c[2])^2*par[:σ_ηc2] + (1-a2)^2*par[:σ_ηb2])))
    @constraint(model, a1 + a2 == 1.0)
    # Objective
    @NLobjective(model, Min,
    ((λ1^2)*(ω_b1[1]^2*s_b[1]^2 + (ω_b1[1]^2 + ω_b1[2]^2)*(1-a1)^2*par[:σ_ηb1] +
        ω_b1[2]^2*ŝ_c[1]^2 + 2*ω_b1[1]*ω_b1[2]*(s_b[1]*ŝ_c[1] + (1-a1)^2*par[:σ_ηb1])) -
        2*λ1*(ω_b1[1]*s_b[1]*μ̄_1 + ω_b1[2]*ŝ_c[1]*μ̄_1) + μ̄_1^2 + Σ̄_1) +
    ((λ2^2)*(ω_b2[1]^2*s_b[2]^2 + (ω_b2[1]^2 + ω_b2[2]^2)*(1-a2)^2*par[:σ_ηb2] +
        ω_b2[2]^2*ŝ_c[2]^2 + 2*ω_b2[1]*ω_b2[2]*(s_b[2]*ŝ_c[2] + (1-a2)^2*par[:σ_ηb2])) -
        2*λ2*(ω_b2[1]*s_b[2]*μ̄_2 + ω_b2[2]*ŝ_c[2]*μ̄_2) + μ̄_2^2 + Σ̄_2) )
    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    # Store key values
    μ = [μ̄_1, μ̄_2] ::Array{Float64,1}
    Σ = [Σ̄_1, Σ̄_2] ::Array{Float64,1}
    attention = [value(a1), value(a2)] ::Array{Float64,1}

    return attention, μ, Σ, ω_b1, ω_b2
end



par[:σ_bc1] = 0.5
"""
Function that simulates the system
"""
function simulate_model(par::Dict{Symbol,Float64}, sim_df::DataFrame)

    ## Draw the correlated shocks
    cov_ϵ1 = [par[:σ_ϵb1] par[:σ_bc1]; par[:σ_bc1] par[:σ_ϵc1]] ::Array{Float64,2}
    cov_ϵ2 = [par[:σ_ϵb2] par[:σ_bc2]; par[:σ_bc2] par[:σ_ϵc2]] ::Array{Float64,2}
    nruns = nrow(sim_df) ::Int64
    ϵ1 = rand(MultivariateNormal([0.0,0.0],cov_ϵ1), nruns) ::Array{Float64,2}
    ϵ2 = rand(MultivariateNormal([0.0,0.0],cov_ϵ2), nruns) ::Array{Float64,2}
    sim_df.ϵ_b1 = ϵ1[1,:]::Array{Float64,1}
    sim_df.ϵ_c1 = ϵ1[2,:]::Array{Float64,1}
    sim_df.ϵ_b2 = ϵ2[1,:]::Array{Float64,1}
    sim_df.ϵ_c2 = ϵ2[2,:]::Array{Float64,1}
    sim_df.η_b1 = sqrt(par[:σ_ηb1])*randn(nruns) ::Array{Float64,1}
    sim_df.η_b2 = sqrt(par[:σ_ηb1])*randn(nruns) ::Array{Float64,1}
    # Populate dataframe with private signals
    sim_df.s_b1 = sim_df.ϵ_b1 + sqrt(par[:σ_νb1])*randn(nruns)::Array{Float64,1}
    sim_df.s_b2 = sim_df.ϵ_b2 + sqrt(par[:σ_νb2])*randn(nruns)::Array{Float64,1}
    sim_df.s_c1 = sim_df.ϵ_c1 + sqrt(par[:σ_νc1])*randn(nruns)::Array{Float64,1}
    sim_df.s_c2 = sim_df.ϵ_c2 + sqrt(par[:σ_νc2])*randn(nruns)::Array{Float64,1}

    prog = Progress(nruns, 1)
    for ii in 1:nruns
        # Solve central bank c's communication problem
        s_c = [sim_df.s_c1[ii], sim_df.s_c2[ii]] ::Array{Float64,1}
        a_c, μ_c, Σ_c  = solve_model_c(par, s_c)
        sim_df.a_c1[ii] = a_c[1]
        sim_df.a_c2[ii] = a_c[2]
        sim_df.μ_c1[ii] = μ_c[1]
        sim_df.μ_c2[ii] = μ_c[2]
        sim_df.Σ_c1[ii] = Σ_c[1]
        sim_df.Σ_c2[ii] = Σ_c[2]
        # Generate the public signals for central bank c
        sim_df.ŝ_c1[ii] = sim_df.μ_c1[ii] + (1.0 - a_c[1])*sqrt(par[:σ_ηc1])*randn()
        sim_df.ŝ_c2[ii] = sim_df.μ_c2[ii] + (1.0 - a_c[2])*sqrt(par[:σ_ηc2])*randn()
        # Extract the necessary input for central bank b's problem
        s_b = [sim_df.s_b1[ii], sim_df.s_b2[ii]] ::Array{Float64,1}
        ŝ_c = [sim_df.ŝ_c1[ii], sim_df.ŝ_c2[ii]] ::Array{Float64,1}
        a_b, μ, Σ, ω_b1, ω_b2 = solve_model_b(par, s_b, ŝ_c, a_c)
        sim_df.a_b1[ii] = a_b[1]
        sim_df.a_b2[ii] = a_b[2]
        sim_df.μ_b1[ii] = μ[1]
        sim_df.μ_b2[ii] = μ[2]
        sim_df.Σ_b1[ii] = Σ[1]
        sim_df.Σ_b2[ii] = Σ[2]
        sim_df.ω_b11[ii] = ω_b1[1]
        sim_df.ω_b12[ii] = ω_b1[2]
        sim_df.ω_b21[ii] = ω_b2[1]
        sim_df.ω_b22[ii] = ω_b2[2]
        # Generate the public signals for central bank b
        sim_df.ŝ_b1[ii] = (ω_b1*[s_b[1] + (1.0 - a_b[1])*sim_df.η_b1[ii];
            ŝ_c[1] + (1.0 - a_b[1])*sim_df.η_b1[ii]])[1]
        sim_df.ŝ_b2[ii] = (ω_b2*[s_b[2] + (1.0 - a_b[2])*sim_df.η_b2[ii];
            ŝ_c[2] + (1.0 - a_b[2])*sim_df.η_b2[ii]])[1]

        next!(prog)
    end

    return sim_df
end


"""
Function to assess sensitivity of a_b1 to various parameters
"""
function sensitivity_analysis(param, prange, par; nruns = 1000)
    sensitivity_df = DataFrame(param = prange)
    sensitivity_df[:,:a1] .= 0.
    sensitivity_df[:,:a2] .= 0.
    # Loop over parameter values
    for ii in 1:nrow(sensitivity_df)
        # Set parameter values
        par = reset_par()
        par[param] = sensitivity_df.param[ii]
        # Create simulation df
        sim_df = DataFrame(repeat([0.],nruns, length(colvars)))
        rename!(sim_df, colvars)
        # Simulate "nruns" times
        println("Simulating for ", string(param), " = ", string(sensitivity_df.param[ii]))
        sim_df = simulate_model(par, sim_df)

        sensitivity_df[ii,:a1] = mean(sim_df.a_b1)
        sensitivity_df[ii,:a2] = mean(sim_df.a_b2)
    end

    return sensitivity_df
end


"""
Function to assess sensitivity of comovement of a_c1 and a_b1 to various parameters
"""
function cor_analysis(param_vec, prange, par; nruns = 1000)
    cor_df = DataFrame(param1 = prange)
    cor_df[:,:param2] = prange
    cor_df[:,:Cor_ab1ac1] .= 0.
    cor_df[:,:Cor_ab2ac2] .= 0.
    cor_df[:,:ab1] .= 0.
    cor_df[:,:ab2] .= 0.
    # Loop over parameter values
    for ii in 1:nrow(cor_df)
        # Set parameter values
        par = reset_par()
        par[:σ_bc1] = 0.5
        par[param_vec[1]] = cor_df.param1[ii]
        par[param_vec[2]] = cor_df.param2[ii]
        # Create simulation df
        sim_df = DataFrame(repeat([0.],nruns, length(colvars)))
        rename!(sim_df, colvars)
        # Simulate "nruns" times
        println("Simulating for ", string.(param_vec), " = ", string(cor_df.param1[ii]))
        sim_df = simulate_model(par, sim_df)

        cor_df[ii,:ab1] = mean(sim_df.a_b1)
        cor_df[ii,:ab2] = mean(sim_df.a_b2)
        cor_df[ii,:Cor_ab1ac1] = cor(sim_df.a_b1, sim_df.a_c1)
        cor_df[ii,:Cor_ab2ac2] = cor(sim_df.a_b2, sim_df.a_c2)
    end

    return cor_df
end



"""
Distribution of a_b1|s_b1
"""
## Variables to store
colvars = [:ϵ_b1, :ϵ_b2, :ϵ_c1, :ϵ_c2, :η_b1, :η_b2, :η_c1, :η_c2,
    :s_b1, :s_b2, :s_c1, :s_c2, :ŝ_b1, :ŝ_b2, :ŝ_c1, :ŝ_c2,
    :a_b1, :a_b2, :a_c1, :a_c2, :μ_b1, :μ_b2, :Σ_b1, :Σ_b2,
     :μ_c1, :μ_c2, :Σ_c1, :Σ_c2, :ω_b11, :ω_b12, :ω_b21, :ω_b22]
## Create df
sim_df = DataFrame(repeat([0.],100000, length(colvars)))
rename!(sim_df, colvars)
## Simulate
par = reset_par()
sim_df = simulate_model(par, sim_df)
CSV.write("model_results/sim_df.csv",sim_df)
# Plot heatmap
plt_s1b = histogram2d(sim_df.s_b1, sim_df.a_b1, nbins=100,
    xlabel = raw"$s_{b,i,t}$", ylabel = raw"$a_{b,i,t}$")


"""
How do variances affect the average attention given to a_b1?
"""
## σ_ϵb1
σ_ϵb1_df = sensitivity_analysis(:σ_ϵb1, 0.1:0.1:3, par, nruns = 100000)
CSV.write("model_results/sigma_epsilonb_df.csv",σ_ϵb1_df)
plt_σ_ϵb1 = plot(σ_ϵb1_df.param, σ_ϵb1_df.a1, legend = false, ylabel = raw"$a_{b,i,t}$",
    xlabel = raw"$\sigma^2_{b,\epsilon,i}$")
## σ_νb1
σ_νb1_df = sensitivity_analysis(:σ_νb1, 0.1:0.1:3, par, nruns = 100000)
CSV.write("model_results/sigma_nub_df.csv",σ_νb1_df)
plt_σ_νb1 = plot(σ_νb1_df.param, σ_νb1_df.a1, legend = false, ylabel = raw"$a_{b,i,t}$",
    xlabel = raw"$\sigma^2_{b,\nu,i}$")
## σ_ηb1
σ_ηb1_df = sensitivity_analysis(:σ_ηb1, 0.1:0.1:3, par, nruns = 100000)
CSV.write("model_results/sigma_etab_df.csv",σ_ηb1_df)
plt_σ_ηb1 = plot(σ_ηb1_df.param, σ_ηb1_df.a1, legend = false, ylabel = raw"$a_{b,i,t}$",
    xlabel = raw"$\sigma^2_{b,\eta,i}$")
## σ_bc1
σ_bc1_df = sensitivity_analysis(:σ_bc1, 0.0:0.05:0.95, par, nruns = 50000)
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
