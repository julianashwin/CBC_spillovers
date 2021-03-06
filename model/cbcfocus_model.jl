"""
Numerically solve model with shifting CBC focus
"""

cd("/Users/julianashwin/Documents/GitHub/CBC_spillovers/")

"""
Load data and necessary packages
"""
## Packages
using DataFrames, CSV, Random, GLM, Distributions, LaTeXStrings, Suppressor
using Plots, StatsPlots, StatsBase, Plots.PlotMeasures, TableView, LinearAlgebra
using JuMP, Ipopt
gr()


"""
Define global options
"""
global N = 2
global signals = [1.0, -1.0]

global par = Dict(:σ_ϵ1 => 1.0, # Variance of fundamental shock
:σ_ν1 => 1.0, # Variance of CB signal
:σ_η1 => 1.0, # Variance of PS signal
:σ_ϵ2 => 1.0, # Variance of fundamental shock
:σ_ν2 => 1.0, # Variance of CB signal
:σ_η2 => 1.0, # Variance of PS signal
:μ => 0.0, # Mean of state variables
:ρ => 0.5, # persistence pf state variables
)




attention = (1/N).*ones(N)


"""
General loss function
"""
function loss(par, signals, a1)
    λ1 = par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1] + (1-a1)^2*par[:σ_η1])
    λ2 = par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2] + (a1)^2*par[:σ_η2])

    L1 = ((λ1- (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*signals[1]^2 +
        λ1^2*(1-a1)^2*par[:σ_η1] + par[:σ_ϵ1]*(par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1]))
    L2 = ((λ2-(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*signals[2]^2 +
        λ2^2*a1^2*par[:σ_η2] + par[:σ_ϵ2]*(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2]))

    L = L1 + L2

    return L
end

"""
How does a1 vary with s1?
"""
## Set default and create df to fill
signals = [0.0, 0.0]
s1_df = DataFrame(s1 = -5.0:0.01:5.0)
s1_df[:,:a1] .= 0.
s1_df[:,:a2] .= 0.
s1_df[:,:λ1] .= 0.
s1_df[:,:λ2] .= 0.
## Loop over possible values of s1
for ii in 1:nrow(s1_df)
    # new value and model
    s1 = s1_df.s1[ii]
    signals = [s1, 0.0]
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
        ((λ1- (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*signals[1]^2 +
            λ1^2*(1-a1)^2*par[:σ_η1] + par[:σ_ϵ1]*(par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) +
        ((λ2-(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*signals[2]^2 +
            λ2^2*(1-a2)^2*par[:σ_η2] + par[:σ_ϵ2]*(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) )
    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    # Store key values
    s1_df.a1[ii] = value(a1)
    s1_df.a2[ii] = value(a2)
    s1_df.λ1[ii] = value(λ1)
    s1_df.λ2[ii] = value(λ2)

    println("a1 = ", value(a1), " a2 = ", value(a2),
        " λ1 = ", value(λ1), " λ2 = ", value(λ2))

end
## Plot
plt_s1 = plot(s1_df.s1, s1_df.a1, legend = false,
    xlabel = raw"$s_{1,t}$", ylabel = raw"$a_{1,t}$")
plot(s1_df.s1, s1_df.λ1, legend = false,
    xlabel = raw"$s_{1,t}$", ylabel = raw"$\lambda_{1,t}$")



"""
How does a1 vary with σ_ϵ1?
"""
## Set default and create df to fill
signals = [0.0, 0.0]
ϵ1_df = DataFrame(σ_ϵ1 = 0.01:0.01:3.0)
ϵ1_df[:,:σ_ϵ2] .= 1.0
ϵ1_df[:,:a1] .= 0.
ϵ1_df[:,:a2] .= 0.
ϵ1_df[:,:λ1] .= 0.
ϵ1_df[:,:λ2] .= 0.
## Loop over possible values of s1
for ii in 1:nrow(ϵ1_df)
    # new value and model
    σ_ϵ1 = ϵ1_df.σ_ϵ1[ii]
    par[:σ_ϵ1] = σ_ϵ1
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
        ((λ1- (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*signals[1]^2 +
            λ1^2*(1-a1)^2*par[:σ_η1] + par[:σ_ϵ1]*(par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) +
        ((λ2-(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*signals[2]^2 +
            λ2^2*(1-a2)^2*par[:σ_η2] + par[:σ_ϵ2]*(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) )
    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    # Store key values
    ϵ1_df.a1[ii] = value(a1)
    ϵ1_df.a2[ii] = value(a2)
    ϵ1_df.λ1[ii] = value(λ1)
    ϵ1_df.λ2[ii] = value(λ2)

    println("a1 = ", value(a1), " a2 = ", value(a2),
        " λ1 = ", value(λ1), " λ2 = ", value(λ2))

end
## Plot
plt_ϵ1 = plot(ϵ1_df.σ_ϵ1./ϵ1_df.σ_ϵ2, ϵ1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$\sigma^2_{\epsilon,1}$")
plot(ϵ1_df.σ_ϵ1./ϵ1_df.σ_ϵ2, ϵ1_df.λ1, legend = false, ylabel = raw"$\lambda_{1,t}$",
    xlabel = raw"$\sigma^2_{\epsilon,1}$")
## Reset
par[:σ_ϵ1] = 1.0





"""
How does a1 vary with σ_ν1?
"""
## Set default and create df to fill
signals = [0.0, 0.0]
ν1_df = DataFrame(σ_ν1 = 0.01:0.01:3.0)
ν1_df[:,:σ_ν2] .= 1.0
ν1_df[:,:a1] .= 0.
ν1_df[:,:a2] .= 0.
ν1_df[:,:λ1] .= 0.
ν1_df[:,:λ2] .= 0.
## Loop over possible values of s1
for ii in 1:nrow(ν1_df)
    # new value and model
    σ_ν1 = ν1_df.σ_ν1[ii]
    par[:σ_ν1] = σ_ν1
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
        ((λ1- (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*signals[1]^2 +
            λ1^2*(1-a1)^2*par[:σ_η1] + par[:σ_ϵ1]*(par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) +
        ((λ2-(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*signals[2]^2 +
            λ2^2*(1-a2)^2*par[:σ_η2] + par[:σ_ϵ2]*(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) )
    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    # Store key values
    ν1_df.a1[ii] = value(a1)
    ν1_df.a2[ii] = value(a2)
    ν1_df.λ1[ii] = value(λ1)
    ν1_df.λ2[ii] = value(λ2)

    println("a1 = ", value(a1), " a2 = ", value(a2),
        " λ1 = ", value(λ1), " λ2 = ", value(λ2))

end
## Plot
plt_ν1 = plot(ν1_df.σ_ν1./ν1_df.σ_ν2, ν1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$\sigma^2_{\nu,1}$")
plot(ν1_df.σ_ν1./ν1_df.σ_ν2, ν1_df.λ1, legend = false, ylabel = raw"$\lambda_{1,t}$",
    xlabel = raw"$\sigma^2_{\nu,1}$")
## Reset
par[:σ_ν1] = 1.0


"""
How does a1 vary with σ_η1?
"""
## Set default and create df to fill
signals = [0.0, 0.0]
η1_df = DataFrame(σ_η1 = 0.01:0.01:3.0)
η1_df[:,:σ_η2] .= 1.0
η1_df[:,:a1] .= 0.
η1_df[:,:a2] .= 0.
η1_df[:,:λ1] .= 0.
η1_df[:,:λ2] .= 0.
## Loop over possible values of s1
for ii in 1:nrow(η1_df)
    # new value and model
    σ_η1 = η1_df.σ_η1[ii]
    par[:σ_η1] = σ_η1
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
        ((λ1- (par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])))^2)*signals[1]^2 +
            λ1^2*(1-a1)^2*par[:σ_η1] + par[:σ_ϵ1]*(par[:σ_ϵ1]/(par[:σ_ϵ1] + par[:σ_ν1])) +
        ((λ2-(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])))^2)*signals[2]^2 +
            λ2^2*(1-a2)^2*par[:σ_η2] + par[:σ_ϵ2]*(par[:σ_ϵ2]/(par[:σ_ϵ2] + par[:σ_ν2])) )
    # Optimise
    @suppress_out begin
        optimize!(model)
    end
    # Store key values
    η1_df.a1[ii] = value(a1)
    η1_df.a2[ii] = value(a2)
    η1_df.λ1[ii] = value(λ1)
    η1_df.λ2[ii] = value(λ2)

    println("a1 = ", value(a1), " a2 = ", value(a2),
        " λ1 = ", value(λ1), " λ2 = ", value(λ2))

end
## Plot
plt_η1 = plot(η1_df.σ_η1./η1_df.σ_η2, η1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$\sigma^2_{\eta,1}$")
plot(η1_df.σ_η1./η1_df.σ_η2, η1_df.λ1, legend = false, ylabel = raw"$\lambda_{1,t}$",
    xlabel = raw"$\sigma^2_{\eta,1}$")
## Reset
par[:σ_η1] = 1.0


"""
Combine plots to save
"""
gr()
plot(layout = (2,2))
plot!(s1_df.s1, s1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$s_{t,1}$", subplot = 1)
plot!(ϵ1_df.σ_ϵ1, ϵ1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$\sigma^2_{\epsilon,1}$", subplot = 2)
plot!(ν1_df.σ_ν1, ν1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$\sigma^2_{\nu,1}$", subplot = 3)
plot!(η1_df.σ_η1, η1_df.a1, legend = false, ylabel = raw"$a_{1,t}$",
    xlabel = raw"$\sigma^2_{\eta,1}$", subplot = 4)

savefig("model_figs/attention_sensitivity.pdf")
