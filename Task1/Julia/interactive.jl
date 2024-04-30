using Pkg
Pkg.activate(".")
using Agents
using CairoMakie
using GLMakie

#### Author: S. Paltra @ TU Berlin

include("model.jl")
include("plotting.jl")

model = initialize(;
    grid_dimensions = (40,40),
    total_agents = 200
    )

adata=[(susceptible, count), (infected, count), 
        (recovered, count)]

params = Dict(
    :susceptibility => 0.05:0.01:0.2,
    :model_illness_duration => 5:15,
)

#Only visualize smaller models
figs, abmobs = abmexploration(model; agent_step!, ac=person_color, am =:rect, as = 15, adata, params)
figs