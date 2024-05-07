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
    total_agents = 400
    )

adata=[(susceptible, count), (infected, count), 
        (recovered, count)]

params = Dict(
    :susceptibility => 0.0001:0.0001:0.0005,
    :model_illness_duration => 800:1000,
)

#Only visualize smaller models
figs, abmobs = abmexploration(model; ac=person_color, am =:rect, as = 15, adata, params)
figs
