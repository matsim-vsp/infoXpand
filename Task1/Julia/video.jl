using Pkg
Pkg.activate(".")
using Agents
using CairoMakie
using GLMakie

include("model.jl")
include("plotting.jl")

model = initialize(;susceptibility = 0.1, model_illness_duration = 5)

#Generate a video Video
Agents.abmvideo("ourmodel.mp4", model, agent_step!; 
                ac = person_color, am =:rect, as = 25,
                frames = 200, framerate = 10,
                title = "Our first model")