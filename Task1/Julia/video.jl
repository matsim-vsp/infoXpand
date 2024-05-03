using Pkg
Pkg.activate(".")
using Agents
using CairoMakie
using GLMakie

#### Author: S. Paltra @ TU Berlin

include("model.jl")
include("plotting.jl")

model_100agents = initialize(;    
                grid_dimensions = (40,40),
                total_agents = 100,
                susceptibility = 0.001)
#Generate a video
Agents.abmvideo("HundredAgents.mp4", model_100agents; 
                ac = person_color, am =:rect, as = 25,
                frames = 50, framerate = 5,
                title = "100 Agents enter the space")

model_200agents = initialize(;    
grid_dimensions = (40,40),
total_agents = 200,
susceptibility = 0.001)

#Generate a video
Agents.abmvideo("TwoHundredAgents.mp4", model_200agents, agent_step!; 
                ac = person_color, am =:rect, as = 25,
                frames = 100, framerate = 5,
                title = "200 Agents enter the space")


model_0agents = initialize(;    
                grid_dimensions = (40,40),
                total_agents = 1,
                susceptibility = 0.2)
                
#Generate a video
Agents.abmvideo("ZeroAgents.mp4", model_0agents, agent_step!; 
                                ac = person_color, am =:rect, as = 25,
                                frames = 50, framerate = 5,
                                title = "1 Agent enters the space")
                            
                
