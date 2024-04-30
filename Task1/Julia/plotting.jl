include("model.jl")

#### Author: S. Paltra @ TU Berlin

#functions for plotting
function person_color(p)
    if p.health == 0 #susceptible
        return :blue 
    elseif p.health == 1 # infected
        return :red
    else
        return :green #immune
    end
end

#step!(model, agent_step!, 3)

model = initialize()

#plot a model
fig, ax, abmobs = abmplot(model; ac = person_color, am =:rect, as = 25)
fig