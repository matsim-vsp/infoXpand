include("model.jl")

#functions for plotting
function person_color(p)
    if p.health == 0 
        return :blue 
    elseif p.health == 1
        return :red
    else
        return :green #immune
    end
end

#alternative : person_color(p) = p.health == 0 ? :blue : :red

function person_shape(p::Person)
    if aware(p)
        return :circle
    end
        
    return :rect

end

#step!(model, agent_step!, 3)

model = initialize()

#plot a model
fig, ax, abmobs = abmplot(model; ac = person_color, am =:rect, as = 25)
fig