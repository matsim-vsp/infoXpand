using Agents, Random

#BUILDING THE ABM
#3 Things

#Agent
@agent  Person ContinuousAgent{2} begin
health :: Int64 #0 susceptible, 1 infected, 2 recovered (immune)
illness_duration :: Int64 #how long has the agent been ill, 0 if agent
#UAU-SIS Model 
end

# Predicates of our agent
susceptible(p::Person) = p.health == 0
infected(p::Person) = p.health == 1
recovered(p::Person) = p.health == 2 
notattending(p::Person) = p.health == 3


function initialize(;
    susceptibility = 0.3, 
    model_illness_duration = 10, 
    immunity_duration = 300, 
    influx_probability = 0.00001,
    grid_dimensions =(20,20),
    total_agents = 400,
    seed = 1234)

    #Environment
    #space = GridSpaceSingle(grid_dimensions; periodic = false)
    space = ContinuousSpace(grid_dimensions, periodic = true)

    rng = Xoshiro(seed)

    #model properties as defined in the function parameters
    properties = Dict(
        :susceptibility => susceptibility, 
        :model_illness_duration => model_illness_duration,
        :immunity_duration => immunity_duration,
        :influx_probability => influx_probability
    )    
    model = ABM(Person, space; properties, rng = rng)

    for i in 1:total_agents
            p = Person(i,(i,1),(1,1),0,0)
            #vel = (1,1)
            add_agent!(p, model)
    end

    sick_person = random_agent(model)
    sick_person.health = 1

    return model
end    

#behavior (SIRS Model) = rules = steps
function agent_step!(person, model)

    #influx from outside of model 
    if rand(model.rng) ≤ model.influx_probability
        p = random_agent(model)
        if p.health == 0
            p.health = 1
        end
    end

    if susceptible(person) 
        for neighbor in nearby_agents(person, model)
            if neighbor.health == 1
                if rand(model.rng) < model.susceptibility
                    person.health = 1
                end    
            end    
        end   
    end 
    #person is infected 
    if person.health == 1
        person.illness_duration += 1
        if person.illness_duration ≥ model.model_illness_duration
            person.health = 2 #immune
            person.illness_duration = 0
        end
    end
    #person is immune/recovered
    if person.health == 2
        person.illness_duration += 1
        if person.illness_duration ≥ model.immunity_duration
            person.health = 0 #susceptibility
            person.illness_duration = 0
        end
    end
end    
