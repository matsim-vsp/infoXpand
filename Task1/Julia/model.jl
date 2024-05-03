using Agents, Random

#### Author: S. Paltra @ TU Berlin

#This script builds the small -> The main building blocks are: 
# 1) The construction of the agents
# 2) The construction of the environment the agents "live in"
# 3) The construction of the agents' behavior or in other words: how the infection spreads

# 1) Construction of the agents
@agent Person ContinuousAgent{2} begin
health :: Int64 # Agents health status; 0 susceptible, 1 infected, 2 recovered (immune)
illness_duration :: Int64 # How long has the agent been sick, 0 if agent is susceptible or immune
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
    influx_probability = 0.00001,
    grid_dimensions =(20,20),
    total_agents = 400)

    # 2) Construction of the Envionment
    space = ContinuousSpace(grid_dimensions, periodic = true)

    # Model properties as defined in the function parameters
    properties = Dict(
        :susceptibility => susceptibility, 
        :model_illness_duration => model_illness_duration,
        :influx_probability => influx_probability
    )    
    model = ABM(Person, space; properties, agent_step! = agent_step!)

    # Agents are added to the environment
    for i in 1:total_agents
            p = Person(i,(i,1),(1,1),0,0)
            add_agent!(p, model)
    end

    sick_person = random_agent(model)
    sick_person.health = 1

    return model
end    

# 3) Construction of agents' behavior (and the systems' disease transmission)
function agent_step!(person, model)

    # # Influx from outside of model ##TODO: Is this really necessary?
    # if rand(model.rng) ≤ model.influx_probability
    #     p = random_agent(model)
    #     if p.health == 0
    #         p.health = 1
    #     end
    # end

    # Consider susceptible agents
    if susceptible(person) #For every susceptibile agent, we check if the agent has infectious neighbors
        # If the susceptible agent has at least one infectious neighbor, then the susceptibile agents may be infected with probability model.susceptible by every infectious neighbor
        for neighbor in nearby_agents(person, model, 1000) 
            if neighbor.health == 1
                if rand() < model.susceptibility
                    person.health = 1
                end    
            end    
        end   
    end 

    # Consider infected agents
    if person.health == 1
        person.illness_duration += 1
        if person.illness_duration ≥ model.model_illness_duration
            person.health = 2 #immune
            person.illness_duration = 0
        end
    end

end    
