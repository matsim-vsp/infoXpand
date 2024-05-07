using Agents, Random, Graphs, DataFrames, Statistics, CSV, Dates

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
exposed(p::Person) = p.health == 3 

function initialize(;
    susceptibility = 0.3, 
    model_illness_duration = 400, 
    grid_dimensions =(20,20),
    total_agents = 400,
    seed = 1234)

    # create random number generator
    rng = Random.Xoshiro(seed)

    # 2) Construction of the Envionment
    space = ContinuousSpace(grid_dimensions, periodic = true)

    # Model properties as defined in the function parameters
    properties = Dict(
        :susceptibility => susceptibility, 
        :model_illness_duration => model_illness_duration,
        :rng => rng,

        # current count for each disease state
        :cnt_susceptible => 0,
        :cnt_exposed => 0,
        :cnt_infectious => 0,
        :cnt_recovered => 0,

        #history of count for each iteration
        :hist_susceptible => [],
        :hist_exposed => [],
        :hist_infectious => [],
        :hist_recovered => []
    )    
    model = ABM(Person, space; properties, agent_step! = agent_step!)

    # Agents are added to the environment
    for i in 1:total_agents
            p = Person(i,(i,1),(1,1),0,0)
            add_agent!(p, model)
    end

        sick_person = random_agent(model)
        sick_person.health = 1

    # set initial values for cnt_DISEASE_STATE
    for agent in allagents(model)
        if agent.health == 0
            model.cnt_susceptible += 1
        elseif agent.health == 1
            model.cnt_infectious += 1
        elseif agent.health == 2
            model.cnt_recovered += 1
        elseif agent.health == 3
            model.cnt_recovered += 1
        else
            throw(DomainError)
        end
    end
    
        # push cnts to first entry in history of each disease states
        push_state_count_to_history!(model)

    return model
end    

# 3) Construction of agents' behavior (and the systems' disease transmission)
function agent_step!(person, model)

    # Consider susceptible agents
    if susceptible(person) #For every susceptibile agent, we check if the agent has infectious neighbors
        # If the susceptible agent has at least one infectious neighbor, then the susceptibile agents may be infected with probability model.susceptible by every infectious neighbor
        for neighbor in nearby_agents(person, model, 1000) 
            if neighbor.health == 1
                if rand(model.rng) < model.susceptibility
                    person.health = 3
                    model.cnt_susceptible -= 1
                    model.cnt_exposed += 1
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
            model.cnt_infectious -= 1
            model.cnt_recovered += 1
        end
    end

end    

function agent_step_nothing_happens!(person, model)

end

#The author wants to thank J. Rehmann as the following function was heavily influenced by his work on https://github.com/matsim-vsp/epi-net-sim

function run_model(params)
    @time begin

        # sets output directory. If running on cluster, output folder is specified by start_multiple_sh. If run locally, a new folder will be created w/ current datetime 
        if ismissing(params[:output_folder])
            output_path = "data/" * replace(first(string(now()), 19), ":" => "")
            mkpath(output_path)

        else
            output_path = params[:output_folder]
        end

        # writes _info.csv with all relevant input parameters
        # TODO: when running on cluster, _info.csv gets overwritten by each seperate job.
        CSV.write(output_path * "/_info.csv", params)

        # for each network structure
        for alpha in params[:alpha]
            # for each base susceptiblity
            for susceptibility in params[:susceptibilities]
                # for each local_global_scenario 
                        # empty dictionary, which will be filled with respective stat (e.g. susceptible count) for each model run (all iterations, rows of matrix) for each seed (columns of matrix)
                        results = Dict(
                            "susceptible" => [],
                            "exposed" => [],
                            "infectious" => [],
                            "recovered" => [],
                        )

            

                        println("#######################")
                        println("Alpha: $(alpha) --  Susceptibility: $(susceptibility)")
                        println("#######################")

                        # loop through all seeds
                        for seed in 1:params[:seeds]

                            no_agents = alpha*100

                            # Create model
                            model = initialize(;
                            susceptibility = susceptibility, 
                            model_illness_duration = 10, 
                            grid_dimensions =(20,20),
                            total_agents = no_agents,
                            seed
                            )


                            # Step through all iterations. In each iteration:
                            #   1) agent_step function is applied to each agent
                            #   2) model_step function occurs at end of iteration
                            if rand() < alpha
                                step!(model, agent_step!, model_step!, params[:iterations])
                            else 
                                step!(model, agent_step_nothing_happens!, model_step!, params[:iterations])
                            end


                            # model stats for particular seed are added as column to each results matrix. 
                            push!(results["susceptible"], model.hist_susceptible)
                            push!(results["exposed"], model.hist_exposed)
                            push!(results["infectious"], model.hist_infectious)
                            push!(results["recovered"], model.hist_recovered)
                        end


                        # prints each stat matrix in results dictionary to csv. 
                        for (result_type, result_matrix_all_seeds) in results
                            df = DataFrame()
                            seed_counter = 1
                            for results_for_single_seed in result_matrix_all_seeds
                                vector_title = "seed$(seed_counter)"
                                # if(typeof(results_for_single_seed)==Float64)
                                #     df[!,vector_title] = [results_for_single_seed]
                                # else
                                df[!, vector_title] = results_for_single_seed
                                # end
                                seed_counter += 1
                            end

                            output_file_name = "/$(alpha)-$(susceptibility)-$(result_type).csv"
                            CSV.write(output_path * output_file_name, df)
                        end 
            end
        end
    end
end

# model state occurs at end of each iteration, after agent_step is applied to all agents
function model_step!(model)

    # push disease state counts for current (ending) iteration to respective history. 
    push_state_count_to_history!(model)

end

# updates disease state histories with disease state counts for current iteration
function push_state_count_to_history!(model)
    push!(model.hist_susceptible, model.cnt_susceptible)
    push!(model.hist_exposed, model.cnt_exposed)
    push!(model.hist_infectious, model.cnt_infectious)
    push!(model.hist_recovered, model.cnt_recovered)
end