#
# bounded rationality opinion diffusion
# alternativ backfire
using Random
using CSV
using DataFrames
struct Config
    agent_count::Int64
    epsilon::Float64
    max_steps::Int64
    backfire::Bool
    seed::Int64
end
mutable struct Agent
    opinion::Float64 # 0 - 1
end
function setup(config, rng)
    # erstelle agent_count agenten als Array mit Zufallsmeinung
    [ Agent(rand(rng)) for i in 1:config.agent_count ]
end
function go(config, rng, agent_list)
    for one_agent in shuffle(rng, agent_list)
        idx = rand(rng, 1:config.agent_count)
        other_agent = agent_list[idx]
        if abs(other_agent.opinion - one_agent.opinion) < config.epsilon
            one_agent.opinion = (other_agent.opinion + one_agent.opinion) / 2
        else
            if config.backfire
                if one_agent.opinion < other_agent.opinion
                    one_agent.opinion = one_agent.opinion -  abs(other_agent.opinion - one_agent.opinion)/2
                else
                    one_agent.opinion = one_agent.opinion + abs(other_agent.opinion - one_agent.opinion)/2
                end
                if one_agent.opinion < 0
                    one_agent.opinion = 0
                end
                if one_agent.opinion > 1
                    one_agent.opinion = 1
                end

            end
        end
    end
    agent_list
end
function simulation(config::Config, rng::MersenneTwister)
    agent_list = setup(config, rng)
    result = DataFrame(step = zeros(Int64, config.agent_count),
                        agent_number = 1:config.agent_count,
                        backfire = config.backfire,
                        opinion = [agent_list[i].opinion for i in 1:config.agent_count]
                        )
    for i in 1:config.max_steps
        # call go and "store/process" the interesting metric
        agent_list = go(config, rng, agent_list)
        step_result = DataFrame(step = fill(i, config.agent_count),
                            agent_number = 1:config.agent_count,
                            backfire = config.backfire,
                            opinion = [ agent_list[i].opinion for i in 1:config.agent_count ]
                            )
        append!(result, step_result)
    end
    result
end
###### eigentliches Programm
function runbatchconfig(all_configs)
    # run config objects
    result = DataFrame()
    for cfg in all_configs
        rng = Random.MersenneTwister(cfg.seed)
        step_result = simulation(cfg, rng)
        nr = nrow(step_result)
        step_result.seed = fill(cfg.seed, nr)
        step_result.agent_count = fill(cfg.agent_count, nr)
        step_result.epsilon = fill(cfg.epsilon, nr)
        append!(result, step_result)
    end
    # merge results into single data frame
    result
end
function startandsave(configs, filename)
    @time data = runbatchconfig(configs)
    CSV.write(filename, data)
end
function generateBatchConfig(agent_counts, epsilons, max_steps, replications)
    bfs = [true, false]
    all_configs = [ Config(ac, eps, ms, bf, sd)
                    for ac in agent_counts
                    for eps in epsilons
                    for ms in max_steps
                    for bf in bfs
                    for sd in replications
                    ]
    all_configs
end
## Hauptprogramm
function main()
    # create config objects
    agent_counts = 100:100:500
    epsilons = 0.1:0.1:1
    max_steps = [100]
    replications = 1:50
    my_config = generateBatchConfig(agent_counts,
                                      epsilons,
                                      max_steps,
                                      replications)
    startandsave(my_config, "results.csv")
end
@time main()
