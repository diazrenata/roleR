library(roleR)

# make one set of params
params1 <- roleParams(individuals_local = 100, individuals_meta = 1000,
                      species_meta = 500, speciation_local = 0.1,
                      speciation_meta = 1, extinction_meta = 0.8,
                      trait_sigma = 1, env_sigma = 1,
                      comp_sigma = 1, dispersal_prob = 0.1, mutation_rate = 0.01,
                      equilib_escape = 1, num_basepairs = 250,
                      init_type = 'oceanic_island', niter = 100,
                      niterTimestep = 50)

# make another slightly different set
params2 <- params1
slot(params2, 'init_type') <- 'bridge_island'

# make one model run
oneMod <- roleModel(params1)

# make an experiment with multiple models
oneExp <- roleExperiment(list(params1, params2))

# check out the metadata
oneExp@experimentMeta

# get the current state for one model run

# get the timeseries of all states for one model run


