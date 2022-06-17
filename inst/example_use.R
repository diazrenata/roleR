library(roleR)

# make one set of params
params1 <- roleParams(individuals_local = 100, 
                      individuals_meta = 1000,
                      species_meta = 500, speciation_local = 0.1,
                      speciation_meta = 1, extinction_meta = 0.8,
                      trait_sigma = 1, env_sigma = 1,
                      comp_sigma = 1, dispersal_prob = 0.1, mutation_rate = 0.01,
                      equilib_escape = 1, num_basepairs = 250,
                      init_type = 'oceanic_island', niter = 100,
                      niterTimestep = 50)

# make a set of params with some params missing
params0 <- roleParams(individuals_local = 100, individuals_meta = 1000,
                      species_meta = 500, speciation_local = 0.1,
                      speciation_meta = 1, extinction_meta = 0.8,
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
length(oneExp@modelRuns)

# use `[` to select from an experiment
foo <- oneExp[oneExp@experimentMeta$iterations <= 50, ]
foo@experimentMeta
length(foo@modelRuns)
length(foo@allParams)

boo <- oneExp[oneExp@experimentMeta$mod_id == 1, ]
boo@experimentMeta
length(boo@allParams)



# get final state for one model run
foo <- getFinalState(oneMod)
length(foo@modelSteps)

# get final state from experiment
foo <- getFinalState(oneExp, modID = 1:2)
foo@experimentMeta

foo <- getFinalState(oneExp)
foo@experimentMeta


# get a single sum stat for one roleData object
oneDat <- getFinalState(oneMod)
oneDat <- oneDat@modelSteps[[1]]
rawAbundance(oneDat)
rawTraits(oneDat)
richness(oneDat)


# use `getSumStats` to get multiple sum stats at once for one roleData object

getSumStats(oneDat, 
            list(abund = rawAbundance, rich = richness))

getSumStats(oneDat, 
            list(rich = richness, abund = rawAbundance,
                 hillAbund = hillStats))

# get sum stats across runs for one model

getSumStats(oneMod, list(abund = rawAbundance, rich = richness))


# get sum stats across runs for one experiment
getSumStats(oneExp, list(abund = rawAbundance, rich = richness))




