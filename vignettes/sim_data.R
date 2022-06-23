## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig_flow, out.width='600px'----------------------------------------------
knitr::include_graphics('fig_roleR_sim_flow.jpeg')

## ----sim_setup----------------------------------------------------------------
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


# a `roleParams` object can also be made with some params missing
params0 <- roleParams(individuals_local = 100, individuals_meta = 1000,
                      species_meta = 500, speciation_local = 0.1,
                      speciation_meta = 1, extinction_meta = 0.8,
                      init_type = 'oceanic_island', niter = 100,
                      niterTimestep = 50)

# we can use standard S4 methods to make another full parameter set starting 
# using `params1` as a starting place
params2 <- params1
slot(params2, 'init_type') <- 'bridge_island'

## ----oneMod-------------------------------------------------------------------
# make one model run
oneMod <- roleModel(params1)

## ----oneExp-------------------------------------------------------------------
# make an experiment with multiple models
oneExp <- roleExperiment(list(params1, params2))

## ----oneExp_meta--------------------------------------------------------------
slot(oneExp, 'experimentMeta')

## ----coerceMod----------------------------------------------------------------
expFrmMod <- as(oneMod, 'roleExperiment')

# see the auto-populated metadata
slot(expFrmMod, 'experimentMeta')

## ----subset-------------------------------------------------------------------
# get only those model runs with fewer than 50 iterations
x <- oneExp[oneExp$iterations <= 50, ]

# view the metadata
slot(x, 'experimentMeta')

## ----finalHard----------------------------------------------------------------
y <- oneExp[oneExp$iterations == max(oneExp$iterations), ]

# view the metadata
slot(y, 'experimentMeta')

## ----finalExp-----------------------------------------------------------------
finalExp <- getFinalState(oneExp)

# view the metadata
slot(finalExp, 'experimentMeta')

## ----oneFinalExp--------------------------------------------------------------
oneFinalExp <- getFinalState(oneExp, 1)

slot(oneFinalExp, 'experimentMeta')

## ----finalMod-----------------------------------------------------------------
finalMod <- getFinalState(oneMod)
class(finalMod)

## ----dataFrmExp---------------------------------------------------------------
# get data from one timestep saved in a `roleExperiment` object
datFrmExp <- slot(oneFinalExp, 'modelRuns')[[1]]

## ----dataFrmMod---------------------------------------------------------------
# get data from one timestep saved in a `roleExperiment` object
datFrmMod <- slot(finalMod, 'modelSteps')[[1]]

## ----sumStatTour--------------------------------------------------------------
# summary stat functions with `raw` in the name return the raw data (as a list
# containing the raw output, more on that later)

rawAbundance(datFrmMod)

rawTraits(datFrmMod)

rawGenDiv(datFrmMod)

rawSeqs(datFrmMod)

rawApePhylo(datFrmMod)

# other stat functions with derived metrics return single vectors 
hillAbund(datFrmMod, q = 1:5) # Hill numbers with exponents 1:5

richness(datFrmMod)

## ----apply_roleExp------------------------------------------------------------
expSumStat <- getSumStats(oneExp, 
                          funs = list(abund = rawAbundance, 
                                      hillAbund = hillAbund, 
                                      rich = richness), 
                          moreArgs = list(hillAbund = list(q = 1:3)))


expSumStat

## ----fancySubset--------------------------------------------------------------
fancy <- oneExp[expSumStat$rich > 50, ]

slot(fancy, 'experimentMeta')

## ----custom-------------------------------------------------------------------
# function to calculate trait mean 
#' @param x is a `roleData` object

meanTrt <- function(x) {
    trt <- rawTraits(x)[[1]]
    
    return(mean(trt))
}

# see the output from running this function through `getSumStats`
getSumStats(oneExp, funs = list(mTrt = meanTrt))

