#' @title Run a roleModel or roleExperiment object 
#' @description Return a RoLE object run to completion using the params contained within it
#' @param x the `roleModel` or `roleExperiment` object to run
#' @param print whether to print step information as the model runs
#' @return a run `roleModel` or `roleExperiment`
#' 
#' @examples 
#' Create and run a model
#' model <- roleModel(roleParams())
#' model <- runRoLE(model)
#' 
#' Create and run an experiment
#' p1 <- roleParams(speciation_local=0.2)
#' p2 <- roleParams(speciation_local=0.3)
#' p3 <- roleParams(speciation_local=0.4)
#' exp <- roleExperiment(list(p1,p2,p3))
#' exp <- runRoLE(exp)
#' 
#' @rdname runRoLE
#' @export

setGeneric('runRoLE', 
           def = function(x, cores=1, print = F) standardGeneric('runRoLE'), 
           signature = 'x')

setMethod('runRoLE', 
          signature = 'roleModel', 
          definition = function(x, print = F) {
              # library(rlang)
              m <- rlang::duplicate(x)
 
              for(i in 2:length(x@modelSteps))
              {
                  if(!is.null(x@modelSteps[[i]]))
                  {
                      stop("Models can only be run once and this model has already been run")
                  }
              }
              #if(length(x@modelSteps) > 1)
              # m <- model
              # init the first data step of the model using params 
              #model <- initModel(model)
              # start_data <- m@modelSteps[[1]]
              # iterate the model using its params 
              
              # returns the new modelSteps (a list of roleData)
              m@modelSteps <- iterModelCpp(slot(m@modelSteps[[1]],"localComm"), 
                                           slot(m@modelSteps[[1]],"metaComm"),
                                           slot(m@modelSteps[[1]],"phylo"),
                                           m@params,print)
              for(d in 1:length(m@modelSteps))
              {
                  m@modelSteps[[d]]@localComm@indSpecies <- m@modelSteps[[d]]@localComm@indSpecies + 1
                  m@modelSteps[[d]]@phylo@e <- m@modelSteps[[d]]@phylo@e + 1
              }
              return(m)
          }
)

setMethod('runRoLE', 
          signature = 'roleExperiment', 
          definition = function(x, cores = 1, print = F) {
              experiment <- x
              library(roleR)
              if(cores == 1){
                  experiment@modelRuns <- lapply(experiment@modelRuns, iterModel)
              }
              else{
                  cl <- makeCluster(cores,type="SOCK")
                  experiment@modelRuns <- clusterApply(cl, experiment@modelRuns, iterModel)
                  stopCluster(cl)
              }
              return(experiment)
          }
)
#runs <- as.list(experiment@modelRuns)
#runs <- experiment@modelRuns
#print=F
iterModel <- function(model,print=F) {
    
    #ibrary(rlang)
    m <- rlang::duplicate(model)
    # m <- model
    # init the first data step of the model using params 
    #model <- initModel(model)
    # start_data <- m@modelSteps[[1]]
    # iterate the model using its params 
    
    # returns the new modelSteps (a list of roleData)
    m@modelSteps <- iterModelCpp(slot(m@modelSteps[[1]],"localComm"), 
                                 slot(m@modelSteps[[1]],"metaComm"),
                                 slot(m@modelSteps[[1]],"phylo"),
                                 m@params,print)
    for(d in 1:length(m@modelSteps))
    {
        m@modelSteps[[d]]@localComm@indSpecies <- m@modelSteps[[d]]@localComm@indSpecies + 1
        m@modelSteps[[d]]@phylo@e <- m@modelSteps[[d]]@phylo@e + 1
    }
    return(m)
}

iterExperiment <- function(experiment, cores=1){
    if(cores == 1){
        experiment@modelRuns <- lapply(experiment@modelRuns, iterModel)
    }
    else{
        cl <- makeCluster(cores,type="SOCK")
        experiment@modelRuns <- clusterApply(cl, experiment@modelRuns, iterModel)
        stopCluster(cl)
    }
    return(experiment)
}

# take a roleModel and init it's first data before giving to iterSim 
# initModel <- function(model) {
#     
#     pars <- model@params
#         
#     # simulate phylogeny
#     phy <- TreeSim::sim.bd.taxa(pars@species_meta[1], numbsim = 1,
#                                 lambda = pars@speciation_meta[1],
#                                 mu = pars@extinction_meta[1], complete = FALSE)[[1]]
#     
#     # create metaComm object using relative abundances and traits generated across the phylo 
#     meta <- metaComm(cbind(lseriesFromSN(pars@species_meta[1],pars@individuals_meta[1]),
#                            ape::rTraitCont(phy, sigma = pars@trait_sigma[1])))
#     
#     # initialize vector of 0 species abundances
#     abundance_l_sp <- rep(0, params@species_meta[1])
#     
#     # oceanic island model assigns all abundance to one species
#     if(type == "oceanic_island"){
#         # index of the species that will initially have all abundance
#         i <- sample(pars@species_meta[1], 1, prob = meta@spAbundTrt[,1])
#         # passing all abundance to that species
#         abundance_l_sp[i] <- pars@individuals_local[1]
#     }
#     # bridge island model assigns abundances to all species proportional to species abundance
#     else if(type == "bridge_island"){
#         # vector of species
#         abundance_l_sp <- sample(pars@species_meta[1], prob = meta@spAbundTrt[,1])
#     }
#     
#     # reshape to individual
#     # init local species traits
#     traits_l_sp <- traits_m
#     
#     # create localCommCpp object
#     local <- new(localCommCpp, abundance_l_sp, traits_l_sp, pi_l, naug)
#     
#     # convert ape phylo to rolePhylo
#     phy <- apeToRolePhylo(phy)
#     
#     # convert rolePhylo to rolePhyloCpp
#     phy <- rolePhyloToCpp(phy)
#     
#     # create roleModelCpp object of local comm, meta comm, phylogeny, and args
#     out <- new(roleModelCpp,local,meta,phy,parlist)
#     
#     return(model)
# }