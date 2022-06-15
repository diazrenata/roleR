#' @title RoLE experiment
#' 
#' @description An S4 class to represent a complete self-enclosed experiment (a 
#'     model run or set of runs with associated meta data). Contains one or more 
#'     `roleModel` runs and the `roleParams` used to generate each of the runs
#' 
#' @slot experimentMeta data.frame of model meta data
#' @slot modelRuns list of `roleData` objects containing outputs
#' @slot allParams list of `roleParams` objects containing input params for 
#'     each run
#' 
#' @rdname roleExperiment
#' @export

setClass('roleExperiment',
         slots = c(experimentMeta = 'data.frame',
                   modelRuns = 'list', 
                   allParams = 'list'))


# constructor for roleExperiment
#' @rdname roleExperiment
#' @export

roleExperiment <- function(allParams) {
    # loop over all params, running roleModel on each set, coercing to a
    # roleExperiment
    outEx <- lapply(allParams, function(ps) {
        mod <- roleModel(ps)
        return(as(mod, 'roleExperiment'))
    })
    
    return(do.call(rbind, outEx))
}


# set coercion method from `roleModel` to `roleExperiment`
setAs(from = 'roleModel', to = 'roleExperiment',
      def = function(from) {
          niter <- from@params@niter
          niterTimestep <- from@params@niterTimestep
          
          # index of when data were saved
          j <- c(0, which(1:niter %% niterTimestep == 0)) + 1
          
          # extract the param vals for those j indeces
          pnames <- slotNames(from@params)
          pnames <- pnames[!grepl('niter', pnames)]
          
          pout <- lapply(pnames, function(p) {
              p <- slot(from@params, p)
              
              if(length(p) > 1) {
                  p <- c(p[1], p)
                  return(p[j])
              } else {
                  return(rep(p, length(j)))
              }
          })
          
          # put param vals in a data.frame
          names(pout) <- pnames
          pout <- do.call(data.frame, pout)
          
          # add time information
          pout$iterations <- j - 1
          pout$generations <- pout$iterations / (pout$individuals_local / 2)
          
          # put an index column in that data.frame for model run
          pout <- cbind(mod_id = 1, pout)
          
          return(new('roleExperiment', 
                     experimentMeta = pout,
                     modelRuns = from@modelSteps, 
                     allParams = list(from@params)))
      }
)


# rbind method for `roleExperiment` class
rbind.roleExperiment <- function(...) {
    allEx <- list(...)
    
    out <- allEx[[1]]
    
    if(length(allEx) > 1) {
        for(i in 2:length(allEx)) {
            # keep track of growing mod_id max index
            j <- max(out@experimentMeta$mod_id)
            
            # extract next experiment
            thisEx <- allEx[[i]]
            
            # augment mod_id
            thisEx@experimentMeta$mod_id <-
                thisEx@experimentMeta$mod_id + j
            
            # combine with `out`
            out@experimentMeta <- rbind(out@experimentMeta,
                                        thisEx@experimentMeta)
            out@modelRuns <- c(out@modelRuns,
                               thisEx@modelRuns)
            out@allParams <- c(out@allParams,
                               thisEx@allParams)
        }
    }
    
    return(out)
}





# testing
# source('R/comm.R')
# source('R/rolePhylo.R')
# source('R/roleData.R')
# source('R/roleParams.R')
# source('R/roleModel.R')
# 
# params <- roleParams(individuals_local = 100, individuals_meta = 1000,
#                      species_meta = 500, speciation_local = 0.1,
#                      speciation_meta = 1, extinction_meta = 0.8,
#                      trait_sigma = 1, env_sigma = 1,
#                      comp_sigma = 1, dispersal_prob = 0.1, mutation_rate = 0.01,
#                      equilib_escape = 1, num_basepairs = 250,
#                      init_type = 'oceanic_island', niter = 100,
#                      niterTimestep = 10)
# 
# x <- roleModel(params)
# y <- as(x, 'roleExperiment')
# 
# boo <- do.call(rbind, list(y, y, y))
# boo@experimentMeta
