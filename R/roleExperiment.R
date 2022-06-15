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
    # loop over all params, running roleModel on each set, keep only the data
    outMod <- lapply(allParams, function(p) {
        o <- roleModel(p)
        o@modelSteps
    })
    
    
}


# set coercion method from `roleModel` to `roleExperiment`
setAs(from = 'roleModel', to = 'roleExperiment',
      def = function(from) {
          j <- c(0, 
                 which(1:from@params@niter %% from@params@niterTimestep == 0)) +
              1
          
          # extract the param vals for those j indeces
          
          # put param vals in a data.frame
          # also put an index column in that data.frame for model run
          # (e.g. run 1 will have multiple roleData entries)
          
          # list out the model runs 
          
          # list out params (i.e. `list(allParams = params)`)
          
      }
)


# make a method to rbind `roleExperiments` and with that we've basically got
# what we need to write the roleExperiment constructor function
