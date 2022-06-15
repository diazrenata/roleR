#' @title Get summary statistics for RoLE objects
#' @description Applies different summary stats functions to `roleExperiment`,
#'     `roleModel`, or `roleData`
#' @param x the object to calculate sum stats across
#' @param fun the function(s) calculating the sum stats; can be a single 
#'     function or a list of functions
#' @param ... additional named parameters passed to `fun`
#' 
#' @details users can define their own functions, so long as they work on any
#'     object of class `roleData`
#' 
#' @export

setGeneric('getSumStats', 
           def = function(x, funs, ...) standardGeneric('getSumStats'), 
           signature = 'x')


# method for roleData
setMethod('getSumStats', 
          signature = 'roleData', 
          definition = function(x, funs, ...) {
              print('foo')
          }
)


# method for roleModel
setMethod('getSumStats', 
          signature = 'roleModel', 
          definition = function(x, funs, ...) {
              print('foo to roleModel')
          }
)


# method for roleExperiment
setMethod('getSumStats', 
          signature = 'roleExperiment', 
          definition = function(x, funs, ...) {
              print('foo to roleExperiment')
          }
)



# helper function to make a new synthetic function from a list of functions 
.funApply <- function(funs) {
    newFun <- function(x) {
        lapply(funs, function(f) f(x))
    }
}


