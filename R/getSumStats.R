#' @title Get summary statistics for RoLE objects
#' @description Applies different summary stats functions to `roleExperiment`,
#'     `roleModel`, or `roleData`
#' @param x the object to calculate sum stats across
#' @param funs a named list of function to calculate the sum stats; can be a 
#'     named list with a single function or many functions, but must be a 
#'     named list of functions
#' @param ... additional parameters, ignored
#' 
#' @details users can define their own functions, so long as they work on any
#'     object of class `roleData`
#' 
#' @rdname getSumStats
#' @export

setGeneric('getSumStats', 
           def = function(x, funs, ...) standardGeneric('getSumStats'), 
           signature = 'x')


# method for roleData
#' @rdname getSumStats
#' @export

setMethod('getSumStats', 
          signature = 'roleData', 
          definition = function(x, funs) {
              bigFun <- .funApply(funs)
              
              return(bigFun(x))
          }
)


# method for roleModel
#' @rdname getSumStats
#' @export

setMethod('getSumStats', 
          signature = 'roleModel', 
          definition = function(x, funs, ...) {
              print('foo to roleModel')
          }
)


# method for roleExperiment
#' @rdname getSumStats
#' @export

setMethod('getSumStats', 
          signature = 'roleExperiment', 
          definition = function(x, funs, ...) {
              print('foo to roleExperiment')
          }
)



# helper function to make a new synthetic function from a list of functions 
.funApply <- function(funs) {
    newFun <- function(x) {
        o <- lapply(1:length(funs), function(i) {
            # get function from list
            f <- funs[[i]]
            fname <- names(funs[i])
            
            # function value
            val <- f(x)
            
            # name the values
            if(length(val) > 1) {
                ename <- names(val)
                
                if(is.null(ename)) {
                    ename <- 1:length(val)
                }
                
                ename <- paste(fname, 1:length(val), sep = '_')
            } else {
                ename <- fname
            }
            
            names(val) <- ename
            
            # return as a data.frame
            if('list' %in% class(val)) {
                # if val is a list, must protect it to make a list column
                df <- data.frame(I(val))
                names(df) <- names(val)
                return(df)
            } else {
                return(as.data.frame(as.list(val)))
            }
        })
        
        # combine data.frames and return
        o <- do.call(cbind, o)
        rownames(o) <- NULL
        
        return(o)
    }
    
    # the actual final return of `.funApply` is a function that runs the 
    # above code
    return(newFun)
}
