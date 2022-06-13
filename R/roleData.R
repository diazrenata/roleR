#' @title RoLE model data
#' 
#' @description An S4 class to hold data from one timestep of one model run
#' 
#' @slot localComm an object of class \code{localComm}
#' @slot metaComm an object of class \code{metaComm}
#' @slot phylo an object of class \code{rolePhylo}
#' 
#' @export

setClass('roleData',
         slots = c(localComm = 'localComm',
                   metaComm = 'metaComm',
                   phylo = 'rolePhylo'))

# constructor
#' @export

roleData <- function(localComm, metaComm, phylo) {
  return(new('roleData',
             localComm = localComm,
             metaComm = metaComm,
             phylo = phylo))
}

