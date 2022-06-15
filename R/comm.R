#' @title Local community
#' 
#' @description An S4 class to specify the state of a local community
#'
#' @slot indSppTrt numeric matrix with rows for individuals; column 1 is species
#'     ID (matches row number in `metaComm@sppData`); column 2 is trait value; 
#'     column 3 is sequence (maybe? maybe shouldn't have it)
#' @slot indSeqs character matrix with rows for individuals; column 1 is 
#'     sequences for each individual
#' @slot sppGenDiv a numeric matrix with rows for species; column 1 is species
#'     level genetic diversity
#' 
#' @rdname localComm
#' @export

setClass('localComm',
         slots = c(indSppTrt = 'matrix',
                   indSeqs = 'matrix',
                   sppGenDiv = 'matrix'))


# constructor 
#' @rdname localComm
#' @export

localComm <- function(indSppTrt, indSeqs, sppGenDiv) {
  return(new('localComm',
             indSppTrt = indSppTrt,
             indSeqs = indSeqs,
             sppGenDiv = sppGenDiv))
}




#' @title Meta community
#' 
#' @description An S4 class to specify the state of a meta community
#'
#' @slot sppAbundTrt numeric matrix with rows for species (row number is 
#'     species ID); column 1 is relative abundance; column 2 is trait mean
#' 
#' @rdname metaComm
#' @export

setClass('metaComm',
         slots = c(sppAbundTrt = 'matrix'))


# constructor 
#' @rdname metaComm
#' @export

metaComm <- function(sppAbundTrt) {
  return(new('metaComm',
             sppAbundTrt = sppAbundTrt))
}
