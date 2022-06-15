#' @title Get raw data from `roleData` objects
#' @description Gets raw data
#' @param x the `roleData` object to get raw data from
#' @param ... additional parameters, ignored
#' 
#' 
#' @rdname raw-sumStats
#' @export

# abundance
setGeneric('rawAbundance', 
           def = function(x, ...) standardGeneric('rawAbundance'), 
           signature = 'x')

setMethod('rawAbundance', 
          signature = 'roleData', 
          definition = function(x) {
              tabulate(x@localComm@indSppTrt[, 1])
          }
)


# spp IDs
#' @rdname raw-sumStats
#' @export

setGeneric('rawSppID', 
           def = function(x, ...) standardGeneric('rawSppID'), 
           signature = 'x')

setMethod('rawSppID', 
          signature = 'roleData', 
          definition = function(x) {
              x@localComm@indSppTrt[, 1]
          }
)

# traits
#' @rdname raw-sumStats
#' @export

setGeneric('rawTraits', 
           def = function(x, ...) standardGeneric('rawTraits'), 
           signature = 'x')

setMethod('rawTraits', 
          signature = 'roleData', 
          definition = function(x) {
              x@localComm@indSppTrt[, 2]
          }
)


# gen div
#' @rdname raw-sumStats
#' @export

setGeneric('rawGenDiv', 
           def = function(x, ...) standardGeneric('rawGenDiv'), 
           signature = 'x')

setMethod('rawGenDiv', 
          signature = 'roleData', 
          definition = function(x) {
              x@localComm@sppGenDiv[, 1]
          }
)



# gen seqs
#' @rdname raw-sumStats
#' @export

setGeneric('rawSeqs', 
           def = function(x, ...) standardGeneric('rawSeqs'), 
           signature = 'x')

setMethod('rawSeqs', 
          signature = 'roleData', 
          definition = function(x) {
              x@localComm@indSeqs[, 1]
          }
)


# branch lengths
#' @rdname raw-sumStats
#' @export

setGeneric('rawBranchLengths', 
           def = function(x, ...) standardGeneric('rawBranchLengths'), 
           signature = 'x')

setMethod('rawBranchLengths', 
          signature = 'roleData', 
          definition = function(x) {
              x@phylo@l[x@phylo@e[, 1] != -1]
          }
)


# ape phylo
#' @rdname raw-sumStats
#' @export

setGeneric('apePhylo', 
           def = function(x, ...) standardGeneric('apePhylo'), 
           signature = 'x')

setMethod('apePhylo', 
          signature = 'roleData', 
          definition = function(x) {
              as(x@phylo, 'phylo')
          }
)


# species richness
#' @rdname raw-sumStats
#' @export

setGeneric('rawRichness', 
           def = function(x, ...) standardGeneric('rawRichness'), 
           signature = 'x')

setMethod('rawRichness', 
          signature = 'roleData', 
          definition = function(x) {
              length(unique(x@localComm@indSppTrt[, 1]))
          }
)


#' @title Get Hill numbers from `roleData` objects
#' @description Gets Hill numbers
#' @param x the `roleData` object to calculate Hill numbers from
#' @param q the Hill number exponent 
#' @param type Hill number type; one of 'abundance', 'trait', 'phylo'
#' @param ... additional parameters, ignored
#' 
#' 
#' @rdname raw-sumStats
#' @export

setGeneric('hillStats', 
           def = function(x, q, type, ...) standardGeneric('rawBranchLengths'), 
           signature = 'x')


