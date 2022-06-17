#' @title Hill number summary statistics for `roleData` objects
#' @description These funcitons calculate numbers. The naming convention 
#'     indicates what type of hill number each function caculates; e.g. 
#'     `hillAbund` calcualtes abundance-based Hill numbers, while `hillTrait` 
#'     calculates trait-based Hill numbers
#' @param x the `roleData` object to calculate Hill numbers from
#' @param q the Hill number exponent 
#' @param ... additional parameters, ignored
#' 
#' 
#' @rdname div-sumStats
#' @export

# abundance hill stats
setGeneric('hillAbund', 
           def = function(x, q, ...) standardGeneric('hillAbund'), 
           signature = 'x')


setMethod('hillStats', 
          signature = 'roleData', 
          definition = function(x, q, type) {
              type <- match.arg(type, 
                                c('abundance', 'trait', 'genetic', 'phylo'))
              
              if(type == 'abundance') {
                  X <- rawAbundance(x)[[1]]
                  X <- X[X > 0]
                  
                  f <- .hillDivSAD
              } else if(type == 'trait') {
                  spp <- rawSppID(x)[[1]]
                  trt <- rawTraits(x)[[1]]
                  
                  X <- lapply(unique(spp), function(i) {
                      xs <- sum(spp == i)
                      xt <- mean(trt[spp == i])
                      
                      return(c(xs, xt))
                  })
                  X <- do.call(rbind, X)
                  
                  f <- .hillDivTrait
              }
              
              return(f(X, q))
          }
)





# function for abundance-based
#' @param n is a vector of species abundances

.hillDivSAD <- function(n, q) {
    n <- n/sum(n)
    
    hill <- outer(n, q, '^')
    hill <- colSums(hill)^(1 / (1 - q))
    hill[hill == 1] <- exp(sum(-n * log(n)))
    
    return(hill)
}


# genetic hill stats
setGeneric('hillGenetic', 
           def = function(x, q, ...) standardGeneric('hillGenetic'), 
           signature = 'x')


# trait hill stats
setGeneric('hillTrait', 
           def = function(x, q, ...) standardGeneric('hillTrait'), 
           signature = 'x')


# function for trait-based
#' @param X a matrix with first column = abund, second column = traits
#' @param traits is a vector of traits

.hillDivTrait <- function(X, q) {
    n <- X[, 1]
    traits <- X[, 2]
    
    p <- n / sum(n)
    dij <- as.matrix(dist(traits))
    Q <- as.vector(p %*% dij %*% p)
    a <- outer(p, p, '*') / Q
    
    Hk <- sapply(q, function(qk) {
        if(qk == 1) {
            return(exp(- sum(dij * a * log(a))))
        } else {
            return(sum(dij * a^qk)^(1 / (1 - qk)))
        }
    })
    
    D <- sqrt(Hk / Q)
    
    return(D)
}



# species richness
#' @rdname div-sumStats
#' @export

setGeneric('richness', 
           def = function(x, ...) standardGeneric('richness'), 
           signature = 'x')

setMethod('richness', 
          signature = 'roleData', 
          definition = function(x) {
              length(unique(x@localComm@indSppTrt[, 1]))
          }
)
