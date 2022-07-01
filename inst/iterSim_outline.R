# an outline for iterSim ----
# run (but ignore!) all lines until 65 (starting with comment "FINALLY!!!")


library(roleR)

# set-up some dummy stuff
params1 <- roleParams(individuals_local = 100, 
                      individuals_meta = 1000,
                      species_meta = 500, speciation_local = 0.1,
                      speciation_meta = 1, extinction_meta = 0.8,
                      trait_sigma = 1, env_sigma = 1,
                      comp_sigma = 1, dispersal_prob = 0.1, mutation_rate = 0.01,
                      equilib_escape = 1, num_basepairs = 250,
                      init_type = 'oceanic_island', niter = 1,
                      niterTimestep = 50)


mod <- roleModel(params1)

params1@niter <- 100L


# convert dummy stuff to a list of lists (hoping that's more like what 
# Rcpp will want to work with)

convertS4list <- function(obj) {
    n <- slotNames(obj)
    
    if(is.null(n)) {
        return(obj)
    } else {
        o <- lapply(n, function(k) {
            slot(obj, k)
        })
        
        names(o) <- n
        
        return(lapply(o, convertS4list))
    }
}


modList <- list(params = convertS4list(params1), 
                data = convertS4list(mod@modelSteps[[1]]))

modList$data$localComm$indSpp <- modList$data$localComm$indSppTrt[, 1]
modList$data$localComm$indTrt <- modList$data$localComm$indSppTrt[, 2, 
                                                                  drop = FALSE]
modList$data$localComm$indSeqs <- modList$data$localComm$indSeqs[, 1]
modList$data$localComm$sppGenDiv <- modList$data$localComm$sppGenDiv[, 1]

modList$data$localComm <- modList$data$localComm[-1]


modList$data$metaComm$sppAbund <- modList$data$metaComm$sppAbundTrt[, 1]
modList$data$metaComm$sppTrt <- modList$data$metaComm$sppAbundTrt[, 2, 
                                                                  drop = FALSE]

modList$data$metaComm <- modList$data$metaComm[-1]

# remove these to make sure I don't accidentally use them in `iterSimR`
rm(mod, params1)

# FINALLY!!!!
# the object `modList` is a list of list representation of the roleModel
# object. Here is its structure:
#'    - `modList$params` a list of named parameters (e.g. 
#'      `modList$params$individuals_local`)
#'    - `modList$data` a named list of data elements:
#'        - `modList$data$localComm` a list containing:
#'            - `modList$data$localComm$indSpp` a vector of spp IDs for each 
#'              individual
#'            - `modList$data$localComm$indTrt` a *matrix* of traits for each 
#'              individual; *note: this should remain a matrix in case we want*
#'              *multiple traits in the future*
#'            - `modList$data$localComm$indSeq` a vector of sequences for each 
#'              ind
#'            - `modList$data$localComm$sppGenDiv` a vector of genetic 
#'              diversities for each spp
#'        - `modList$data$metaComm` a list containing
#'            - `modList$data$metaComm$sppAbund` relative spp abundances
#'            - `modList$data$metaComm$sppTrt` species mean trait values; 
#'              *this is a matrix and should stay a matrix as noted above*
#'        - `modList$data$phylo` a list representation of a rolePhylo object, 
#'          see `?rolePhylo` for details


# the actual outline for the loop expressed as an R function ----
#' @param x should be a list of lists exactly like `modList`

iterSimR <- function(x) {
    # make a list to hold output
    outSims <- vector('list', length = x$params$niterTimestep + 1)
    length = x$params$niter / x$params$niterTimestep + 1
    
    # save initial state
    outSims[[1]] <- x$data # this would be indexed at 0 instead of 1 for C++
    
    # index for tracking where to save output
    j <- 2 # set `int j = 1;` for C++
    
    # for ease of referring to different elements, create separate objects
    # in C++ use pointers to save on memory and computation 
    locs <- x$data$localComm
    meta <- x$data$metaComm
    tree <- x$data$phylo
    
    # the loop itself
    # in C++ loop directly over `i`, 
    # i.e. `for(int i = 0; i < x$params$niter - 1; i++)`
    # replacing `x$params$niter` with proper C++ of course
    for(k in 0:(x$params$niter - 1)) {
        i <- k + 1 # hack-ish way of getting indexing to be like C++
        
        # who dies
        whoDead <- sample(1:x$params$individuals_local, 1)
        
        # check for extinction
        localAbund <- sum(locs$indSpp == locs$indSpp[whoDead])
        inMeta <- locs$indSpp[whoDead] <= length(meta$sppAbund)
        if(localAbund <= 0 & !inMeta) {
            tree$alive[locs$indSpp[whoDead]] <- FALSE
        }
        
        # check for birth or immigration
        birthYes <- runif(1) >= x$params$dispersal_prob
        if(birthYes) {
            # birth event ----
            
            # who is the parent individual
            parent <- sample(1:x$params$individuals_local, 1)
            
            # update spp IDs
            locs$indSpp[whoDead] <- locs$indSpp[parent]
            
            # update traits
            # note we're adding some (very specifically calculated) Gaussian
            # noise to this...we can talk more about that
            locs$indTrt[whoDead] <- locs$indTrt[parent] + 
                rnorm(1, mean = 0, 
                      sd = x$params$trait_sigma / 
                          (x$params$speciation_meta + x$params$extinction_meta))
            
            # over-write `parent` so it saves the spp ID of the parent
            # this is necessary for speciation process below
            parent <- locs$indSpp[parent]
            
        } else {
            # immigration event ----
            
            # which species immigrated
            parent <- sample(1:length(meta$sppAbund), 1, prob = meta$sppAbund)
            
            # update abundances
            locs$indSpp[whoDead] <- parent
            
            # update traits
            locs$indTrt[whoDead] <- meta$sppTrt[parent] + 
                rnorm(1, mean = 0, 
                      sd = x$params$trait_sigma / 
                          (x$params$speciation_meta + x$params$extinction_meta))
        }
        
        
        # check for speciation
        speciationYes <- runif(1) < x$params$speciation_local
        
        if(speciationYes) {
            # we know parent ID from above
            # update spp ID in local comm by taking total number of all species
            # ever and add 1
            locs$indSpp[whoDead] <- tree$n + 1 
            
            # traits don't need further updating
            
            # update tree
            # I won't belabor this here; the Rcpp code I wrote for this 
            # should (*hoooopefully*) still work; find it here:
            # https://github.com/role-model/roleR/blob/main/inst/specPhyloRCpp.cpp
            # the one thing I have to do to make all this work is update 
            # `tree$n` and `tree$alive` 
            tree$n <- tree$n + 1
            tree$alive[tree$n] <- TRUE
            
        }
        
        # every niterTimestep save the output
        if((i + 1) %% x$params$niterTimestep == 0) {
            # because R doesn't have pointers I have to re-make the `x` object, 
            # but in C++ (if you use pointers) you shouldn't have to re-make `x` 
            # (I *thiiiiiink*)
            
            x$data$localComm <- locs
            x$data$metaComm <- meta
            x$data$phylo <- tree
            
            # write it out
            outSims[[j]] <- x
            
            # increment `j`
            j <- j + 1
        }
    }
    
    # outside of loop: save last step if haven't already
    if(i + 1 %% x$params$niterTimestep != 0) {
        x$data$localComm <- locs
        x$data$metaComm <- meta
        x$data$phylo <- tree
        outSims <- c(outSims, x)
    }
    
    # return all the output data as a list of lists (of data objects)
    # even in the C++ version I would strongly suggest we return a List (rather
    # than return void) so that we can use `.Call` in R and avoid exposing C++
    # objects and methods to R users
    return(outSims)
}


# test it out
iterSimR(modList)
