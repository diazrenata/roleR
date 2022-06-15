#' @title One run of the RoLE model
#'
#' @description An S4 class to hold one model run of the RoLE model
#' 
#' @slot modelSteps a list of roleData objects, one for each recorded time step
#' @slot params a roleParams object with the RoLE model params
#' 
#' @rdname roleModel
#' @export

setClass('roleModel',
         slots = c(params = 'roleParams', modelSteps = 'list'))

# constructor for roleModel
#' @rdname roleModel
#' @export

roleModel <- function(params) {
    J <- params@individuals_local[1]
    if(J < 100) {
        stop('`individuals_local` (set in `roleParams`) cannot be less than 100')
    }

    Sm <- params@species_meta
    if(Sm < 200) {
        stop('`species_meta` (set in `roleParams`) cannot be less than 200')
    }
    
    locs <- localComm(indSppTrt = matrix(c(rep(1, J), 
                                           seq(1, 1.2, length.out = J)), 
                                         ncol = 2), 
                      indSeqs = matrix(rep('ATCG', J), ncol = 1), 
                      sppGenDiv = matrix(1, ncol = 1))
    
    meta <- metaComm(sppAbundTrt = matrix(c((Sm:1) / Sm, 1:Sm), ncol = 2))
    
    phylo <- ape::rphylo(Sm, params@speciation_meta, params@extinction_meta)
    phylo <- as(phylo, 'rolePhylo')
    
    dat <- roleData(localComm = locs, metaComm = meta, phylo = phylo)
    
    niter <- params@niter
    if(niter > 100) {
        stop('`niter` (set in `roleParams`) cannot be greater than 100')
    }
    
    niterTimestep <- params@niterTimestep
    
    # output data
    modelSteps <- vector('list', length = niter / niterTimestep + 1)
    modelSteps[[1]] <- dat
    j <- 2 # counter to keep track of where to save data
    
    for(i in 1:niter) {
        # update local comm
        locs@indSppTrt[i, ] <- c(i + 1, meta@sppAbundTrt[i + 1, 2])
        locs@sppGenDiv <- rbind(locs@sppGenDiv, 
                                matrix(1 / (i + 1), nrow = 1, ncol = 1))
        
        # write data every `niterTimestep`
        if(i %% niterTimestep == 0) {
            # over-write local comm in dat
            dat@localComm <- locs
            
            # save it
            modelSteps[[j]] <- dat
            j <- j + 1
        }
    }
    
    new('roleModel', 
        params =  params, 
        modelSteps = modelSteps)
}


# testing
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
