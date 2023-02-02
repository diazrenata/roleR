// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "roleDataCpp.cpp"
#include "roleParamsCpp.cpp"

using namespace Rcpp;

// iterModelCpp.cpp contains:
//      + Cpp functions used within the core loop of iterModelCpp
//          1. sample_zero_to_x
//          2. sample_index_using_probs
//          3. call_birth
//          4. call_dispersal
//          5. call_speciation
//
//      + iterModelCpp, a function exported to R that iterates a model
//      + Wrappers around the loop C++ functions exported to R for testing - this structure avoids have to redundantly wrap every function
//          1. intFunCpp
//          2. dataFunCpp
//          3. vectFunCpp

// sample a random integer from 0 to x
// replicates sample(1:x, 1)
int sample_zero_to_x(int x)
{
    return((int) (R::runif(0,1) * (double) x));
}

// sample an index using a vector of relative probabilities
int sample_index_using_probs(NumericVector probs){
    IntegerVector v = Rcpp::sample(probs.length(), 1, false, probs, false);
    return(v(0));
}

// get prob of death for each individual due to env filtering
NumericVector get_filtering_death_probs(int i, roleDataCpp &d, roleParamsCpp &p){
    return(1 - exp(-1/p.env_sigma[0] * pow(d.indTraitL - 0, 2)));
}

// get prob of death for each individual due to competition
NumericVector get_comp_death_probs(int i, roleDataCpp &d, roleParamsCpp &p){
    arma::colvec comp_probs_a = arma::sum(arma::exp((-1/p.comp_sigma(i)) * d.traitDiffsSq),1) / p.individuals_local[0];
    return(Rcpp::wrap(comp_probs_a.begin(),comp_probs_a.end()));
}

// get combined probs of death from filtering and competition, weighted by degree of neutrality
NumericVector get_combined_death_probs(int i, roleParamsCpp &p, NumericVector f_probs, NumericVector c_probs){
    return(p.neut_delta[0] + (1-p.neut_delta[0]) * ((p.env_comp_delta[0] * f_probs) + ((1-p.env_comp_delta[0]) * c_probs)));
}

// check if extinction happens and if it does update the saved species extinction steps & the alive vector in the phylo
void update_extinct(int i, roleDataCpp &d, roleParamsCpp &p, int dead_index){ //NOTE - uses dead_index as dead species index
    
    int dead_species = dead_index;
    // check for extinction
    // extinct if species abundance of dead individual is now <0 
    bool extinct_in_local = d.spAbundL(dead_species) <= 0;
    // not in meta if species of the dead individual is not in meta
    bool not_in_meta = dead_species > d.spAbundM.length();
    
    // check for local extinction
    if(extinct_in_local){
        d.spExtinctionStepL(dead_species) = i;
    }
    
    // check for local and meta extinction
    if(extinct_in_local & not_in_meta){
        d.aliveP(dead_species) = false;
    }
}

// call death on a roleDataCpp object using the params at iteration i
// returns the individual chosen for death which is needed in the rest of the loop
int call_death(int i, roleDataCpp &d, roleParamsCpp &p){
    
    int n_indv = p.individuals_local[0];
    // start with neutral death probs where all probs are equal: rep(1,n_indv)
    NumericVector death_probs(n_indv,1.0);
    
    // if not neutral, calculate initial probs of death from environmental filtering
    if(p.neut_delta[0] != 1)
    {
        // METHOD - get probs of death due to env filtering
        NumericVector env_filter_probs = get_filtering_death_probs(i,d,p);
        
        // save the used env sigma as the prevEnvSigma for the next loop
        d.prevEnvSigma = p.env_sigma[0];
        
        // METHOD - get probs of death due to competition
        NumericVector comp_probs = get_comp_death_probs(i,d,p);

        // METHOD - calculate combined death probs
        death_probs = get_combined_death_probs(i,p,env_filter_probs,comp_probs);
    }
    
    // get dead index and dead species
    int dead_index = sample_index_using_probs(death_probs);
    int dead_species = d.indSpeciesL(dead_index);

    // subtract from abundance of dead species
    d.spAbundL(dead_species) = d.spAbundL(dead_species) - 1;
    
    // check if extinct and update phylo
    update_extinct(i,d,p,dead_species);
    
    return(dead_index);
}

// passing explicitly by reference using '&' makes things as fast or almost as fast as if code is pasted into loop
// given a presampled parent, sets the sp of the new individual, adds to the abundance of the sp matrix, 
// and calculate and add new trait value
void call_birth(int i, int dead_index, int parent_indv, roleDataCpp &d, roleParamsCpp &p, bool print){
    
    // get the birthed species 
    int birthed_species = d.indSpeciesL(parent_indv); 
    
    // set the species of the new indv to that of the parent 
    d.indSpeciesL(dead_index) = birthed_species;

    // add to the abundance of the species matrix
    d.spAbundL(birthed_species) = d.spAbundL(birthed_species) + 1; 
    
    // calculate trait change from parent
    NumericVector trait_change = Rcpp::rnorm(1, 0, p.trait_sigma(1) / (p.speciation_meta(1) + p.extinction_meta(1)));

    // add new trait value
    d.indTraitL(dead_index) = d.indTraitL(parent_indv) + trait_change(0);
}

// set the species of the new indv, update last time of origin, add to sp abundance matrix, and calculate and add new trait
void call_dispersal(int i, int dead_index, int parent_indv, roleDataCpp &d, roleParamsCpp &p, bool print){
    
    // sample a parent index using species abundances as probs
    int parent_index = parent_indv;

    // set the species to the parent species from meta
    d.indSpeciesL(dead_index) = parent_index;
    
    // update time of last origin if the species' last local abundance was 0 
    if(d.spAbundL(d.indSpeciesL(dead_index)) <= 0){
        d.spLastOriginStepL(d.indSpeciesL(dead_index)) = i;
    }
    
    // add to the abundance of the species matrix
    d.spAbundL(d.indSpeciesL(dead_index)) = d.spAbundL(d.indSpeciesL(dead_index)) + 1;
    
    // calculate trait change from parent
    NumericVector trait_change = Rcpp::rnorm(1, 0, p.trait_sigma(1) / (p.speciation_meta(1) + p.extinction_meta(1)));

    // add new trait value
    d.indTraitL(dead_index) = d.spTraitM(parent_index) + trait_change(0);
}

// get the probs of speciation, determines by weighted local and meta abundance
NumericVector get_speciation_probs(int i, roleDataCpp &d, roleParamsCpp &p){
    
    // get dispersal prob
    float dp = p.dispersal_prob(i);
    
    // calculate speciation probs as sum of local and meta abundances weighted by dispersal prob
    NumericVector probs = dp * (d.spAbundM / sum(d.spAbundM)) + 
        (1 - dp) * (d.spAbundL / sum(d.spAbundL));
    
    // normalize 
    probs = (abs(probs)+probs)/2;
    
    return(probs);
}

// call the local and meta aspects of speciation
void update_speciation_local_meta(int i, int dead_index, roleDataCpp &d, roleParamsCpp &p, bool dispersed_this_iter, int speciation_sp, bool print){
    
    // calculate deviation of trait from previous species trait
    float trait_dev = R::rnorm(0, p.trait_sigma(i));
    float parent_trait_val = 0;
    
    // if dispersed, parent trait comes from meta
    if(dispersed_this_iter){
        parent_trait_val = d.spTraitM(speciation_sp);
    }
    // otherwise it comes from local
    else{
        parent_trait_val = d.spTraitL(speciation_sp);
    }
    
    // set new trait value
    d.indTraitL(dead_index) = parent_trait_val + trait_dev;

    // the indv that was birthed or immigrated becomes the new species
    d.indSpeciesL(dead_index) = d.nTipsP(0); 
    
    // update time of last origin
    d.spLastOriginStepL(d.indSpeciesL(dead_index)) = i;
}

// call the phylo aspect of speciation
void update_speciation_phylo(int i, roleDataCpp &d, roleParamsCpp &p, int speciation_sp){
    
    // set easy named variables for phylogeny speciation
    NumericMatrix e = d.edgesP;
    NumericVector l = d.lengthsP;
    int n = d.nTipsP(0);
    LogicalVector alive = d.aliveP;
    
    // nrows of the edge matrix
    int eMax = e.nrow();

    // NOTE - this could be made into a separate function
    // find index of where unrealized edges in edge matrix start
    // equivalent to eNew <- min(which(e[, 1] == -1))
    int eNew = -1;
    
    for (int k = 0; k < eMax; k++) {
        if (e(k, 0) == -2) {
            eNew = k;
            break;
        }
    }
    
    // NOTE - possible strategy for augmentation
    // add if statement to catch whether eNew >= eMax
    // if eNew >= eMax, augment e with addition eMax rows
    // else leave as is
    
    // NOTE - this could be made into a separate function
    // get index of the edge matrix of where to add new edge
    // equivalent to j <- which(e[, 2] == i)
    int j = -1;
    for (int k = 0; k < eMax; k++) {
        if (e(k, 1) == speciation_sp) {
            j = k;
            break;
        }
    }
    
    // add one to internal node indices
    // equivalent to e[e > n] <- e[e > n] + 1
    for (int r = 0; r < eNew; r++) {
        for (int c = 0; c < 2; c++){
            if (e(r, c) >= n) {
                e(r, c) ++;
            }
        }
    }

    // add new internal node
    int newNode = 2 * n; // index of new node n+1
    e(eNew, 0) = newNode;
    e(1 + eNew, 0) = newNode;

    // add tips
    e(eNew, 1) = e(j, 1); // replace old tip
    e(eNew + 1, 1) = n; // add new tip

    // update ancestry of internal nodes
    e(j, 1) = newNode;

    // augment edge lengths
    l[eNew] = 0;
    l[1 + eNew] = 0;
    
    // increase all tip edge lengths by 1 time step
    for (int r = 0; r <= eNew + 1; r++) {
        if (e(r, 1) <= n + 1) { //n+1
            l(r) ++;
        }
    }
    
    // update alive vector
    alive(n) = TRUE; // NOTE - double check that this updates properly
    
    // increment nTipsP
    d.nTipsP(0) = d.nTipsP(0) + 1;
}

// call speciation
void call_speciation(int i, int dead_index, roleDataCpp &d, roleParamsCpp &p, bool dispersed_this_iter, bool print){
    
    // computing probs of speciation as sum of weighted meta and local abundances
    NumericVector probs = get_speciation_probs(i,d,p);
    
    // sample using probs (may be able to replace with sample_index_using_probs)
    IntegerVector s = sample(d.spAbundM.length(), 1, false, probs);
    
    // make i from 0 to phylo.n - 1 (previously 1 to phylo.n)
    int speciation_sp = s(0) - 1;
    
    // update the local and meta traits, sp vector, and time of last origin
    update_speciation_local_meta(i,dead_index,d,p,dispersed_this_iter,speciation_sp,print);
    
    // update the phylogeny
    update_speciation_phylo(i,d,p,speciation_sp);
}

// update the squared differences between the traits of every individual
void update_trait_diffs_sq(int dead_index, roleDataCpp &d, roleParamsCpp &p){
    
    // update traitDiffsSq using two for loops 
    // updates ONLY the row and column of the dead_index
    for(int r = 0; r < p.individuals_local[0]; r++) 
    {
        d.traitDiffsSq(r,dead_index) = pow(d.indTraitL(r) - d.indTraitL(dead_index),2);
    }
    for(int c = 0; c < p.individuals_local[0]; c++)
    {
        d.traitDiffsSq(dead_index,c) = pow(d.indTraitL(dead_index) - d.indTraitL(c),2);
    }
}

// if non-neutral, save on computation by updating env_filtering probs fully if env_sigma changes and only for the new indv if it doesn't
// make sure env_filter_probs initial null state is used properly in death
void update_env_filter_probs(int i, int dead_index,roleDataCpp &d,roleParamsCpp &p){
    
    // if neutral 
    if(p.neut_delta[0] != 1)
    {
        // check if new env_sigma, if so must recalculate envFilterProbs entirely
        if(d.prevEnvSigma != p.env_sigma[i]){
            d.envFilterProbs = 1 - exp((-1/p.env_sigma(i)) * pow(d.indTraitL - 0, 2));
        }
        //otherwise just update the probs of the new individual
        else{
            d.envFilterProbs(dead_index) = 1 - exp((-1/p.env_sigma(i)) * pow(d.indTraitL(dead_index), 2));
        }
        d.prevEnvSigma = p.env_sigma[i]; // NOTE - double check this is right
    }
}

// update the local species sum of reciprocals
// used in R in the genetic simulation 
void update_local_sp_sum_recipr(int i, int dead_index, roleDataCpp &d, roleParamsCpp &p){

    // for each species...
    for(int s = 0; s < d.nTipsP(0)-1; s++){
        // if species is currently alive in local
        if(d.spAbundL(s) > 0){
            
            // add new abundance to species reciprocal sum 
            d.spReciprSumL(s) = d.spReciprSumL(s) + (1/d.spAbundL(s));
            
            // get n, the number of steps in this emergence period, as
            // the current iteration - the iteration of origin 
            int n = i - d.spLastOriginStepL(s);
            
            // harmonic mean is then n / the current reciprocal sum 
            d.spAbundHarmMeanL(s) = n / d.spReciprSumL(s);
        }
        // else species is dead in local
        else{
            d.spReciprSumL(s) = 0; 
            d.spAbundHarmMeanL(s) = 0;
        }
    }
}

// copy a roleDataCpp object to it's R side S4 equivalent
S4 role_data_from_cpp(roleDataCpp d){
    
    // construct S4 object, cloning each member of roleDataCpp directly to slots
    S4 out_l("localComm");
    out_l.slot("indSpecies") = Rcpp::clone(d.indSpeciesL);
    out_l.slot("indTrait") = Rcpp::clone(d.indTraitL);
    out_l.slot("spAbund") = Rcpp::clone(d.spAbundL);
    out_l.slot("spTrait") = Rcpp::clone(d.spTraitL);
    out_l.slot("spAbundHarmMean") = Rcpp::clone(d.spAbundHarmMeanL);
    out_l.slot("spLastOriginStep") = Rcpp::clone(d.spLastOriginStepL);
    out_l.slot("spExtinctionStep") = Rcpp::clone(d.spExtinctionStepL);

    S4 out_m("metaComm");
    out_m.slot("spAbund") = Rcpp::clone(d.spAbundM);
    out_m.slot("spTrait") = Rcpp::clone(d.spTraitM);

    S4 out_p("rolePhylo");
    out_p.slot("n") = Rcpp::clone(d.nTipsP);
    out_p.slot("e") = Rcpp::clone(d.edgesP);
    out_p.slot("l") = Rcpp::clone(d.lengthsP);
    out_p.slot("alive") = Rcpp::clone(d.aliveP);
    out_p.slot("tipNames") = Rcpp::clone(d.tipNamesP);
    out_p.slot("scale") = Rcpp::clone(d.scaleP);

    S4 out_d("roleData");
    out_d.slot("localComm") = out_l;
    out_d.slot("metaComm") = out_m; 
    out_d.slot("phylo") = out_p;
    
    return(out_d);
}
    
// [[Rcpp::export]]
List iterModelCpp(RObject local, RObject meta, RObject phylo, RObject params, bool print) {

    // save niter and niterTimestep
    int niter = params.slot("niter");
    int niter_timestep = params.slot("niterTimestep");

    // make cpp objects for data and params
    roleDataCpp d(local,meta,phylo);
    roleParamsCpp p = roleParamsCpp(params,niter); // constructor samples/stretches
    
    // save n_indv to use easily throughout
    int n_indv = p.individuals_local[0];
    
    // if not neutral, calculate initial probs of death from environmental filtering
    NumericVector env_filter_probs (n_indv,1.0);
    if(p.neut_delta[0] != 1)
    {
        env_filter_probs = 1 - exp(-1/p.env_sigma[0] * pow(d.indTraitL - 0, 2));
        if(print){Rcout << "calculated initial probs of death from env filtering: " << env_filter_probs << "\n";}
    }
    d.envFilterProbs = env_filter_probs;
    d.prevEnvSigma = p.env_sigma[0]; // NOTE - double check this is correct
    
    // create out array to hold timeseries data 
    RObject out[(niter / niter_timestep) + 1]; 
    
    // save the initial state to index 0 
    out[0] = role_data_from_cpp(d);
    
    // loop from 0 to niter - 1 
    for(int i = 0; i < (int) params.slot("niter"); i++) {
        if(print){Rcout << "started iteration " << i << "\n";}
        
        // METHOD - call death
        int dead_index = call_death(i,d,p);
        
        // set dispersal var for use in speciation, which is slightly different depending
        bool dispersed_this_iter = false; 
        
        // check for birth (prob = 1 - dispersal_prob) 
        if(R::runif(0,1) >= p.dispersal_prob(i)){
            if(print){Rcout << "birthed, dispersal prob: " << p.dispersal_prob(i) << "\n";}
            
            // sample for the parent
            int parent_indv = sample_zero_to_x(p.individuals_local(i));
            
            // METHOD - call birth
            call_birth(i, dead_index, parent_indv, d, p, print);
        }
        else{
            if(print){Rcout << "dispersed, dispersal prob: " <<  p.dispersal_prob(i) << "\n";}
            dispersed_this_iter = true;
            
            // sample a parent index using species abundances as probs
            int parent_index = sample_index_using_probs(d.spAbundM);
            
            // METHOD - call dispersal
            call_dispersal(i,dead_index, parent_index, d, p, print);
        }
        
        // METHOD - update the squared differences between traits at the dead_index
        update_trait_diffs_sq(dead_index,d,p);
        
        // METHOD - update the probs of enviromental filtering ONLY if non-neutral and either for all indv or just the new one
        update_env_filter_probs(i, dead_index,d,p);
        
        // randomly decide if speciation occurs
        if(R::runif(0,1) < p.speciation_local(i))
        {
            // METHOD - call speciation and get the chosen species 
            call_speciation(i, dead_index, d, p, dispersed_this_iter, print);
        }
        
        // METHOD - update the local species sum of reciprocals
        update_local_sp_sum_recipr(i, dead_index, d, p);
        
        // save if i is 0, 9, 19 ... 99 
        if((i + 1) % niter_timestep == 0)
        {
            // if iter is 0, add to 0th index
            if(i == 0){
                // METHOD - convert to R S4 object
                out[i] = role_data_from_cpp(d);
            }
            // otherwise add to the correct timestep index
            else{
                out[(i + 1) / niter_timestep] = role_data_from_cpp(d); 
            }
        }
    } 
    
    // build the final list and return it to R
    List out_list = List::create(out[0]);
    for(int j = 1; j < sizeof(out)/sizeof(out[0]); j++)
    {
        out_list.push_back(out[j]);
    }
    return(out_list);
};


// these funs, one per return data type, are R wrappers around multiple Cpp functions
// these avoid having to wrap all ~20 functions individually 
// ONLY used for testing and nothing else
int intFunCpp(Rcpp::StringVector fun_name,
                NumericVector probs=NULL, int x=NULL) {
    std::string fn = Rcpp::as<std::string>(fun_name(0));
    
    // tried switch, didn't work but may revisit
    if(fn == "sample_index_using_probs"){
        return(sample_index_using_probs(probs));
    }
    if(fn == "sample_zero_to_x"){
        return(sample_zero_to_x(x));
    }
}
S4 dataFunCpp(Rcpp::StringVector fun_name, 
                       RObject local=NULL, RObject meta=NULL,RObject phylo=NULL, //used universally
                       RObject params=NULL, int niter=NULL, int i=NULL, //used universally
                       int dead_index=NULL, // used universally
                       int parent_indv=NULL, // used by call_birth and call_dispersal
                       bool dispersed_this_iter=NULL, // used by call_speciation and update_speciation_local_meta
                       int speciation_sp=NULL) { // used in update_speciation_local_meta
    
    // create Cpp objects
    roleDataCpp d(local,meta,phylo);
    roleParamsCpp p(params,niter);
    std::string fn = Rcpp::as<std::string>(fun_name(0));
    
    // tried switch, didn't work
    if(fn == std::string("call_birth")){
        call_birth(i,dead_index,parent_indv,d, p,true);
        return(role_data_from_cpp(d));
    }
    if(fn == std::string("call_dispersal")){
        call_dispersal(i,dead_index,parent_indv,d, p,true);
        return(role_data_from_cpp(d));
    }
    if(fn == std::string("update_trait_diffs_sq")){
        update_trait_diffs_sq(dead_index,d, p);
        return(role_data_from_cpp(d));
    }
    if(fn == std::string("call_speciation")){
        call_speciation(i, dead_index, d, p, dispersed_this_iter, true);
        return(role_data_from_cpp(d));
    }
    if(fn == std::string("update_speciation_local_meta")){
        update_speciation_local_meta(i,dead_index,d,p,dispersed_this_iter,speciation_sp,true);
        return(role_data_from_cpp(d));
    }
    if(fn == std::string("update_speciation_phylo")){
        update_speciation_phylo(i,d,p,speciation_sp);
        return(role_data_from_cpp(d));
    }
}

NumericVector vectFunCpp(Rcpp::StringVector fun_name,
                         RObject local=NULL, RObject meta=NULL,RObject phylo=NULL, // used universally 
                         RObject params=NULL, int niter=NULL, int i = NULL){ // used universally 
    // create Cpp objects
    roleDataCpp d(local,meta,phylo);
    roleParamsCpp p(params,niter);
    std::string fn = Rcpp::as<std::string >(fun_name(0));
    
    // tried switch, didn't work
    if(fn == "get_filtering_death_probs"){
        return(get_filtering_death_probs(i,d,p));
    }
    if(fn == "get_comp_death_probs"){
        return(get_comp_death_probs(i,d,p));
    }
    if(fn == "get_speciation_probs"){
        return(get_speciation_probs(i,d,p));
    }
}

// export Rcpp modules for use in R
// only 4 functions get exported - the core loop and the test wrappers
RCPP_MODULE(iterModelCpp) {
    function("iterModelCpp", &iterModelCpp);
    function("intFunCpp", &intFunCpp);
    function("vectFunCpp", &vectFunCpp);
    function("dataFunCpp", &dataFunCpp);
}
