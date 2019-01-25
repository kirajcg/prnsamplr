// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

// First draft
// [[Rcpp::export]]
Rcpp::DataFrame pps_cpp(Rcpp::DataFrame df,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec size,
                        arma::vec id){
  // goes nowhere, does nothing
  return 0;
}


// [[Rcpp::export]]
Rcpp::DataFrame srs_cpp(Rcpp::DataFrame df,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec id){
  
  
  int ntot = id.size();
  arma::uvec strat = arma::find_unique(arma::sort(stratid));
  int nstrat = strat.size();
  Rcpp::LogicalVector sampled(ntot, 1);
  Rcpp::LogicalVector sampled_sorted(ntot, 1);
  Rcpp::IntegerVector strat_tot(nstrat+1, 1);
  
  arma::vec id_sorted = id(arma::sort_index(prn));
  arma::vec nsamp_sorted = nsamp(arma::sort_index(prn));
  arma::vec stratid_sorted = stratid(arma::sort_index(prn));
  arma::vec id_sorted_st = id_sorted(arma::stable_sort_index(stratid_sorted));
  arma::vec nsamp_sorted_st = nsamp_sorted(arma::stable_sort_index(stratid_sorted));
  
  strat_tot[0] = 0;
  for (int i = 1; i <= nstrat; i++) {
    strat_tot[i] = strat_tot[i-1] + arma::sum(stratid == stratid[strat[i-1]]);
  }
  
  for (int i = 0; i < nstrat; i++) {
    for (int j = strat_tot[i]; j < strat_tot[i+1]; j++) {
      if (stratid[j] == stratid[strat[i]]) {
        int ind_strat = j - strat_tot[i];
        if (ind_strat < nsamp_sorted_st[j]) {
          sampled_sorted[j] = TRUE;
        }
        else {
          sampled_sorted[j] = FALSE;
        }
      }
    }
  }
  
  
  for (int i = 0; i < ntot; i++) {
    for (int j = 0; j < ntot; j++) {
      if (id[i] == id_sorted_st[j]) {
        sampled[i] = sampled_sorted[j];
      }
    }
  }
  
  df["sampled"] = sampled;
  return df;
}