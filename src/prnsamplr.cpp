// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
//using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// First draft
// [[Rcpp::export]]
Rcpp::DataFrame srs_cpp(Rcpp::DataFrame df,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec id){
  
  
  int ntot = id.size();
  arma::uvec strat = arma::find_unique(stratid);
  int nstrat = strat.size();
  Rcpp::LogicalVector sampled(ntot, 1);
  Rcpp::LogicalVector sampled_sorted(ntot, 1);
  
  arma::vec stratid_sorted = stratid(arma::stable_sort_index(prn));
  arma::vec id_sorted = id(arma::stable_sort_index(prn));
  arma::vec nsamp_sorted = nsamp(arma::stable_sort_index(prn));
  
  for (int i = 0; i < nstrat; i++) {
    for (int j = 0; j < ntot; j++) {
      if (stratid[j] == stratid[i]) {
        if (j <= nsamp_sorted[j]) {
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
      if (id[i] == id_sorted[j]) {
        sampled[i] = sampled_sorted[j];
      }
    }
  }

  df["sampled"] = sampled;
  return df;
}