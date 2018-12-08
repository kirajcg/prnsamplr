// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


// Does nothing useful so far
// [[Rcpp::export]]
Rcpp::DataFrame srs_cpp(Rcpp::DataFrame df,
                        std::string stratid,
                        std::string nsamp,
                        std::string prn){
  Rcpp::CharacterVector strata = df[stratid] ;
  Rcpp::NumericVector   prns   = df[prn] ;
  Rcpp::IntegerVector   n      = df[nsamp] ;
  
  std::unordered_set<SEXP> tab(strata.begin(), strata.end());
  int nstrat = tab.size();
  
  int ntot = prns.size();
  Rcpp::LogicalVector sampled(ntot,1);
  
  for (int i=0; i<ntot; i++) {
    sampled(i) = TRUE;
  }
  
  df["sampled"] = sampled;
  return df;
}