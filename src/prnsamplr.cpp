// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

// First draft

// [Rcpp::export]
Rcpp::DataFrame transform_cpp(Rcpp::DataFrame df,
                              arma::vec prn,
                              char direction,
                              double start) {
  // goes nowhere, does nothing
  return 0;
}

// [[Rcpp::export]]
Rcpp::DataFrame pps_cpp(Rcpp::DataFrame df,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec size,
                        arma::vec id){
  int ntot = id.size();
  arma::vec stratid_num;
  arma::vec strat;
  arma::vec lambda;
  arma::vec sum_size;
  arma::vec Q;
  int nstrat;
  
  Rcpp::LogicalVector sampled_gtone;
  arma::vec stratid_gtone;
  arma::vec nsamp_gtone;
  arma::vec prn_gtone;
  arma::vec size_gtone;
  arma::vec id_gtone;
  
  arma::vec stratid_old;
  arma::vec nsamp_old;
  arma::vec prn_old;
  arma::vec size_old;
  arma::vec id_old;
  
  arma::vec lambda_gtone;
  arma::vec Q_gtone;
  int n_gtone;
  
  arma::vec strat_ltone;
  arma::vec stratid_ltone;
  arma::vec nsamp_ltone;
  arma::vec prn_ltone;
  arma::vec size_ltone;
  arma::vec id_ltone;
  arma::vec lambda_ltone;
  arma::vec Q_ltone;
  arma::vec sum_size_ltone;
  arma::vec strat_tot_ltone;
  int nstrat_ltone;
  int n_ltone;
  
  
  arma::vec strat_tot;
  arma::vec id_sorted;
  arma::vec nsamp_sorted;
  arma::vec stratid_sorted;
  arma::vec id_sorted_st;
  arma::vec nsamp_sorted_st;
  
  arma::vec id_int;
  arma::vec Q_int;
  arma::vec Q_out;
  arma::vec lambda_int;
  arma::vec lambda_out;
  Rcpp::LogicalVector sampled_int(ntot, 1);
  Rcpp::LogicalVector sampled_out(ntot, 1);
  
  arma::uvec find_ltone;
  arma::uvec find_gtone;
  
  
  stratid_num = (arma::vec) stratid;
  strat = arma::unique(arma::sort(stratid_num));
  lambda = arma::zeros(ntot);
  sum_size = arma::zeros(ntot);
  Q = arma::zeros(ntot);
  nstrat = strat.size();
  
  
  for (int i=0; i<ntot; i++) {
    for (int j=0; j<ntot; j++) {
      for (int k=0; k<nstrat; k++) {
        if (stratid_num[j] == strat[k]) {
          sum_size[i] += size[j];
        }
      }
    }
  }
  
  for (int i=0; i<ntot; i++) {
    lambda[i] = nsamp[i] * size[i] / sum_size[i];
  }
  
  
  do {
    find_gtone = arma::find(lambda >= 1);
    find_ltone = arma::find(lambda < 1);
    
    if (find_gtone.size() > 0) { 
      stratid_old = stratid_gtone;
      nsamp_old = nsamp_gtone;
      prn_old = prn_gtone;
      size_old = size_gtone;
      id_old = id_gtone;
      
      stratid_gtone = arma::join_cols(stratid_old, stratid.elem(find_gtone));
      nsamp_gtone = arma::join_cols(nsamp_old, nsamp.elem(find_gtone));
      prn_gtone = arma::join_cols(prn_old, prn.elem(find_gtone));
      size_gtone = arma::join_cols(size_old, size.elem(find_gtone));
      id_gtone = arma::join_cols(id_old, id.elem(find_gtone));
      
      n_gtone = id_gtone.size();
      lambda_gtone = arma::ones(n_gtone);
      Q_gtone = arma::zeros(n_gtone);
      
      sampled_gtone(n_gtone, 1);
      for (int i=0; i<n_gtone; i++) {
        sampled_gtone[i] = TRUE;
      }
    }
    
    
    stratid_ltone = stratid.elem(find_ltone);
    nsamp_ltone = nsamp.elem(find_ltone);
    prn_ltone = prn.elem(find_ltone);
    size_ltone = size.elem(find_ltone);
    id_ltone = id.elem(find_ltone);
    
    n_ltone = id_ltone.size();
    stratid_num = (arma::vec) stratid_ltone;
    strat = arma::unique(arma::sort(stratid_num));
    lambda = arma::zeros(n_ltone);
    sum_size_ltone = arma::zeros(n_ltone);
    nstrat_ltone = strat.size();
    
    for (int i=0; i<n_ltone; i++) {
      for (int j=0; j<n_ltone; j++) {
        for (int k=0; k<nstrat_ltone; k++) {
          if (stratid_num[j] == strat[k]) {
            sum_size_ltone[i] += size_ltone[j];
          }
        }
      }
    }
    
    for (int i=0; i<n_ltone; i++) {
      for (int j=0; j<n_gtone; j++) {
        if (stratid_ltone[i] == stratid_gtone[j]) {
          nsamp_ltone[i]--;
        }
      }
    }
    
    for (int i=0; i<n_ltone; i++) {
      lambda[i] = nsamp_ltone[i] * size_ltone[i] / sum_size_ltone[i];
    }
  } while (arma::any(lambda >= 1));
  
  
  Q_ltone = arma::zeros(n_ltone);
  for (int i=0; i<n_ltone; i++) {
    Q_ltone[i] = prn_ltone[i] * (1 - lambda[i]) / (lambda[i] * (1 - prn_ltone[i]));
  }
  
  strat_ltone = arma::unique(arma::sort(stratid_ltone));
  nstrat_ltone = strat_ltone.size();
  Rcpp::LogicalVector sampled_sorted(n_ltone, 1);
  Rcpp::LogicalVector sampled_ltone(n_ltone, 1);
  strat_tot = arma::zeros(nstrat_ltone+1);
  
  id_sorted = id_ltone(arma::sort_index(Q_ltone));
  nsamp_sorted = nsamp_ltone(arma::sort_index(Q_ltone));
  stratid_sorted = stratid_ltone(arma::sort_index(Q_ltone));
  
  id_sorted_st = id_sorted(arma::stable_sort_index(stratid_sorted));
  nsamp_sorted_st = nsamp_sorted(arma::stable_sort_index(stratid_sorted));
  
  strat_tot[0] = 0;
  for (int i = 1; i <= nstrat_ltone; i++) {
    strat_tot[i] = strat_tot[i-1] + arma::sum(stratid_ltone == stratid_ltone[strat_ltone[i-1]]);
  }
  
  
  for (int i = 0; i < nstrat_ltone; i++) {
    for (int j = strat_tot[i]; j < strat_tot[i+1]; j++) {
      if (stratid_ltone[j] == stratid_ltone[strat_ltone[i]]) {
        int ind_strat_ltone = j - strat_tot[i];
        if (ind_strat_ltone < nsamp_sorted_st[j]) {
          sampled_sorted[j] = TRUE;
        }
        else {
          sampled_sorted[j] = FALSE;
        }
      }
    }
  }
  
  for (int i = 0; i < n_ltone; i++) {
    for (int j = 0; j < n_ltone; j++) {
      if (id_ltone[i] == id_sorted_st[j]) {
        sampled_ltone[i] = sampled_sorted[j];
      }
    }
  }
  
  id_int = arma::join_cols(id_gtone, id_ltone);
  Q_int = arma::join_cols(Q_gtone, Q_ltone);
  lambda_int = arma::join_cols(lambda_gtone, lambda);
  Q_out = arma::zeros(ntot);
  lambda_out = arma::zeros(ntot);
  
  for (int i = 0; i < n_gtone; i++) {
    sampled_int[i] = sampled_gtone[i];
  }
  
  for (int i = n_gtone; i < n_ltone+n_gtone; i++) {
    sampled_int[i] = sampled_ltone[i];
  }
  
  for (int i = 0; i < ntot; i++) {
    for (int j = 0; j <ntot; j++) {
      if (id_int[i] == id[j]) {
        Q_out[j] = Q_int[i];
        lambda_out[j] = lambda_int[i];
        sampled_out[j] = sampled_int[i];
      }
    }
  }
  
  df["lambda"] = lambda_out;
  df["Q"] = Q_out;
  df["sampled"] = sampled_out;
  
  return df;
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