// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

// First draft



// [[Rcpp::export]]
Rcpp::DataFrame srs_cpp(const Rcpp::DataFrame& dfin,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec id){
  
  Rcpp::DataFrame dfout = dfin;
  
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
  
  dfout["sampled"] = sampled;
  return dfout;
}

// [Rcpp::export]
Rcpp::DataFrame transform_cpp(Rcpp::DataFrame df,
                              arma::vec prn,
                              char direction,
                              double start) {
  // goes nowhere, does nothing
  return 0;
}

// [[Rcpp::export]]
Rcpp::DataFrame pps_cpp(const Rcpp::DataFrame& dfin,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec size,
                        arma::vec id){
  
  Rcpp::DataFrame dfout = dfin;
  
    
  int ntot = id.size();
  arma::vec stratid_num;
  arma::vec strat;
  arma::vec lambda;
  arma::vec sum_size;
  arma::vec Q;
  
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
  int n_gtone = 0;
  
  arma::vec strat_ltone;
  arma::vec stratid_ltone;
  arma::vec nsamp_ltone;
  arma::vec prn_ltone;
  arma::vec size_ltone;
  arma::vec id_ltone;
  arma::vec sum_size_ltone;
  arma::vec strat_tot_ltone;
  
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
  int nstrat = strat.size();
  
  
  for (int i=0; i<ntot; i++) {
    for (int j=0; j<ntot; j++) {
      if (stratid_num[i] == stratid_num[j]) {
        sum_size[i] += size[j];
      }
    }
  }
  
  for (int i=0; i<ntot; i++) {
    lambda[i] = nsamp[i] * size[i] / sum_size[i];
  }
  
  if (arma::any(lambda >= 1)) {
    
    find_gtone = arma::find(lambda >= 1);
    find_ltone = arma::find(lambda < 1);
    
    
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
    
    stratid_ltone = stratid(find_ltone);
    nsamp_ltone = nsamp(find_ltone);
    prn_ltone = prn(find_ltone);
    size_ltone = size(find_ltone);
    id_ltone = id(find_ltone);
    
    n_gtone = id_gtone.size();
    lambda_gtone = arma::ones(n_gtone);
    Q_gtone = arma::zeros(n_gtone);
    
    for (int i=0; i<n_gtone; i++) {
      sampled_gtone[i] = TRUE;
    }
    int n_ltone = id_ltone.size();
    int n_gtone = id_gtone.size();
    
    for (int i=0; i<n_ltone; i++) {
      for (int j=0; j<n_gtone; j++) {
        if (stratid_ltone[i] == stratid_gtone[j]) {
          nsamp_ltone[i]--;
        }
      }
    }
    
    Rcpp::DataFrame df_ltone = Rcpp::DataFrame::create(Rcpp::Named("stratid")=stratid_ltone, 
                                                       Rcpp::Named("nsamp")=nsamp_ltone,
                                                       Rcpp::Named("prn")=prn_ltone,
                                                       Rcpp::Named("size")=size_ltone,
                                                       Rcpp::Named("id")=id_ltone);
    
    Rcpp::DataFrame df_ltone_out = pps_cpp(df_ltone,
                                           stratid_ltone,
                                           nsamp_ltone,
                                           prn_ltone,
                                           size_ltone,
                                           id_ltone);
    
    arma::vec Q_ltone = df_ltone_out["Q"];
    arma::vec lambda_ltone = df_ltone_out["lambda"];
    Rcpp::LogicalVector sampled_ltone = df_ltone_out["sampled"];
    
    
    id_int = arma::join_cols(id_gtone, id_ltone);
    Q_int = arma::join_cols(Q_gtone, Q_ltone);
    lambda_int = arma::join_cols(lambda_gtone, lambda);
    Q_out = arma::zeros(ntot);
    lambda_out = arma::zeros(ntot);
    
    for (int i = 0; i < n_gtone; i++) {
      sampled_int[i] = sampled_gtone[i];
    }
    
    for (int i = 0; i < n_ltone; i++) {
      sampled_int[i + n_gtone] = sampled_ltone[i];
    }
    for (int i = 0; i < ntot; i++) {
      for (int j = 0; j < ntot; j++) {
        if (id[i] == id_int[j]) {
          Q_out[i] = Q_int[j];
          lambda_out[i] = lambda_int[j];
          sampled_out[i] = sampled_int[j];
        }
      }
    }
    
    dfout["lambda"] = lambda_out;
    dfout["Q"] = Q_out;
    dfout["sampled"] = sampled_out;
    
    return dfout;
  }
  
  else {
    for (int i=0; i<ntot; i++) {
      Q[i] = prn[i] * (1 - lambda[i]) / (lambda[i] * (1 - prn[i]));
    }
    
    Rcpp::DataFrame df_srs = Rcpp::DataFrame::create(Rcpp::Named("stratid") = stratid,
                                                     Rcpp::Named("nsamp") = nsamp,
                                                     Rcpp::Named("Q") = Q,
                                                     Rcpp::Named("id") = id);
    
    Rcpp::DataFrame df_out = srs_cpp(df_srs,
                                     stratid,
                                     nsamp,
                                     Q,
                                     id);
    
    Rcpp::LogicalVector sampled = df_out["sampled"];
    
    dfout["lambda"] = lambda;
    dfout["Q"] = Q;
    dfout["sampled"] = sampled;
    
    return dfout;
  }
}