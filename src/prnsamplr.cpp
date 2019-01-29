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
  bool iterated = FALSE;
  int ntot = id.size();
  
  arma::vec stratid_num = (arma::vec) stratid;
  arma::vec strat = arma::unique(arma::sort(stratid_num));
  int nstrat = strat.size();
  
  arma::vec lambda(ntot, 1);
  
  arma::vec sum_size = arma::zeros(ntot);
  
  arma::vec Q(ntot, 1);
  
  for (int i=0; i<ntot; i++) {
    for (int j=0; j<ntot; j++) {
      for (int k=0; k<ntot; k++) {
        if (stratid_num[j] == strat[k]) {
          sum_size[i] += size[j];
        }
      }
    }
  }
  
  for (int i=0; i<ntot; i++) {
    lambda[i] = nsamp[i] * size[i] / sum_size[i];
  }
  
  if (arma::any(lambda >= 1)) {
    iterated = TRUE;
    
    arma::vec stratid_gtone = stratid(arma::find(lambda >= 1));
    arma::vec nsamp_gtone = nsamp(arma::find(lambda >= 1));
    arma::vec prn_gtone = prn(arma::find(lambda >= 1));
    arma::vec size_gtone = size(arma::find(lambda >= 1));
    arma::vec id_gtone = id(arma::find(lambda >= 1));
    arma::vec lambda_gtone = arma::ones(id_gtone.size());
    arma::vec Q_gtone = arma::zeros(id_gtone.size());
    
    arma::vec sampled_gtone_n = arma::ones(id_gtone.size());
    arma::ivec sampled_gtone = arma::conv_to<arma::ivec>::from(sampled_gtone_n);
    
    arma::vec stratid_ltone = stratid(arma::find(lambda < 1));
    arma::vec nsamp_ltone = nsamp(arma::find(lambda < 1));
    arma::vec prn_ltone = prn(arma::find(lambda < 1));
    arma::vec size_ltone = size(arma::find(lambda < 1));
    arma::vec id_ltone = id(arma::find(lambda < 1));
    
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
    
    Rcpp::DataFrame df_ltone_out = pps_cpp(df=df_ltone,
                                           stratid=stratid_ltone,
                                           nsamp=nsamp_ltone,
                                           prn_ltone,
                                           size_ltone,
                                           id_ltone);
    
    arma::vec Q_ltone = df_ltone_out["Q"];
    arma::vec lambda_ltone = df_ltone_out["lambda"];
    arma::ivec sampled_ltone = df_ltone_out["sampled"];
    
    arma::vec stratid_out = arma::join_cols(stratid_gtone, stratid_ltone);
    arma::vec nsamp_out = arma::join_cols(nsamp_gtone, nsamp_ltone);
    arma::vec prn_out = arma::join_cols(prn_gtone, prn_ltone);
    arma::vec size_out = arma::join_cols(size_gtone, size_ltone);
    arma::vec id_out = arma::join_cols(id_gtone, id_ltone);
    arma::vec Q_out = arma::join_cols(Q_gtone, Q_ltone);
    arma::vec lambda_out = arma::join_cols(lambda_gtone, lambda_ltone);
    arma::ivec sampled_out_i = arma::join_cols(sampled_gtone, sampled_ltone);
    
    Rcpp::LogicalVector sampled_out;
    for (int i = 0; i < ntot; i++) {
      sampled_out[i] = sampled_out_i[i] == 1;
    }
    
    Rcpp::DataFrame df_out = Rcpp::DataFrame::create(Rcpp::Named("stratid")=stratid_out,
                                                     Rcpp::Named("nsamp")=nsamp_out,
                                                     Rcpp::Named("prn")=prn_out,
                                                     Rcpp::Named("size")=size_out,
                                                     Rcpp::Named("id")=id_out,
                                                     Rcpp::Named("Q")=Q_out,
                                                     Rcpp::Named("lambda")=lambda_out,
                                                     Rcpp::Named("sampled")=sampled_out);
    
    return df_out;
  }
  
  else {
    for (int i=0; i<ntot; i++) {
      Q[i] = prn[i] * (1 - lambda[i]) / (lambda[i] * (1 - prn[i]));
    }
    
    arma::ivec sampled_int(ntot, 1);
    Rcpp::LogicalVector sampled_bool(ntot, 1);
    
    arma::ivec sampled_sorted(ntot, 1);
    arma::vec strat_tot(nstrat+1, 1);
    
    arma::vec id_sorted = id(arma::sort_index(Q));
    arma::vec nsamp_sorted = nsamp(arma::sort_index(Q));
    arma::vec stratid_sorted = stratid(arma::sort_index(Q));
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
            sampled_sorted[j] = 1;
          }
          else {
            sampled_sorted[j] = 0;
          }
        }
      }
    }
    
    
    for (int i = 0; i < ntot; i++) {
      for (int j = 0; j < ntot; j++) {
        if (id[i] == id_sorted_st[j]) {
          sampled_int[i] = sampled_sorted[j]; 
          sampled_bool[i] = sampled_sorted[j] == 1;
        }
      }
    }
    
    df["Q"] = Q;
    df["lambda"] = lambda;
    if (iterated) {
      df["sampled"] = sampled_int;
    }
    else {
      df["sampled"] = sampled_bool;
    }
    return df;
  }
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