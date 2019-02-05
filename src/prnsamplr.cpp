// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// First draft



// [[Rcpp::export]]
Rcpp::DataFrame srs_cpp(const Rcpp::DataFrame& dfin,
                        arma::vec stratid,
                        arma::vec nsamp,
                        arma::vec prn,
                        arma::vec id){
  
  Rcpp::DataFrame dfout = dfin;
  
  uword ntot = id.size();
  uvec strat = find_unique(sort(stratid));
  uword nstrat = strat.size();
  uvec sampled(ntot, 1);
  uvec sampled_sorted(ntot, 1);
  uvec strat_tot(nstrat+1, 1);
  
  //std::cout << 'frukt' << prn << 'blorg \n';
  
  vec id_sorted = id(sort_index(prn));
  vec nsamp_sorted = nsamp(sort_index(prn));
  vec stratid_sorted = stratid(sort_index(prn));
  vec id_sorted_st = id_sorted(stable_sort_index(stratid_sorted));
  vec nsamp_sorted_st = nsamp_sorted(stable_sort_index(stratid_sorted));
  
  strat_tot[0] = 0;
  for (uword i = 1; i <= nstrat; i++) {
    strat_tot[i] = strat_tot[i-1] + sum(stratid == stratid[strat[i-1]]);
  }
  
  for (uword i = 0; i < nstrat; i++) {
    for (uword j = strat_tot[i]; j < strat_tot[i+1]; j++) {
      if (stratid[j] == stratid[strat[i]]) {
        uword ind_strat = j - strat_tot[i];
        if (ind_strat < nsamp_sorted_st[j]) {
          sampled_sorted[j] = 1;
        }
        else {
          sampled_sorted[j] = 0;
        }
      }
    }
  }
  
  
  for (uword i = 0; i < ntot; i++) {
    /*
    for (uword j = 0; j < ntot; j++) {
      if (id[i] == id_sorted_st[j]) {
        sampled[i] = sampled_sorted[j];
      }
    }
    */
    
    sampled[i] = sum(sampled_sorted.elem(find(id_sorted_st==id[i])));
  }
  
  dfout["sampled"] = sampled;
  return Rcpp::DataFrame(dfout);
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
  
    
  uword ntot = id.size();
  vec stratid_num;
  vec strat;
  vec lambda;
  vec sum_size;
  vec Q;
  
  uvec sampled_gtone;
  vec stratid_gtone;
  vec nsamp_gtone;
  vec prn_gtone;
  vec size_gtone;
  vec id_gtone;
  
  vec stratid_old;
  vec nsamp_old;
  vec prn_old;
  vec size_old;
  vec id_old;
  
  vec lambda_gtone;
  vec Q_gtone;
  uword n_gtone = 0;
  uword n_ltone;
  
  vec strat_ltone;
  vec stratid_ltone;
  vec nsamp_ltone;
  vec prn_ltone;
  vec size_ltone;
  vec id_ltone;
  vec sum_size_ltone;
  vec strat_tot_ltone;
  
  vec strat_tot;
  vec id_sorted;
  vec nsamp_sorted;
  vec stratid_sorted;
  vec id_sorted_st;
  vec nsamp_sorted_st;
  
  vec id_int;
  vec Q_int;
  vec Q_out;
  vec lambda_int;
  vec lambda_out;
  uvec sampled_int(ntot, 1);
  uvec sampled_out(ntot, 1);
  
  uvec find_ltone;
  uvec find_gtone;
  
  
  stratid_num = (vec) stratid;
  strat = unique(sort(stratid_num));
  lambda = zeros(ntot);
  sum_size = zeros(ntot);
  Q = zeros(ntot);
  //uword nstrat = strat.size();
  
  
  for (uword i=0; i<ntot; i++) {
    /*
    for (uword j=0; j<ntot; j++) {
      if (stratid_num[i] == stratid_num[j]) {
        sum_size[i] += size[j];
      }
    }
     */
    
    sum_size[i] = sum(size.elem(find(stratid == stratid[i])));
    
  }
  
  
  
  
  /*
  for (uword i=0; i<ntot; i++) {
    lambda[i] = nsamp[i] * size[i] / sum_size[i];
  }
   */
  lambda = nsamp % size / sum_size;
  
  if (any(lambda >= 1)) {
    
    find_gtone = find(lambda >= 1);
    find_ltone = find(lambda < 1);
    
    /*
    stratid_old = stratid_gtone;
    nsamp_old = nsamp_gtone;
    prn_old = prn_gtone;
    size_old = size_gtone;
    id_old = id_gtone;
    */
    
    //stratid_gtone = join_cols(stratid_old, stratid.elem(find_gtone));
    //nsamp_gtone = join_cols(nsamp_old, nsamp.elem(find_gtone));
    //prn_gtone = join_cols(prn_old, prn.elem(find_gtone));
    //size_gtone = join_cols(size_old, size.elem(find_gtone));
    //id_gtone = join_cols(id_old, id.elem(find_gtone));
    
    stratid_ltone = stratid(find_ltone);
    nsamp_ltone = nsamp(find_ltone);
    prn_ltone = prn(find_ltone);
    size_ltone = size(find_ltone);
    id_ltone = id(find_ltone);
    
    n_gtone = find_gtone.size();
    n_ltone = id_ltone.size();
    
    lambda_gtone = ones(n_gtone);
    Q_gtone = zeros(n_gtone);
    sampled_gtone = ones<uvec>(n_gtone);
    
    /*
    for (uword i=0; i<n_gtone; i++) {
      sampled_gtone[i] = 1;
    }
     */
    
    /*
    uword n_ltone = id_ltone.size();
    uword n_gtone = id_gtone.size();
    */
    
    /*
    for (uword i=0; i<n_ltone; i++) {
      for (uword j=0; j<n_gtone; j++) {
        if (stratid_ltone[i] == stratid_gtone[j]) {
          nsamp_ltone[i]--;
        }
      }
    }
     */
    
    
    for (uword i=0; i<n_ltone; i++) {
      nsamp_ltone[i] -= sum(stratid_gtone == stratid_ltone[i]);
    }
    
    //nsamp_ltone -= sum(stratid_ltone == stratid_gtone);
    
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
    
    vec Q_ltone = df_ltone_out["Q"];
    vec lambda_ltone = df_ltone_out["lambda"];
    uvec sampled_ltone = df_ltone_out["sampled"];
    
    
    id_int = join_cols(id_gtone, id_ltone);
    Q_int = join_cols(Q_gtone, Q_ltone);
    lambda_int = join_cols(lambda_gtone, lambda_ltone);
    sampled_int = join_cols(sampled_gtone, sampled_ltone);
    Q_out = zeros(ntot);
    lambda_out = zeros(ntot);
    sampled_out = zeros<uvec>(ntot);
    
    /*
    for (uword i = 0; i < n_gtone; i++) {
      sampled_int[i] = sampled_gtone[i];
    }
    
    for (uword i = 0; i < n_ltone; i++) {
      sampled_int[i + n_gtone] = sampled_ltone[i];
    }
     */
    
    for (uword i = 0; i < ntot; i++) {
      /*for (uword j = 0; j < ntot; j++) {
        if (id[i] == id_int[j]) {
          Q_out[i] = Q_int[j];
          lambda_out[i] = lambda_int[j];
          sampled_out[i] = sampled_int[j];
        }
      }*/
      
      
      Q_out[i] = sum(Q_int.elem(find(id_int==id[i])));
      lambda_out[i] = sum(lambda_int.elem(find(id_int==id[i])));
      sampled_out[i] = sum(sampled_int.elem(find(id_int==id[i])));
    }
    
    
    dfout["lambda"] = lambda_out;
    dfout["Q"] = Q_out;
    dfout["sampled"] = sampled_out;
    
    return dfout;
  }
  
  else {
    //for (uword i=0; i<ntot; i++) {
    Q = prn % (1 - lambda) / (lambda % (1 - prn));
    //}
    
    
    
    Rcpp::DataFrame df_srs = Rcpp::DataFrame::create(Rcpp::Named("stratid") = stratid,
                                                     Rcpp::Named("nsamp") = nsamp,
                                                     Rcpp::Named("Q") = Q,
                                                     Rcpp::Named("id") = id);
    
    Rcpp::DataFrame df_out = srs_cpp(df_srs,
                                     stratid,
                                     nsamp,
                                     Q,
                                     id);
    
    uvec sampled = df_out["sampled"];
    
    dfout["lambda"] = lambda;
    dfout["Q"] = Q;
    dfout["sampled"] = sampled;
    
    return Rcpp::DataFrame(dfout);
  }
}