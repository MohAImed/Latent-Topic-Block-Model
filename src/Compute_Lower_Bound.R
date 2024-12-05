source("Connection_Functions.R")
library(logOfGamma)

##Compute Lower bound

#gamma,phi,A,Y,X,beta,pi,rho,delta,alpha,corpus

Compute_Lower_Bound <- function(){
  Q <- length(unique(LTBM_env$Y))
  L <- length(unique(LTBM_env$X))
  ## Variational Lower Bound
  
  # OUTPUTS: the value of the lower bound evaluated at 
  Term_1 <- 0
  Term_2 <- 0
  Term_3 <- 0
  Term_4 <- 0
  Term_5 <- 0
  
  #Precompute digamma and gammaln for gamma
  digamma_gamma <- digamma(LTBM_env$gamma)
  digamma_gamma_sum <- digamma(rowSums(LTBM_env$gamma, dims = 2))
  gammaln_gamma <- gammaln(LTBM_env$gamma)
  gammaln_gamma_sum <- gammaln(rowSums(LTBM_env$gamma, dims = 2))
  
  for (i in 1:nrow(LTBM_env$A)){
    for (j in 1:ncol(LTBM_env$A)){
      if (LTBM_env$A[i,j] == 1){
        q = LTBM_env$Y[i];l = LTBM_env$X[j]; D_ij <- length(LTBM_env$corpus[[paste(i, j,sep = ",")]])
        
        docs <- LTBM_env$corpus[[paste(i, j, sep = ",")]]
        phi_docs <- LTBM_env$phi[[paste(i, j, sep = ",")]]
        
        # Process each document
        for (d in 1:D_ij) {
          words <- docs[[d]]
          phi <- phi_docs[[d]]
          
          # Term 1
          log_beta <- log(LTBM_env$beta[, words, drop = FALSE])  # Log-beta for the current document words
          Term_1 <- Term_1 + sum(t(phi) * log_beta)
          
          # Term 2
          digamma_diff <- digamma_gamma[q, l, ] - digamma_gamma_sum[q, l]
          Term_2 <- Term_2 + sum(phi * digamma_diff)
          
          # Term 4
          Term_4 <- Term_4 + sum(phi * log(phi))
      }
    }
  }
  }
  
  for (q in 1:Q){
    for (l in 1:L){
      
      gamma_ql <- LTBM_env$gamma[q, l, ]
      digamma_diff <- digamma_gamma[q, l, ] - digamma_gamma_sum[q, l]
      
      # Term 3: Prior contribution
      Term_3 <- Term_3 + gammaln(sum(LTBM_env$alpha)) - sum(gammaln(LTBM_env$alpha)) + sum((LTBM_env$alpha - 1) * digamma_diff)
      
      # Term 5: Gamma posterior entropy
      Term_5 <- Term_5 + gammaln(sum(gamma_ql)) - sum(gammaln(gamma_ql)) + sum((gamma_ql - 1) * digamma_diff)
    }
  }
  
  Variationnal_Lower_bound = Term_1 + Term_2 + Term_3 - Term_4 - Term_5
  
  return(Variationnal_Lower_bound + p_A_Y_X__pi_rho_delta())
}