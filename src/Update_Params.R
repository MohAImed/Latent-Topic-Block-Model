## Functions for parameter's updates

# E-step

### Update phi
#gamma,A,Y,X,beta,corpus
update_phi <- function(verbose = TRUE){
  # INPUTS:
  ## gamma:
  ## A:
  ## Y:
  ## X:
  ## beta:
  ## corpus:
  if (verbose) message("Starting parameter phi update...")
  
  start_time <- Sys.time()
  for (i in 1:nrow(LTBM_env$A)){
    for (j in 1:ncol(LTBM_env$A)){
      if (LTBM_env$A[i, j] == 1) {
        q <- LTBM_env$Y[i]
        l <- LTBM_env$X[j]
        D_ij <- length(LTBM_env$corpus[[paste(i, j, sep = ",")]])
        
        # Pré-calculer les termes exponentiels pour tous les k
        expo_comp <- exp(digamma(LTBM_env$gamma[q, l, ]) - digamma(sum(LTBM_env$gamma[q, l, ])))
        
        for (d in 1:D_ij) {
          corpus_doc <- LTBM_env$corpus[[paste(i, j, sep = ",")]][[d]]
          N_ij_d <- length(corpus_doc)
          word_indices <- unlist(corpus_doc)
          
          # Utiliser une matrice pour les calculs sur les mots
          beta_selected <- LTBM_env$beta[, word_indices, drop = FALSE]  # Matrice \(K \times N_{ij,d}\)
          phi_temp <- beta_selected * matrix(expo_comp, nrow = LTBM_env$K, ncol = N_ij_d, byrow = TRUE)
          phi_temp <- t(phi_temp) / colSums(phi_temp)  # Normalisation
          LTBM_env$phi[[paste(i, j, sep = ",")]][[d]] <- phi_temp
        }
      }
    }
  }
  
  end_time <- Sys.time()
  if (verbose) message(sprintf("Parameters updates completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}

### Update gamma
#phi,A,Y,X,beta,alpha,corpus, 
update_gamma <- function(verbose = TRUE){
  #INPUTS:
  ##phi:
  ##A:
  ##Y:
  ##X:
  # OUTPUTS
  if (verbose) message("Starting parameter gamma update...")
  
  start_time <- Sys.time()
  for(i in 1:nrow(LTBM_env$A)){
    for(j in 1:ncol(LTBM_env$A)){
      if(LTBM_env$A[i,j] == 1){
        q <- LTBM_env$Y[i];l <- LTBM_env$X[j];D_ij <- length(LTBM_env$corpus[[paste(i,j,sep = ",")]])
        for(d in 1:D_ij){
          phi_doc <- LTBM_env$phi[[paste(i, j, sep = ",")]][[d]]
          LTBM_env$gamma[q,l,] <- LTBM_env$gamma[q,l,] + colSums(phi_doc)
        }
      }
    }
  }
  
  end_time <- Sys.time()
  if (verbose) message(sprintf("Parameters updates completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}

## M-Step
#phi,A,corpus,K,V
update_beta <- function(verbose = TRUE){
  if (verbose) message("Starting parameter beta update...")
  start_time <- Sys.time()
  #INPUTS:
  ## phi 
  ## A
  ## Corpus
  ## K
  ## V
  #OUTPUTS:
  for(i in 1:nrow(LTBM_env$A)){
    for(j in 1:ncol(LTBM_env$A)){
      if (LTBM_env$A[i,j] == 1){
        D_ij <- length(LTBM_env$corpus[[paste(i,j,sep = ",")]])
        for(d in 1:D_ij){
          doc_words <- LTBM_env$corpus[[paste(i,j,sep = ",")]][[d]]
          phi_doc <- LTBM_env$phi[[paste(i,j,sep = ",")]][[d]]
          
          for (k in 1:LTBM_env$K){
            #Calculate beta increment for topic k
            beta_increment <- tapply(phi_doc[,k], doc_words, sum, default = 0)
            valid_indices <- as.numeric(names(beta_increment))
            
            #Update beta
            LTBM_env$beta[k, valid_indices] <- LTBM_env$beta[k, valid_indices] + beta_increment
          }
        }
      }
    }
  }
  #Normalize beta
  LTBM_env$beta <- LTBM_env$beta / rowSums(LTBM_env$beta)
  end_time <- Sys.time()
  if (verbose) message(sprintf("Parameters updates completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}

## Update pi
# A,Y,X
update_pi <- function(verbose = TRUE){
  if (verbose) message("Starting parameter pi update...")
  start_time <- Sys.time()
  ## INPUTS:
  Q = length(unique(LTBM_env$Y));L = length(unique(LTBM_env$X))
  for(q in 1:Q){
    for(l in 1:L){
      sum_q_l <- 0
      for (i in 1:nrow(LTBM_env$A)){
        for (j in 1:ncol(LTBM_env$A)){
          if(LTBM_env$Y[i] == q & LTBM_env$X[j] == l){
            sum_q_l <- sum_q_l + 1
            if(LTBM_env$A[i,j] == 1){
              LTBM_env$pi[q,l] <- LTBM_env$pi[q,l] + 1
            }
          }
        }
      }
      LTBM_env$pi[q,l] <- LTBM_env$pi[q,l]/sum_q_l
    }
  }
  end_time <- Sys.time()
  if (verbose) message(sprintf("Parameters updates completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}

## Update rho
## Y
update_rho <- function(){
  ## INPUTS:
  LTBM_env$rho <- (table(LTBM_env$Y)/length(LTBM_env$Y))
}


## Update delta
## X
update_delta <- function(){
  ## INPUTS:
  LTBM_env$delta <- (table(LTBM_env$X)/length(LTBM_env$X))
}
