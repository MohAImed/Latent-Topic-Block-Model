source("Compute_Lower_Bound.R")

## A, Y, X, pi, rho, delta, beta, gamma, phi, 
greedy_search <- function(verbose = TRUE) {
  
  if (verbose) message("Starting Greedy Search ...")
  start_time <- Sys.time()
  M <- nrow(LTBM_env$A)
  P <- ncol(LTBM_env$A)
  Q <- length(unique(LTBM_env$Y))
  L <- length(unique(LTBM_env$X))
  
  # Update Y (row clusters)
  for (i in 1:M) {
    current_lb <- Compute_Lower_Bound()
    q <- LTBM_env$Y[i]
    if (sum(Y == q) > 1){ # i not alone in q
      final_clust <- q
      best_gain <- 0
      
      for (q_ in 1:Q){
        if(q_ != q){
          LTBM_env$Y[i] <- q_
          new_lb <- Compute_Lower_Bound()
          gain = new_lb - current_lb
          if (gain > best_gain){
            best_gain <- gain
            final_clust <- q_
          }
        }
      }
      LTBM_env$Y[i] <- final_clust
    }
  }
  
  # Update X (column clusters)
for (j in 1:P) {
    current_lb <- Compute_Lower_Bound()
    l <- LTBM_env$X[j]
    if (sum(X == l) > 1){ # i not alone in q
      final_clust <- l
      best_gain <- 0
      
      for (l_ in 1:L){
        if(l_ != l){
          LTBM_env$X[j] <- l_
          new_lb <- Compute_Lower_Bound()
          gain = new_lb - current_lb
          if (gain > best_gain){
            best_gain <- gain
            final_clust <- l_
          }
        }
      }
      LTBM_env$X[j] <- final_clust
    }
  }
  end_time <- Sys.time()
  if (verbose) message(sprintf("Greedy Search completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}