source("Update_Params.R")
source("Compute_Lower_Bound.R")

#A, Y, X, gamma_init, beta_init,alpha, corpus, 
VEM <- function(epsilon = 1e-3, max_iter = 100, verbose = TRUE) {
  # Inputs:
  # A: Incidence matrix (MxP)
  # Y: Row cluster assignments
  # X: Column cluster assignments
  # pi_init, rho_init, delta_init, beta_init: Initial values of parameters
  # epsilon: Convergence threshold for Euclidean distance
  # max_iter: Maximum number of iterations
  # Initialize parameters
  if (verbose) message("Starting VEM...")
  start_time <- Sys.time()
  
  # Track lower bound
  lower_bound_prev <- -Inf
  iter <- 0
  repeat {
    iter <- iter + 1
    if (verbose) message(sprintf("VEM Iteration %d", iter))
    
    # E-step: Update q(·|gamma,phi)
    update_phi()
    update_gamma()
    
    # M-step: Update model parameters
    update_rho()
    update_delta()
    update_pi()
    update_beta()
    
    # Compute lower bound

    lower_bound <- Compute_Lower_Bound()
    LTBM_env$lower_bound_history <- c(LTBM_env$lower_bound_history, lower_bound)
    
    if (is.na(lower_bound)) {
      lower_bound <- -Inf
      if (verbose) message("WARNING: lower_bound is NA. Taking -Inf.")
    }
    
    # Check for convergence
    if (abs(lower_bound - lower_bound_prev) < epsilon || iter >= max_iter) {
      break
    }
    
    lower_bound_prev <- lower_bound
  }
  
  end_time <- Sys.time()
  if (verbose) message(sprintf("VEM completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}