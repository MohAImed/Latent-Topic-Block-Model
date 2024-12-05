setwd("C:/Users/badis/Documents/M2 Math&IA/Cours Non supervisé/Articles/Articles preparation/Article-Latent-Topic-Modeling/Code/Utils")
source("Greedy_Search.R")
source("VEM.R")

##A, Y_init, X_init, gamma_init, beta_init,alpha, corpus
LTBM <- function(epsilon = 1e-3, max_iter = 100, verbose = TRUE){
  if(verbose) message("Starting LTBM...")
  start_time <- Sys.time()
                                
  # Iterate until convergence
  for (iter in 1:max_iter) {
    # Step 1: VEM with Y and X fixed
    if(verbose) message(sprintf("Iteration %d%d", iter, max_iter))
    VEM(epsilon = 1e-3, max_iter = 100)
    
    # Step 2: Greedy search to update Y and X
    greedy_search()
  }
  
  end_time <- Sys.time()
  if (verbose) message(sprintf("LTBM completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}