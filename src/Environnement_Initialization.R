## Importing libraries
setwd("C:/Users/badis/Documents/M2 Math&IA/Cours Non supervisé/Articles/Articles preparation/Article-Latent-Topic-Modeling/Code/Utils")

library(MCMCpack)
library(blockmodels)
source("LTBM.R")
source("Initialization_Methods.R")

## THE PURPOSE OF THIS FILE IS TO:
## 1. DEFINE AND INITIALISE ALL LTBM VARIABLES
## 2. PROVIDE A SINGLE ENTRY POINT FOR MODYFIYING OR RESETTING THE ENVIRONMENT
# Clean the variables 

LTBM_env <- new.env()

# Initialize variables in the LTBM environment

LTBM_env$A <- NULL
LTBM_env$corpus <- NULL
LTBM_env$K <- NULL
LTBM_env$V <- NULL

LTBM_env$Y <- NULL
LTBM_env$X <- NULL

LTBM_env$rho <- NULL
LTBM_env$delta <- NULL
LTBM_env$pi <- NULL
LTBM_env$alpha <- NULL
LTBM_env$beta <- NULL

LTBM_env$phi <- NULL
LTBM_env$gamma <- NULL
LTBM_env$lower_bound_history <- NULL

initialize_environment <- function(A,Y,X,corpus,K,V){
  message("Initializing the LTBM variables...")
  start_time <- Sys.time()
  
  LTBM_env$lower_bound_history <- c()
  LTBM_env$A <- A
  LTBM_env$corpus <- corpus 
  LTBM_env$K <- K
  LTBM_env$V <- V
  
  ## Initialization of Y and X
  M <- nrow(A)
  P <- ncol(A)
  Q <- length(unique(Y))
  L <- length(unique(X))
  
  LTBM_env$Y <- Y
  LTBM_env$X <- X
  
  ## Initialization of model parameters
  LTBM_env$rho <- (table(LTBM_env$Y)/length(LTBM_env$Y))
  LTBM_env$delta <- (table(LTBM_env$X)/length(LTBM_env$X))
  LTBM_env$pi <- array(0,dim = c(Q,L))
  LTBM_env$alpha <- rep(1, K)
  LTBM_env$beta <- rdirichlet(n = K, alpha = rep(1, V))
  
  ## Initialization of variationnal parameters
  
  ### Init phi
  LTBM_env$phi <- list()
  for (i in 1:nrow(LTBM_env$A)){
    for (j in 1:ncol(LTBM_env$A)){
      if(LTBM_env$A[i,j] == 1){
        D_ij <- length(LTBM_env$corpus[[paste(i, j, sep = ",")]])
        for (d in 1:D_ij){
          N_ij_d <- length(LTBM_env$corpus[[paste(i, j, sep = ",")]][[d]])
          LTBM_env$phi[[paste(i, j, sep = ",")]][[d]] <- array(1, dim = c(N_ij_d, K))/K  
        }
      }
    }
  }
  
  LTBM_env$gamma <- array(1, dim = c(length(unique(Y)),length(unique(X)),K))*alpha
  
  end_time <- Sys.time()
  message(sprintf("Environnement initialized in %f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))
}

reset_environment <- function(){
  rm(list = ls(envir = LTBM_env), envir = LTBM_env)
}
