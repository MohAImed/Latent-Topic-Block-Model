## Utility functions

# p(Y|rho) # Y,rho
p_Y__rho <- function(){
  return(prod(LTBM_env$rho[LTBM_env$Y]))
}

# p(X|delta) #X, delta
p_X__delta <- function(){
  return(prod(LTBM_env$delta[LTBM_env$X]))
}

# p(A|Y,X,pi) #A,Y,X,pi
p_A__Y_X_pi <- function(){
  prod = 1
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      prob <- LTBM_env$pi[LTBM_env$Y[i], LTBM_env$X[j]]
      prod = prod * dbinom(A[i,j],1,prob)
    }
  }
  return(prod)
}

## The function that we will use
# A,Y,X,pi,rho,delta

p_A_Y_X__pi_rho_delta <- function(){
  ## Inputs :
  # A: incidence matrix of dimension MxP
  # Y: cow clusters vector of dimension M
  # X: col clusters vector of dimension P
  # pi: probability matrix of dimension QxL
  # rho: 
  # delta: 
  return(p_A__Y_X_pi()*p_Y__rho()*p_X__delta())
}