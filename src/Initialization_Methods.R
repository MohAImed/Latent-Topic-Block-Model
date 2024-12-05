## Random initialization for Y and X
random_initialization <- function(M,P,Q,L){
  # M: Number of rows
  # P: Number of columns
  # Q: Number of row clusters
  # L: Number of column clusters
  
  Y <- sample(1:Q, M, replace = TRUE) # Random row cluster assignments
  X <- sample(1:L, P, replace = TRUE) # Random column cluster assignments
  return(list(Y = Y, X = X))
}

## Kmeans initialization of Y and X
kmeans_initialization <- function(A,Q,L){
  # A: Incidence matrix (M x P)
  # Q: Number of row clusters
  # L: Number of column clusters
  
  # Apply k-means to rows
  kmeans_rows <- kmeans(A, centers = Q, nstart = 10) # Row clustering
  Y <- kmeans_rows$cluster
  
  # Apply k-means to columns
  kmeans_cols <- kmeans(t(A), centers = L, nstart = 10) # column clustering
  X <- kmeans_cols$cluster
  
  return(list(Y = Y, X = X))
}

## Latent Block Model Initialization for Y and X
lbm_initialization <- function(A) {
  library(blockmodels)  # For Latent Block Model (LBM)
  
  # Fit LBM to the incidence matrix
  model <- BM_bernoulli("LBM",A)
  model$estimate()
  
  row_memberships <- model$memberships[[which.max(model$ICL)]]$Z1
  col_memberships <- model$memberships[[which.max(model$ICL)]]$Z2
  
  row_clusters <- apply(row_memberships, 1, which.max)
  col_clusters <- apply(col_memberships, 1, which.max)

  return(list(Y = row_clusters, X = col_clusters))
}