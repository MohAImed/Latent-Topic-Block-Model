# **Latent Topic Block Model (LTBM)**

## **Overview**
The Latent Topic Block Model (LTBM) is a probabilistic model designed for the analysis of interaction data. This model leverages latent structures, including topics and clusters, to capture and explain complex relationships within the data. The repository provides:
- Core algorithms for initializing and optimizing the LTBM.
- Methods for synthetic data generation.
- Functions for visualization and evaluation.

This implementation supports parameter estimation using Variational Expectation-Maximization (VEM) and includes a greedy search strategy for improved initialization.

---

## **Repository Structure**

```
📁 LatentTopicBlockModel/ 
│ ├── 📁 src/ # Source code 
│ ├── Compute_Lower_Bound.R # Calculates the Evidence Lower Bound (ELBO) 
│ ├── Connection_Functions.R # Manages connections and interaction matrices
| ├── Environnement_Initialization.R # Sets up the LTBM environment
| ├── Graph_functions.R # Visualization and graph-related utilities
| ├── Greedy_Search.R # Implements greedy search optimization
| ├── Initialization_Methods.R # Initialization strategies (random, K-means, LBM)
| ├── LTBM.R # Main implementation of the Latent Topic Block Model
| ├── Update_Params.R # Updates model parameters
| ├── VEM.R # Variational Expectation-Maximization 
├── 📁 docs/ # Documentation and examples
| └── SyntheticData.Rmd # Demonstration using synthetic data 
├── 📁 data/ # Placeholder for datasets
| └── README.md # Instructions for working with datasets 
├── LICENSE # Licensing information 
├── README.md # This README file 
```

---

## **Features**

1. **Synthetic Data Generation**:
   - Generate synthetic incidence matrices and associated metadata.
   - Create synthetic vocabulary and document corpora.
   - Simulate realistic interaction patterns.

2. **Model Initialization**:
   - Random initialization.
   - Clustering-based initialization (K-means and LBM).

3. **Optimization**:
   - Variational Expectation-Maximization (VEM) for parameter updates.
   - Greedy search for refined optimization.

4. **Visualization**:
   - Graph-based visualization of connections and clusters.
   - Topic overlays on incidence matrices.
   - Evolution of the lower bound during optimization.

5. **Evaluation**:
   - Compare initialization methods.
   - Analyze parameter convergence.

---

## **Getting Started**

### **Prerequisites**
- **R Version**: ≥ 4.0.0
- **Required Libraries**:
  - `ggplot2`
  - `tidyr`
  - `dplyr`
  - `reshape2`
  - Additional dependencies will be installed via `setup.R`.

---

### **Installation**
Clone the repository:
   ```bash
   git clone https://github.com/yourusername/LatentTopicBlockModel.git
   cd LatentTopicBlockModel
   ```
---
### **Usage**
1. **Synthetic Data generation**
   To generate synthetic data and test the LTBM:
   ```r
   source("src/Connection_Functions.R")
    connections <- generate_connection_matrix(M = 100, P = 80, Q = 4, L = 3, high_prob = 0.3,                     low_prob = 0.02)

   ```
2. **Initialize the Model Environment**
   ```r
   source("src/Environnement_Initialization.R")
   initialize_environment(connections$A, connections$row_clusters, connections$col_clusters, corpus, K = 3, V = length(vocab))
   ```

3. **Run LTBM Optimization**
   ```r
   source("src/LTBM.R")
   LTBM(epsilon = 1e-3, max_iter = 20)
   ```
4. **Visualize and Analyse Results**
   * Plot the evolution of the Lower Bound:
   ```r
   plot_lower_bound(LTBM_env$lower_bound_history)
   ```
   * Visualize topic overlays on incidence matrices:
   ```r
   plot_connection_density_with_topics_overlay(A, row_clusters, col_clusters, Q, L, theta, topic_colors)
   ```
---
### **Documentation**
Detailed documentation and examples are provided in `docs/SyntheticData.Rmd`.
This file includes:
* Generating synthetic data.
* Running the LTBM optimization.
* Visualizing and analyzing results.

---

### Authors

*Mohamed Badi* (mohamed.badi@etu-upsaclay.fr) and *Malek Bouzidi* (malek.bouzidi@etu-upsaclay.fr). 


### Contributing

Please feel free to submit a pull request if you'd like to add improvements or fix issues. All contributions are welcome!

### License
This project is licensed under the MIT License.
