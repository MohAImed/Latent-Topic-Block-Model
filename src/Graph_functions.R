## The function plot_connection_density

plot_connection_density <- function(A, row_clusters, col_clusters, Q, L, topic_colors) {
  # Step 1: Reorganize rows and columns by their cluster assignments
  row_order <- order(row_clusters)  # Order rows by clusters
  col_order <- order(col_clusters)  # Order columns by clusters
  reordered_A <- A[row_order, col_order]
  
  # Step 2: Compute cluster boundaries
  row_bounds <- cumsum(table(factor(row_clusters, levels = 1:Q)))
  col_bounds <- cumsum(table(factor(col_clusters, levels = 1:L)))
  
  # Step 3: Adjust dimensions for image()
  x <- seq(1, ncol(reordered_A) + 1)
  y <- seq(1, nrow(reordered_A) + 1)
  z <- reordered_A  # The matrix to visualize
  
  # Step 4: Plot the incidence matrix
  image(x - 0.5, y - 0.5, t(z), col = c("white", "black"), axes = FALSE,
        main = "Reorganized Incidence Matrix", xlab = "Column-Clusters (Objects)", ylab = "Row-Clusters (Individuals)")
  
  # Step 5: Overlay grid lines for cluster boundaries
  par(xpd = FALSE)  # Restrict drawing within plot region
  for (r in row_bounds[-length(row_bounds)]) {
    abline(h = r + 0.5, col = "red", lwd = 2)
  }
  for (c in col_bounds[-length(col_bounds)]) {
    abline(v = c + 0.5, col = "blue", lwd = 2)
  }
  
  # Step 6: Annotate cluster boundaries
  axis(1, at = col_bounds - 0.5, labels = 1:L, tick = FALSE, las = 1)  # Column labels
  axis(2, at = row_bounds - 0.5, labels = 1:Q, tick = FALSE, las = 2)  # Row labels
}

## With topics colors

plot_connection_density_with_topics_overlay <- function(A, row_clusters, col_clusters, Q, L, theta, topic_colors) {
  # Step 1: Reorganize rows and columns by their cluster assignments
  row_order <- order(row_clusters)  # Order rows by clusters
  col_order <- order(col_clusters)  # Order columns by clusters
  reordered_A <- A[row_order, col_order]
  
  # Step 2: Compute cluster boundaries
  row_bounds <- cumsum(table(factor(row_clusters, levels = 1:Q)))
  col_bounds <- cumsum(table(factor(col_clusters, levels = 1:L)))
  
  # Step 3: Compute dominant topics for each cluster pair
  dominant_topics <- matrix(NA, nrow = Q, ncol = L)
  for (q in 1:Q) {
    for (l in 1:L) {
      dominant_topics[q, l] <- which.max(theta[q, l, ])  # Find dominant topic
    }
  }
  
  # Step 4: Plot the base incidence matrix
  image(1:ncol(reordered_A), 1:nrow(reordered_A), t(reordered_A[nrow(reordered_A):1, ]),
        col = c("white", "black"), axes = FALSE,
        main = "Reorganized Incidence Matrix with Topic Colors", xlab = "Column-Clusters (Objects)", ylab = "Row-Clusters (Individuals)")
  
  # Step 5: Superpose topic-colored blocks
  for (q in 1:Q) {
    row_start <- ifelse(q == 1, 1, row_bounds[q - 1] + 1)
    row_end <- row_bounds[q]
    for (l in 1:L) {
      col_start <- ifelse(l == 1, 1, col_bounds[l - 1] + 1)
      col_end <- col_bounds[l]
      
      # Get the dominant topic color
      topic_color <- topic_colors[dominant_topics[q, l]]
      
      # Draw a semi-transparent rectangle over the block
      rect(xleft = col_start - 1, ybottom = nrow(reordered_A) - row_end, 
           xright = col_end, ytop = nrow(reordered_A) - row_start + 1, 
           col = adjustcolor(topic_color, alpha.f = 0.2), border = NA)
    }
  }
  
  # Step 6: Overlay grid lines for cluster boundaries
  par(xpd = FALSE)
  for (r in row_bounds[-length(row_bounds)]) {
    abline(h = nrow(reordered_A) - r, col = "red", lwd = 2)
  }
  for (c in col_bounds[-length(col_bounds)]) {
    abline(v = c, col = "blue", lwd = 2)
  }
  
  # Step 7: Add axes and labels
  axis(1, at = (col_bounds - 0.5), labels = 1:L, tick = FALSE, las = 1)  # Column labels
  axis(2, at = nrow(reordered_A) - (row_bounds - 0.5), labels = 1:Q, tick = FALSE, las = 2)  # Row labels (flipped)
  
  # Step 8: Add legend
  #legend("topright", legend = paste("Topic", 1:length(topic_colors)), fill = adjustcolor(topic_colors, alpha.f = 0.2), title = "Topics", cex = 0.8)
}
