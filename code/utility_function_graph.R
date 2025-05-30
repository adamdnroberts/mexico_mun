library(ggplot2)

d = 0
vd = 0.5

c = -1
vc = 1.5


# Define the function
voter <- function(x, d, c, vd, vc) {
  # Calculate the left-hand side of the inequality
  lhs = -((x - d)^2 + vd^2) / (2 * vd) - (-((x - c)^2 + vc^2) / (2 * vc))
  
  # Check if the inequality is satisfied
  return(lhs <= 0)
}

# Define the dimensions of the matrix
rows <- length(seq(-1, -0.5, 0.0005))  # Number of x values
cols <- length(seq(0.5, 2.5, 0.0005)) # Number of vc values

results_matrix <- matrix(NA, nrow = rows, ncol = cols)

for (i in seq(0.5,2.5,0.0005)){
  for (x in seq(-1,-0.5,0.0005)){
    # Calculate the row and column indices for the matrix
    row_index = which(seq(-1, -0.5, 0.0005) == x)
    col_index = which(seq(0.5, 2.5, 0.0005) == i)
    
    # Store the result in the matrix
    results_matrix[row_index, col_index] <- voter(x = x, d, c, vd, vc = i)
  }
}

# Convert the matrix into a data frame for ggplot2
results_df <- expand.grid(x = seq(-1, -0.5, 0.0005), vc = seq(0.5, 2.5, 0.0005))
results_df$value <- as.vector(results_matrix)

# Plot using ggplot2
graph <- ggplot(results_df, aes(x = x, y = vc, fill = value)) +
  geom_tile() +
  scale_fill_discrete(name = "C preferred") + #  low = "blue", high = "red") +  # Customize colors
  labs(title = "", y = "C Uncertainty", x = "Voter's Ideal Point", legend.title = "D preferred") +
  theme_classic()

print(graph)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/utility_function_graph.png", plot = graph, width = 6, height = 4)

