# LAB-04(SPII)

# Define probabilities for the first generation
P0 <- 1/2
P1 <- 1/4
P2 <- 1/4

# a)
# Load the library
library(igraph)

# Create a tree diagram for the first two generations
tree_edges <- c("G0", "G1_P0", "G0", "G1_P1", "G0", "G1_P2", 
                "G1_P0", "G2_P0_0", "G1_P0", "G2_P0_1", "G1_P0", "G2_P0_2",
                "G1_P1", "G2_P1_0", "G1_P1", "G2_P1_1", "G1_P1", "G2_P1_2",
                "G1_P2", "G2_P2_0", "G1_P2", "G2_P2_1", "G1_P2", "G2_P2_2")

# Create a graph
tree_graph <- graph(edges = tree_edges, directed = TRUE)

# Plot the tree diagram
plot(tree_graph, vertex.size = 25, vertex.label.cex = 0.8, edge.arrow.size = 0.5, 
     main = "Tree Diagram for First Two Generations")


# b) Mean and Variance of 2nd generation
mean <- (0 * P0 + 1 * P1 + 2 * P2)^2
mean

variance <- (0 - mean)^2 * P0 + (1 - mean)^2 * P1 + (2 - mean)^2 * P2 + ((0 * P0 + 1 * P1 + 2 * P2)^2)
variance

# print (Additional Part)
cat("Mean of 2nd generation: ", mean, "\n")
cat("Variance of 2nd generation: ", variance, "\n")

# c) Extinction probability
extinction_prob <- function(q) {
  return(P0 + P1 * q + P2 * q^2 - q)
}

# Solving extinction probability equation
q_solution <- uniroot(extinction_prob, c(0, 1))$root
cat("Extinction probability: ", q_solution, "\n")

