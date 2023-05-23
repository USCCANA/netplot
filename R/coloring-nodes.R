# Function to extract vertex attribute from igraph object
# and use it to color the nodes in the plot
# Input: igraph object, vertex attribute name
# Output: plot of the graph with colored nodes
# TODO:
# 1. Figure out if the attribute is a factor or numeric
# 2. Add an argument to specify a color palette
# 3. Explore how to work with formulas. Ideally, we need the following
#    a. Read the formula
#    b. Capture the elements. For now, all shold be vertex attributes
#    c. Extract the vertex attributes from the graph
#    d. If it doesn't exist, throw an error using the stop() function.
# 4. Go over the netplot_edge_formulae function.
#------------------------------------------------------------
coloring_nodes <- function(graph, attribute) {
  # Extract the vertex attribute from the graph
  vertex_attribute <- V(graph)[[attribute]]
  # Create a vector of colors
  colors <- rainbow(length(unique(vertex_attribute)))
  # Assign colors to the nodes
  V(graph)$color <- colors[vertex_attribute]
  # Plot the graph
  plot(graph)
}