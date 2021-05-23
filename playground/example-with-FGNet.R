# https://bioconductor.org/packages/release/bioc/vignettes/FGNet/inst/doc/FGNet.html

library(FGNet)
library(netplot)
library(igraph)

# Getting the data
data(FEA_tools)

jobID <- 1639610
feaAlzheimer <- fea_gtLinker_getResults(jobID=jobID, organism="Hs")

incidMat <- fea2incidMat(feaAlzheimer)

genesAlz <- rownames(incidMat$metagroupsMatrix)
genesAlzExpr <- setNames(c(rep(1,50), rep(-1,27)), genesAlz)

# Plotting with igraph
fNw <- functionalNetwork(incidMat, geneExpr=genesAlzExpr, keepColors=FALSE, vLabelCex=0.5)

# Calling netplot
# frameCol <- incidMat$metagroupsMatrix
# frameCol <-
frameCol <- apply(incidMat$metagroupsMatrix, 1, which.max)

nplot(
  fNw$iGraph$commonClusters,
  vertex.label       = V(fNw$iGraph$commonClusters)$name,
  vertex.label.show  = 1,
  vertex.size.range  = c(.03, .03),
  edge.curvature     = .5,
  vertex.frame.prop  = .5,
  vertex.label.range = c(10,10),
  vertex.color       = "gray",
  vertex.frame.color = grDevices::hcl.colors(max(frameCol), "Inferno")[frameCol],
  vertex.nsides      = frameCol+2,
  bg.col             = "transparent",
  edge.arrow.size    = .03
  )
