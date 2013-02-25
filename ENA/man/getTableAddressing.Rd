\name{getTableAddressing}
\alias{getTableAddressing}
\title{Get the adjacency list addressing template.}
\usage{
  getTableAddressing(variableNames, truth)
}
\arguments{
  \item{variableNames}{the names of all genes to include in
  the adjacency list}

  \item{truth}{The true adjacency matrix. Often will not be
  available, but is useful for debugging and testing.}
}
\value{
  A data.frame representing the adjacency list of the
  matrix provided.
}
\description{
  Useful if you want to store the networks in their
  condensed upper-diagonal form while still having the
  benefit of convenient addressing and/or if you are using
  a simulated dataset in which you know the truth and want
  to store all the values in a single data.frame.
}
\details{
  Internal function used to get the addressing template for
  a data.frame to contain the adjacency list representation
  of a matrix.
}
\examples{
#Load in the sample Protein-Protein-Interaction data that comes with this package.
data(PPI)

#Simulate a dataset based on the 44-gene topology provided.
sim <- simulateNetwork(net44)

#Convert the true, underlying adjacency list to an adjacency matrix
trueMat <- adj2mat(net44)

#Reconstruct using GeneNet
gn <- abs(buildGenenet(sim))
gn <- gn[upper.tri(gn)]

wg <- abs(buildWgcna(sim))
wg <- wg[upper.tri(wg)]

#Aggregate all results into a single data.frame
data <- getTableAddressing(rownames(sim), trueMat)
data <- cbind(data, gn, wg)
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

