\name{buildAracne}
\alias{buildAracne}
\title{Reconstruct network using Aracne}
\usage{
buildAracne(mat)
}
\arguments{
  \item{mat}{The matrix on which to reconstruct. The matrix
  should store one gene per row, and one sample per
  column.}
}
\value{
The adjacency matrix of the genes provided.
}
\description{
Reconstructs a gene regulatory network using the Aracne
algorithm
}
\examples{
#Load in the sample PPI data provided with this package
data(PPI)
#Simulate the network based on one of the adjacency lists just loaded.
net <- simulateNetwork(net44)
#Reconstruct the network using GeneNet, then grab the upper traingular portion
# of the matrix
ar <- abs(buildAracne(net))
ar <- ar[upper.tri(ar)]
}
\author{
Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

