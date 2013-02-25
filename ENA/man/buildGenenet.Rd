\name{buildGenenet}
\alias{buildGenenet}
\title{Reconstruct network using GeneNet}
\usage{
  buildGenenet(data)
}
\arguments{
  \item{data}{The matrix on which to reconstruct. The
  matrix should store one gene per row, and one sample per
  column.}
}
\value{
  The adjacency matrix of the genes provided.
}
\description{
  Reconstructs a gene regulatory network using the GeneNet
  algorithm
}
\examples{
#Load in the sample PPI data provided with this package
data(PPI)
#Simulate the network based on one of the adjacency lists just loaded.
net <- simulateNetwork(net44)
#Reconstruct the network using GeneNet, then grab the upper traingular portion
# of the matrix
gn <- abs(buildGenenet(net))
gn <- gn[upper.tri(gn)]
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

