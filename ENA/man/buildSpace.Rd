\name{buildSpace}
\alias{buildSpace}
\title{Reconstruct network using SPACE}
\usage{
buildSpace(data)
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
Reconstructs a gene regulatory network using the SPACE
algorithm
}
\examples{
#Load in the sample PPI data provided with this package
data(PPI)
#Simulate the network based on one of the adjacency lists just loaded.
net <- simulateNetwork(net44)
#Process with SPACE
sp <- abs(buildSpace(net))
sp <- sp[upper.tri(sp)]
}
\author{
Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

