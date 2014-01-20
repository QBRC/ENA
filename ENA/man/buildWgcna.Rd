\name{buildWgcna}
\alias{buildWgcna}
\title{Reconstruct network using WGCNA}
\usage{
buildWgcna(mat)
}
\arguments{
  \item{mat}{The matrix on which to reconstruct. The matrix
  should store one gene per row, and one sample per column.
  Note that this is the transpose of how WGCNA typically
  accepts their matrix.}
}
\value{
The adjacency matrix of the genes provided.
}
\description{
Reconstructs a gene regulatory network using the WGCNA
algorithm
}
\examples{
#Load in the sample PPI data provided with this package
data(PPI)
#Simulate the network based on one of the adjacency lists just loaded.
net <- simulateNetwork(net44)
#Process with WGCNA
wg <- abs(buildWgcna(net))
}
\author{
Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

