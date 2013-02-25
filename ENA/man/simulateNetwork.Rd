\name{simulateNetwork}
\alias{simulateNetwork}
\title{Simulate a gene expression dataset.}
\usage{
  simulateNetwork(adjList,
    genes = sort(union(adjList[, 1], adjList[, 2])),
    samples = 100, noise = 1)
}
\arguments{
  \item{adjList}{the adjacency list of the matrix you're
  looking to simulate. The first column should be the
  source and the second column the target. To avoid any
  confusion, we require that the columns be named exactly
  "Source" and "Target".}

  \item{genes}{The list of all genes in the network. By
  default this is any gene mentioned in the adjacency
  list.}

  \item{samples}{The number of samples you wish to
  simulate}

  \item{noise}{the amount of noise present in the simulated
  expression levels.}
}
\description{
  Simulates the observed gene expression levels in a
  dataset using the underlying truth network provided,
  allowing cusomtization of the number of samples and the
  noise levels in the dataset.
}
\examples{
#Load in the sample PPI data provided with this package
data(PPI)
#Simulate the network based on one of the adjacency lists just loaded.
net <- simulateNetwork(net44)
}
\author{
  Guanghua Xiao \email{Guanghua.Xiao@UTSouthwestern.edu}
}

