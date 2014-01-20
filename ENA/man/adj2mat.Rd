\name{adj2mat}
\alias{adj2mat}
\title{Convert adjacency list into an adjacency matrix.}
\usage{
adj2mat(adjList, IDs = sort(union(adjList[, 1], adjList[, 2])))
}
\arguments{
  \item{adjList}{the adjacency list of the matrix you're
  looking to simulate. There should be two-three columns
  for source, target, and the (optional) regulation value.
  If the regulation value isn't specified, it's assumed to
  be 1. To avoid any confusion, we require that the columns
  be named exactly "Source", "Target", and "Regulation",
  where "Regulation" represents the strength or weight of
  the edge in the network.}

  \item{IDs}{The set of genes in this network. By default,
  the set of all genes mentioned in the adjacency matrix.
  IDs can be provided if there are unconnected genes in the
  network which aren't mentioned in the adjacency list or
  when the ordering of the genes is important.}
}
\description{
Converts an adjacency-like list (which may or may not
contain all the gene IDs in the network) into an adjacency
matrix.
}
\examples{
#Load in the sample Protein-Protein-Interaction data that comes with this package.
data(PPI)

#Simulate a dataset based on the 44-gene topology provided.
sim <- simulateNetwork(net44)

#Convert the true, underlying adjacency list to an adjacency matrix
trueMat <- adj2mat(net44)
}
\author{
Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

