\name{adj2mat}
\alias{adj2mat}
\title{Convert an adjacency-like list (which may or may not contain all the gene
IDs in the network) into an adjacency matrix.}
\usage{
  adj2mat(adjList,
    IDs = sort(union(adjList[, 1], adjList[, 2])))
}
\arguments{
  \item{adjList}{the adjacency list of the matrix you're
  looking to simulate. There should be three columns for
  source, target, and the regulation value. To avoid any
  confusion, we require that the columns be named exactly
  "Source", "Target", and "Regulation", where "Regulation"
  represents the strength or weight of the regulation.}

  \item{IDs}{The set of genes in this network. By default,
  the set of all genes mentioned in the adjacency matrix.
  IDs can be provided if there are unconnected genes in the
  network which aren't mentioned in the adjacency list or
  when the ordering of the genes is important.}
}
\description{
  Convert an adjacency-like list (which may or may not
  contain all the gene IDs in the network) into an
  adjacency matrix.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

