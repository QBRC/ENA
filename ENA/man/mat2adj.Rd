\name{mat2adj}
\alias{mat2adj}
\title{Convert a matrix to an adjacency list}
\usage{
  mat2adj(adjMat)
}
\arguments{
  \item{adjMat}{The symmetric adjacency matrix with rows
  and columns named.}
}
\description{
  Takes a matrix and converts all non-zero elements to an
  adjacency list using the row/colnames as the names for
  this list. Currently, the matrix must be symmetric.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

