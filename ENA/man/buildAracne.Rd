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
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

