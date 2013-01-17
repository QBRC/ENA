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
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

