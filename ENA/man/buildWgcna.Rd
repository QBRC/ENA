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
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

