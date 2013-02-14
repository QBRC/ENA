\name{tri2mat}
\alias{tri2mat}
\title{Convert triangular elements to full matrix.}
\usage{
  tri2mat(genes, tri, diag = 1, upper = TRUE)
}
\arguments{
  \item{genes}{The names of the genes to use as row and
  column names. Note that these must be in the original
  order as was used when the traingular portion was
  extracted from the matrix. Otherwise, the matrix will not
  be constructed correctly.}

  \item{tri}{The triangular elements of the matrix. Could
  be extracted using a command like
  \code{mat[upper.tri(mat)]}}

  \item{diag}{The value to use for the diagonal elements in
  the matrix.}

  \item{upper}{\code{TRUE} if \code{tri} represents the
  upper triangular portion of a matrix, \code{FALSE} if the
  lower. \code{\link{upper.tri}} and
  \code{\link{lower.tri}} extract the elements in a
  different order.}
}
\value{
  the complete 2D matrix represented by the traingular
  portion provided.
}
\description{
  Converts the upper or lower-triangular portion of a
  matrix back to the complete 2D matrix using the gene
  names provided. The matrix is assumed to be symmetrical.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

