\name{edgeCutoff}
\alias{edgeCutoff}
\title{Compute a binary network from a continuous rank-product network.}
\usage{
  edgeCutoff(rp, nNets, pfp = 0.05, nPerm = 100)
}
\arguments{
  \item{rp}{The rank product of the network you wish to
  binarize. This can either be the adjacency matrix of the
  network, or just the upper triangle of that network, as
  could be computed by the \code{\link{ena}} function.}

  \item{nNets}{The number of networks used to compute the
  given rank product}

  \item{pfp}{The percentage of false positives to use as a
  cutoff}

  \item{nPerm}{The number of permutations to run when
  calculating the pfps of the network.}
}
\value{
  The binarized network in which only edges surpassing the
  specified significance level are maintained. If \code{rp}
  was provided as a named matrix, the results will also be
  a matrix. If \code{rp} was a vector of the upper
  triangle, the result will also be a vector of the upper
  triangle. Note that an upper triangle can be converted
  back to an adjacency matrix using \code{\link{tri2mat}}.
}
\description{
  Compute a binary network from a continuous rank-product
  network.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

