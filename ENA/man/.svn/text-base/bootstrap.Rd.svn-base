\name{bootstrap}
\alias{bootstrap}
\title{Bootstrap the reconstruction of a network from a dataset}
\usage{
  bootstrap(data, fun, sample.percentage = 0.7,
    iterations = 150, cluster, truth)
}
\arguments{
  \item{data}{The dataset to reconstruct. Each column
  should contain one sample, and each row should contain
  one gene.}

  \item{fun}{The network reconstruction technique to employ
  while bootstrapping. Could be one of the provided methods
  such as \dQuote{\code{buildSpace}} or a custom function.
  Provide the name of the function in quotes.}

  \item{sample.percentage}{The percentage of samples to
  select for each iteration.}

  \item{iterations}{The number of bootstrapping iterations
  to perform -- i.e. the number of networks to build}

  \item{cluster}{Optionally provide an RMPI cluster (of
  class MPIcluster) to distribute the workload across.}

  \item{truth}{The true network structure. Typically not
  available, but useful in testing and debugging.}
}
\value{
  A data.frame representing the adjacency list of the
  ENA-produced network.
}
\description{
  Randomly selects a subset of the avaialble samples and
  performs a network reconstruction using the selected
  technique. Aggregate all produced networks into a single
  network using the \code{ena} method.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

