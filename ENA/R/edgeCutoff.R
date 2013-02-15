#' Compute a binary network from a continuous rank-product network.
#' 
#' @param rp The rank product of the network you wish to binarize. This can either
#' be the adjacency matrix of the network, or just the upper triangle of that 
#' network, as could be computed by the \code{\link{ena}} function.
#' @param nNets The number of networks used to compute the given rank product
#' @param pfp The percentage of false positives to use as a cutoff
#' @return The binarized network in which only edges surpassing the specified 
#' significance level are maintained. If \code{rp} was provided as a named matrix, the
#' results will also be a matrix. If \code{rp} was a vector of the upper triangle,
#' the result will also be a vector of the upper triangle. Note that an upper triangle
#' can be converted back to an adjacency matrix using \code{\link{tri2mat}}.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
edgeCutoff <- function(rp, nNets, pfp=0.05, nPerm=100){
	names <- NULL
	if(is.matrix(rp)){
		names <- colnames(rp)
		rp <- rp[upper.tri(rp)]
	}
	
	n <- length(rp)
	pena <- NULL
	for (i in 1:nPerm){
		perm <- matrix(runif(n*nNets, 0, 1), ncol=nNets)
		pena <- c(pena,ena(perm))
	}
	
	count <- apply(sapply(rp, "<", pena), 2, sum)
	count <- count / nPerm
	
	fp <- count/rank(1-rp)
	
	if (!is.null(names)){
		return(tri2mat(names, fp <= pfp))	
	}	
	fp <= pfp
}