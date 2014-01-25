#' Reconstruct network using SPACE
#'
#' Reconstructs a gene regulatory network using the SPACE algorithm
#' @param data The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column.
#' @return The adjacency matrix of the genes provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @examples
#' #Load in the sample PPI data provided with this package
#' data(PPI)
#' #Simulate the network based on one of the adjacency lists just loaded.
#' net <- simulateNetwork(net44)
#' #Process with SPACE
#' sp <- abs(buildSpace(net))
#' sp <- sp[upper.tri(sp)]
#' @importFrom space space.joint
buildSpace <-
function(data){
	#number of samples
	n=ncol(data)
	
	#number of genes
	p=nrow(data)
	
	alpha=1
	l1=1/sqrt(n)*qnorm(1-alpha/(2*p^2))
	iter=3
	  
	result <- space.joint(t(data), lam1=l1*n*.161, lam2=0, weight=2, iter=iter)$ParCor
	
	
	#space strips the colnames off, so replace those
	colnames(result) <- rownames(data)
	rownames(result) <- rownames(data)
	as.matrix(result)
}
