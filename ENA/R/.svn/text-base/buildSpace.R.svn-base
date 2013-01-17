#' Reconstruct network using SPACE
#'
#' Reconstructs a gene regulatory network using the SPACE algorithm
#' @param data The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column.
#' @return The adjacency matrix of the genes provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
buildSpace <-
function(data){
	library(space)
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
