#' Convert triangular elements to full matrix.
#' 
#' Converts the upper or lower-triangular portion of a matrix back to the
#' complete 2D matrix using the gene names provided. The matrix is assumed
#' to be symmetrical.
#' 
#' @param genes The names of the genes to use as row and column names. Note that
#' these must be in the original order as was used when the traingular portion
#' was extracted from the matrix. Otherwise, the matrix will not be constructed
#' correctly.
#' @param tri The triangular elements of the matrix. Could be extracted using 
#' a command like \code{mat[upper.tri(mat)]}
#' @param diag The value to use for the diagonal elements in the matrix.
#' @param upper \code{TRUE} if \code{tri} represents the upper triangular 
#' portion of a matrix, \code{FALSE} if the lower. \code{\link{upper.tri}} and 
#' \code{\link{lower.tri}} extract the elements in a different order.
#' @return the complete 2D matrix represented by the traingular portion provided.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
#' @examples
#' #Load in the sample PPI data provided with this package
#' data(PPI)
#' #Simulate the network based on one of the adjacency lists just loaded.
#' net <- simulateNetwork(net44)
#' #Reconstruct the network using GeneNet, then grab the upper traingular portion
#' # of the matrix
#' gn <- abs(buildGenenet(net))
#' gn <- gn[upper.tri(gn)]
#'  
#' #Convert from a triangular vector to a full matrix.
#' gnMat <- tri2mat(rownames(net), gn)
#' 
tri2mat <- function(genes, tri, diag=1, upper=TRUE){
	mat <- matrix(0, nrow=length(genes), ncol=length(genes))
	colnames(mat) <- rownames(mat) <- genes
	diag(mat) <- diag
	
	#inspired by Michael Conklin, http://www.biostat.wustl.edu/archives/html/s-news/2000-03/msg00127.html
	if (upper){	
		#extract the indices of all upper-triangular elements
		uti <- matrix(c(row(mat)[upper.tri(mat)], col(mat)[upper.tri(mat)]), ncol = 2)
		
		mat[uti] <- tri
		mat <- symmetricize(mat, "ud")
	} else{
		#extract the indices of all upper-triangular elements		
		lti <- matrix(c(row(mat)[lower.tri(mat)], col(mat)[lower.tri(mat)]), ncol = 2)
		
		mat[lti] <- tri
		mat <- symmetricize(mat, "ld")
	}
	
	mat
}