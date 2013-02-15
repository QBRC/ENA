#' Reconstruct network using Aracne
#'
#' Reconstructs a gene regulatory network using the Aracne algorithm
#' @param mat The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column.
#' @return The adjacency matrix of the genes provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @examples
#' #Load in the sample PPI data provided with this package
#' data(PPI)
#' #Simulate the network based on one of the adjacency lists just loaded.
#' net <- simulateNetwork(net44)
#' #Reconstruct the network using GeneNet, then grab the upper traingular portion
#' # of the matrix
#' ar <- abs(buildAracne(net))
#' ar <- ar[upper.tri(ar)]
#' 
buildAracne <-
function(mat){
	library(parmigene)
	mi <- knnmi.all(mat)
	grn <- aracne.a(mi, 0.05);
	return (grn);
}
