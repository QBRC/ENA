#' Reconstruct network using GeneNet
#'
#' Reconstructs a gene regulatory network using the GeneNet algorithm
#' @param data The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column.
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
#' gn <- abs(buildGenenet(net))
#' gn <- gn[upper.tri(gn)]
#' 
buildGenenet <-
function(data){
	library(GeneNet)
	
	inferred.pcor <- ggm.estimate.pcor(t(data));
	
	geneList <- rownames(data);
	
	#get rid of gn-specific classing and attributes
	inferred.pcor <- matrix(as.numeric(inferred.pcor), ncol=nrow(data))
	
	grn <- inferred.pcor
	rownames(grn) <- geneList
	colnames(grn) <- geneList
	
	return(grn);
}
