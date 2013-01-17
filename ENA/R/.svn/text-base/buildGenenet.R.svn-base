#' Reconstruct network using GeneNet
#'
#' Reconstructs a gene regulatory network using the GeneNet algorithm
#' @param data The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column.
#' @return The adjacency matrix of the genes provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
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
