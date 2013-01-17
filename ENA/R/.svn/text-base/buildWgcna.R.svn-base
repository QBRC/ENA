#' Reconstruct network using WGCNA
#' 
#' Reconstructs a gene regulatory network using the WGCNA algorithm
#' @param mat The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column. Note that this is the transpose of how WGCNA typically accepts their matrix.
#' @return The adjacency matrix of the genes provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
buildWgcna <-
function(mat){
	library(WGCNA)
	grn <- adjacency(t(mat))
	return(grn);
}
