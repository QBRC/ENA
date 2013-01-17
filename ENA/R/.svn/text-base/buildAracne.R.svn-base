#' Reconstruct network using Aracne
#'
#' Reconstructs a gene regulatory network using the Aracne algorithm
#' @param mat The matrix on which to reconstruct. The matrix should store one gene per row, and one sample per column.
#' @return The adjacency matrix of the genes provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
buildAracne <-
function(mat){
	library(parmigene)
	mi <- knnmi.all(mat)
	grn <- aracne.a(mi, 0.05);
	return (grn);
}
