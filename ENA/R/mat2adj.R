#' Convert a matrix to an adjacency list
#' 
#' Takes a matrix and converts all non-zero elements to an adjacency
#' list using the row/colnames as the names for this list. Currently,
#' the matrix must be symmetric.
#' @param adjMat The symmetric adjacency matrix with rows and columns
#' named. 
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
#' @examples
#' mat <- matrix(c(1,4,0,4,1,2,0,2,1), ncol=3)
#' rownames(mat) <- colnames(mat) <- letters[1:3]
#' mat2adj(mat)
mat2adj <- function(adjMat){
	if (!isSymmetric(adjMat)){
		stop("The matrix provided is not symmetric. Currently, this function only works for symmetric matrices. This includes the rownames and colnames of the matrix.")
	}
	
	names <- rownames(adjMat)
	
	if (is.null(names) || length(names) != ncol(adjMat)){
		stop("Matrix must have (identical) rownames and colnames in order to convert to adjacency list.")
	}
	
	#inspired by Michael Conklin, http://www.biostat.wustl.edu/archives/html/s-news/2000-03/msg00127.html
	#extract the indices of all upper-triangular elements
	uti <- matrix(c(row(adjMat)[upper.tri(adjMat)], col(adjMat)[upper.tri(adjMat)]), ncol = 2)
	uti <- uti[adjMat[uti]!=0,]
	
	adj <- data.frame(Source=names[uti[,1]], Target=names[uti[,2]], Regulation=adjMat[uti])
	
	adj	
}