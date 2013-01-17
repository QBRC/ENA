#' Get the adjacency list addressing template
#' 
#' Internal function used to get the addressing template for a data.frame to contain the adjacency list representation of a matrix.
#' @param variableNames the names of all genes to include in the adjacency list
#' @param truth The true adjacency matrix. Often will not be available, but is useful for debugging and testing.
#' @return A data.frame representing the adjacency list of the matrix provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
getTableAddressing <- function (variableNames, truth){		
	joint <- matrix(0, nrow=length(variableNames), ncol=length(variableNames))
	rownames(joint) <- variableNames
	colnames(joint) <- variableNames
	
	#extract the row, col indices for the upper-triangular portion of the matrix
	address <- cbind(row(joint)[upper.tri(joint)], col(joint)[upper.tri(joint)])
	address <- matrix(row.names(joint)[address], ncol=2)
	
	upper <- upper.tri(joint);
	
	if (missing(truth)){
		aggregate <- data.frame(address)
		colnames(aggregate) <- c("Source", "Dest")
	}
	else{
		aggregate <- data.frame(address, truth[upper])
		colnames(aggregate) <- c("Source", "Dest", "Truth")
	}
	return(aggregate);
}
