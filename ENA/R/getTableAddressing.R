#' Get the adjacency list addressing template. 
#' 
#' Useful if you want to store the networks in their condensed upper-diagonal form while still
#' having the benefit of convenient addressing and/or if you are using a simulated dataset in
#' which you know the truth and want to store all the values in a single data.frame.
#' 
#' Internal function used to get the addressing template for a data.frame to contain the adjacency 
#' list representation of a matrix.
#' @param variableNames the names of all genes to include in the adjacency list
#' @param truth The true adjacency matrix. Often will not be available, but is useful for 
#' debugging and testing.
#' @return A data.frame representing the adjacency list of the matrix provided.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @examples
#' #Load in the sample Protein-Protein-Interaction data that comes with this package.
#' data(PPI)
#' 
#' #Simulate a dataset based on the 44-gene topology provided.
#' sim <- simulateNetwork(net44)
#' 
#' #Convert the true, underlying adjacency list to an adjacency matrix
#' trueMat <- adj2mat(net44)
#' 
#' #Reconstruct using GeneNet
#' gn <- abs(buildGenenet(sim))
#' gn <- gn[upper.tri(gn)]
#' 
#' wg <- abs(buildWgcna(sim))
#' wg <- wg[upper.tri(wg)]
#' 
#' #Aggregate all results into a single data.frame
#' data <- getTableAddressing(rownames(sim), trueMat)
#' data <- cbind(data, gn, wg)
#' 
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
