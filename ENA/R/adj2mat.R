#' Convert an adjacency-like list (which may or may not contain all the gene
#' IDs in the network) into an adjacency matrix.
#'
#' @param adjList the adjacency list of the matrix you're looking to simulate. 
#' There should be three columns for source, target, and the regulation value. 
#' To avoid any confusion, we require that the columns be named exactly 
#' "Source", "Target", and "Regulation", where "Regulation" represents the 
#' strength or weight of the regulation.
#' @param IDs The set of genes in this network. By default, the set of all
#' genes mentioned in the adjacency matrix. IDs can be provided if there are
#' unconnected genes in the network which aren't mentioned in the adjacency
#' list or when the ordering of the genes is important.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
adj2mat <- function(adjList, IDs=sort(union(adjList[,1], adjList[,2]))){
	#get rid of any factors in the data.frame
	adjList <- as.data.frame(lapply(adjList, as.character), stringsAsFactors=FALSE)
	
	if (!all(colnames(adjList %in% c("Source", "Target", "Regulation"))) || 
				length(unique(colnames(adjList))) != 3){
		stop("adjList must have three columns named 'Source', 'Target', and 'Regulation'.")
	}
	
	mat <- matrix(0, ncol=length(IDs), nrow=length(IDs))
	rownames(mat) <- IDs
	colnames(mat) <- IDs
	diag(mat) <- 1;
	for (i in 1:nrow(adjList)){
		s <- adjList[i,]$Source
		t <- adjList[i,]$Target
		r <- adjList[i,]$Regulation
		mat[which(IDs == s), which(IDs == t)] <- as.numeric(r)
	}
	mat <- symmetricize(mat, "ud")
	mat
}