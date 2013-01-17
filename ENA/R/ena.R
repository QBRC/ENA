#' Perform ensemble network aggregation
#' 
#' Manipulates adjacency-list-formatted networks into a single adjacency list
#' @param adjacencyList A \code{data.frame} in which each column represents the connection weights of a network. Each row represents a possible connection within the network.
#' @param method Currently only support (Inverse) Rank Product, specified by "RankProd"
#' @return A single adjacency list representing the Inverse Rank Product of all connections in the provided networks.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
ena <-
function(adjacencyList, method=c("RankProd")){	
	method <- match.arg(method)	
	if (method == "RankProd"){
		toReturn <-			1/apply(log(apply(-adjacencyList,2,rank)+1),1,sum);
	}
	#TODO: others: mean, rank median
	
	 return(toReturn);		
}
