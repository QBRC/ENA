#' Perform ensemble network aggregation
#' 
#' Manipulates adjacency-list-formatted networks into a single adjacency list
#' @param adjacencyList A \code{data.frame} in which each column represents the connection weights of a network. Each row represents a possible connection within the network.
#' @param method Currently only support (Inverse) Rank Product, specified by "RankProd"
#' @return A single adjacency list representing the Inverse Rank Product of all connections in the provided networks.
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
#' #Process with WGCNA
#' wg <- abs(buildWgcna(net))
#' wg <- wg[upper.tri(wg)]
#' 
#' #Process with SPACE
#' sp <- abs(buildSpace(net))
#' sp <- sp[upper.tri(sp)]
#' 
#' #Aggregate methods using ENA
#' ena <- ena(cbind(gn, wg, sp))
#' 
#' #Convert from a triangular vector to a full matrix.
#' enaMat <- tri2mat(rownames(net), ena)
ena <- function(adjacencyList, method=c("RankProd")){	
	method <- match.arg(method)	
	if (method == "RankProd"){
		toReturn <-			1/apply(log(apply(-adjacencyList,2,rank)+1),1,sum);
	}
	#TODO: others: mean, rank median
	
	 return(toReturn);		
}
