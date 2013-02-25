#' Simulate a gene expression dataset.
#' 
#' Simulates the observed gene expression levels in a dataset using the underlying
#' truth network provided, allowing cusomtization of the number of samples and the
#' noise levels in the dataset.
#'
#' @param adjList the adjacency list of the matrix you're looking to simulate. 
#' The first column should be the source and the second column the target. To avoid
#' any confusion, we require that the columns be named exactly "Source" and "Target".
#' @param genes The list of all genes in the network. By default this is any gene
#' mentioned in the adjacency list.
#' @param samples The number of samples you wish to simulate
#' @param noise the amount of noise present in the simulated expression levels.
#' @author Guanghua Xiao \email{Guanghua.Xiao@@UTSouthwestern.edu}
#' @export
#' @examples
#' #Load in the sample PPI data provided with this package
#' data(PPI)
#' #Simulate the network based on one of the adjacency lists just loaded.
#' net <- simulateNetwork(net44)
simulateNetwork <- function(adjList, genes=sort(union(adjList[,1], adjList[,2])), samples=100, noise=1)
{	
	#Code gets grumpy when levels of factors are different, just convert to character
	adjList$Source <- as.character(adjList$Source)
	adjList$Target <- as.character(adjList$Target)
	
	adjList <- adjList[adjList$Source != adjList$Target,]
	adjList[adjList$Source > adjList$Target,] <- adjList[adjList$Source > adjList$Target,2:1]
	adjList[adjList$Source > adjList$Target,]
	
	nGene <- length(genes)
	
	N <- nrow(adjList)
	adjList$regulation <- round(sign(rbinom(N, 1, 0.5) - 0.5)*rnorm(N,0.5,0.2) ,2)
	adjList$type <- sign(adjList$regulation)
	
	
	#### initiate the matrix expr to store the simulated expression level ####
	expr <- matrix(0,nGene, samples)
	rownames(expr) <- genes
	colnames(expr) <- paste("S", 1:samples, sep="")
	
	unknown <- unique(adjList$Target)
	known <- setdiff(genes, unknown)
	
	for(i in known)
	{
		expr[as.character(i),] <- rbinom(samples,1,0.5)*2 + rnorm(samples,0,noise) - 1
	}
	
	while(length(unknown) > 0)
	{
		for(i in unknown)
		{
			source <- as.character(adjList$Source[adjList$Target == i])
			if(all(source %in% known))
			{
				val <- ifelse(rep(length(source) == 1,samples),
											expr[as.character(source),]*adjList[adjList$Target == i,3] ,
											adjList[adjList$Target == i,3] %*% expr[as.character(source),])
				expr[as.character(i),] <- as.numeric(val > 0.0)*2-1 + rnorm(samples,0,noise)
				known <- append(known,i)
				unknown <- setdiff(unknown, i)
			}
		}
	}
	
	expr	
}