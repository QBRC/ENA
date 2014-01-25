#' Bootstrap the reconstruction of a network
#' 
#' Randomly selects a subset of the avaialble samples and performs a network reconstruction using the selected technique. Aggregate all produced networks into a single network using the \code{ena} method.
#' @param data The dataset to reconstruct. Each column should contain one sample, and each row should contain one gene.
#' @param fun The network reconstruction technique to employ while bootstrapping. Could be one of the provided methods such as \dQuote{\code{buildSpace}} or a custom function. Provide the name of the function in quotes.
#' @param sample.percentage The percentage of samples to select for each iteration.
#' @param iterations The number of bootstrapping iterations to perform -- i.e. the number of networks to build
#' @param cluster Optionally provide an RMPI cluster (of class MPIcluster) to distribute the workload across.
#' @param truth The true network structure. Typically not available, but useful in testing and debugging.
#' @return A data.frame representing the adjacency list of the ENA-produced network.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @examples
#' #Load in the sample Protein-Protein-Interaction data that comes with this package.
#' data(PPI)
#' set.seed(123)
#' 
#' #Simulate a dataset based on the 44-gene topology provided.
#' sim <- simulateNetwork(net44)
#' 
#' boot <- bootstrap(sim, "buildGenenet", .9, 10, )
#' bootMat <- tri2mat(rownames(sim), boot[,3])
#' @importFrom parallel clusterExport
#' @importFrom parallel parLapply
bootstrap <- function(data, fun, sample.percentage=0.7, iterations=150, cluster, truth){
	if (typeof(fun) != "character"){
		stop("You must provide the character name of the function you want to bootstrap. For instance, fun=\"buildSpace\"")
	}
	funName <- fun
	fun <- get(fun)
	
	funWrapper <- function(rand.seed, fun, data, sample.percentage, ...){
		set.seed(rand.seed)
		sampledData <- data[,sample(1:ncol(data),round(sample.percentage * ncol(data)))]	
		
		net <- symmetricize(abs(fun(sampledData)));
		
		return(net[upper.tri(net)])
	}
	
	toReturn <- getTableAddressing(rownames(data), truth)
	
	if (!missing(cluster) && "MPIcluster" %in% class(cluster)){		
		# Due to some strange quirk of the depends/imports mess (we import GeneNet
		# but GeneNet depends on corpcor), we have to export this function from
		# corpcor than GeneNet relies on. No idea.
		clusterExport(cluster, c("symmetricize", "pcor.shrink"))
		result <- parLapply(cluster, 1:iterations, funWrapper, fun, data, sample.percentage)	
	}
	else{ 
		result <- lapply(1:iterations, funWrapper, fun, data, sample.percentage)		
	}			
	
	result <- as.data.frame(result)
	
	toReturn[paste("Bootstrapped.", funName, sep="")] <- ena(result)
	return(toReturn)
}