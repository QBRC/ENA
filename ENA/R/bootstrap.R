#' Bootstrap the reconstruction of a network
#' 
#' Randomly selects a subset of the avaialble samples and performs a network reconstruction using the selected technique. Aggregate all produced networks into a single network using the \code{ena} method.
#' @param data The dataset to reconstruct. Each column should contain one sample, and each row should contain one gene.
#' @param fun The network reconstruction technique to employ while bootstrapping. Could be one of the provided methods such as \dQuote{\code{buildSpace}} or a custom function. Provide the name of the function in quotes.
#' @param sample.percentage The percentage of samples to select for each iteration.
#' @param gene.percentage The percentage of genes to select for each iteration.
#' @param iterations The number of bootstrapping iterations to perform -- i.e. the number of networks to build
#' @param cluster Optionally provide an RMPI cluster (of class MPIcluster) to distribute the workload across.
#' @param truth The true network structure. Typically not available, but useful in testing and debugging.
#' @return A data.frame representing the adjacency list of the ENA-produced network.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
bootstrap <- function(data, fun, sample.percentage=0.7, gene.percentage=0.9, iterations=150, cluster, truth){
	if (typeof(fun) != "character"){
		stop("You must provide the character name of the function you want to bootstrap. For instance, fun=\"buildSpace\"")
	}
	funName <- fun
	fun <- get(fun)
	
	funWrapper <- function(rand.seed, fun, data, sample.percentage, gene.percentage, ...){
		set.seed(rand.seed)
		sampledData <- data[sample(1:nrow(data),round(gene.percentage * nrow(data))),
												sample(1:ncol(data),round(sample.percentage * ncol(data)))]	
		
		net <- symmetricize(abs(fun(sampledData)));
		
		#expand the matrix to fill the entire network, rather than just the selected genes.
		totalNet <- matrix(NA_real_, ncol=nrow(data), nrow=nrow(data))
		rownames(totalNet) <- rownames(data)
		colnames(totalNet) <- rownames(data)
		totalNet[rownames(net),colnames(net)] <- net
		
		return(totalNet[upper.tri(totalNet)])
	}
	
	toReturn <- getTableAddressing(rownames(data), truth)
	
	if (!missing(cluster) && "MPIcluster" %in% class(cluster)){		
		clusterExport(cluster, c("symmetricize"))
		result <- clusterApplyLB(cluster, 1:iterations, funWrapper, fun, data, sample.percentage, gene.percentage)	
	}
	else{ 
		result <- lapply(1:iterations, funWrapper, fun, data, sample.percentage, gene.percentage)		
	}			
	
	result <- as.data.frame(result)
	
	toReturn[paste("Bootstrapped.", funName, sep="")] <- ena(result)
	return(toReturn)
}