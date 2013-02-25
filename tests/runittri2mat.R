test.upper <- function(){	
	adjMat <- symmetricize(matrix(1:16, ncol=4))
	diag(adjMat) <- 1
	rownames(adjMat) <- colnames(adjMat) <- letters[1:4]
	
	checkIdentical(adjMat, tri2mat(rownames(adjMat), adjMat[upper.tri(adjMat)]))
}

test.lower <- function(){
	adjMat <- symmetricize(matrix(1:16, ncol=4))
	diag(adjMat) <- 1
	rownames(adjMat) <- colnames(adjMat) <- letters[1:4]
	
	checkIdentical(adjMat, tri2mat(rownames(adjMat), adjMat[lower.tri(adjMat)], upper=FALSE))
}
