test.upper <- function(){	
	checkIdentical(adjMat, tri2mat(rownames(adjMat), adjMat[upper.tri(adjMat)]))
}

test.lower <- function(){
	checkIdentical(adjMat, tri2mat(rownames(adjMat), adjMat[lower.tri(adjMat)], upper=FALSE))
}
