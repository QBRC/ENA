test.matrix <- function(){	
	set.seed(100)
	mat <- matrix(runif(16, 0, 1), ncol=4)
	colnames(mat) <- rownames(mat) <- letters[1:4]
	cut <- edgeCutoff(mat, 2)
	
	correct <- matrix(0, ncol=4, nrow=4)
	diag(correct) <- 1
	correct[3,4] <- correct[4,3] <- 1
	colnames(correct) <- rownames(correct) <- letters[1:4]
	
	checkIdentical(correct, cut)
}

test.tri <- function(){
	set.seed(100)
	mat <- matrix(runif(16, 0, 1), ncol=4)
	mat <- mat[upper.tri(mat)]	
	cut <- edgeCutoff(mat, 2)
	
	correct <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
	checkIdentical(correct, cut)
}
