test.nonsym <- function(){	
	checkException(mat2adj(matrix(c(1:9), ncol=3)))	
}

test.reorderNames <- function(){
	mat <- matrix(c(1,2,3,4,2,1,2,3,3,2,1,2,4,3,2,1), ncol=4)
	colnames(mat) <- letters[1:4]
	rownames(mat) <- letters[4:1]
	checkException(mat2adj(mat))
}

test.emptyNames <- function(){
	mat <- matrix(c(1,2,3,4,2,1,2,3,3,2,1,2,4,3,2,1), ncol=4)	
	checkException(mat2adj(mat))
}

test.noZeroes <- function(){
	mat <- matrix(c(1,2,3,4,2,1,2,3,3,2,1,2,4,3,2,1), ncol=4)
	colnames(mat) <- rownames(mat) <- letters[1:4]
	adj <- mat2adj(mat)
	correct <- data.frame(Source=c("a", "a", "b", "a", "b", "c"),
												Target=c("b", "c", "c", "d", "d", "d"),
												Regulation=c(2,3,2,4,3,2))
	checkIdentical(adj, correct)
}

test.withZeroes <- function(){
	mat <- matrix(c(1,2,3,0,2,1,2,3,3,2,1,2,0,3,2,1), ncol=4)
	colnames(mat) <- rownames(mat) <- letters[1:4]
	adj <- mat2adj(mat)
	correct <- data.frame(Source=c("a", "a", "b", "b", "c"),
												Target=c("b", "c", "c", "d", "d"),
												Regulation=c(2,3,2,3,2))
	checkIdentical(adj, correct)
}