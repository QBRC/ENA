test.misnamed <- function(){	
	checkException(adj2mat(data.frame(A=letters[1:4], B=letters[2:5])))	
}

test.valid <- function(){
	mat <- adj2mat(data.frame(Source=letters[1:4], Target=letters[2:5], Regulation=.5))
	correct <- matrix(0, ncol=5, nrow=5)
	diag(correct) <- 1
	for (i in 1:4){
		correct[i+1,i] <- .5
		correct[i,i+1] <- .5
	}
	rownames(correct) <- colnames(correct) <- letters[1:5]
	checkIdentical(correct, mat)
}