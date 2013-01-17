library(RUnit)
library(ENA)

m <- matrix(1:9, ncol=3)	
rownames(m) <- colnames(m) <- letters[1:3]

#simulate a 10 gene, 5 sample expression dataset
exp <- matrix(rnorm(50), ncol=5)
rownames(exp) <- paste("g", 1:10, sep="")
colnames(exp) <- paste("s", 1:5, sep="")

#' Mock network reconstruction function
#' 
#' Mocks the behavior of a network reconstruction function, but consistently provides an appropriately size matrix for the input, which is all 0 except for the diagonal and [2,1] and [1,2] = 1
#' @param mat The matrix of gene expression samples with genes in the rows.
#' @return a matrix which is all 0 except for the diagonal and [2,1], [1,2] which = 1 and is n x n where n is the number of rows in the provided matrix
mockBuilder <- function(mat){
	n <- nrow(mat)
	mat2 <- matrix(0, nrow=n, ncol=n)
	colnames(mat2) <- rownames(mat2) <- rownames(mat2)
	diag(mat2) <- 1
	mat2[1,2] <- mat2[2,1] <- 1
	return(mat2)
}

testsuite <- defineTestSuite("ENA", dirs="./tests/", testFileRegexp="^runit.+\\.[rR]", testFuncRegexp = "^test.+")
testResult <- runTestSuite(testsuite)
printHTMLProtocol(testResult, "tests/testResults.html")
