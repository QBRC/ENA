test.ld <- function(){	
	checkEqualsNumeric(symmetricize(m, "ld"), matrix(c(1:3,2,5,6,3,6,9), ncol=3))
	checkEquals(rownames(symmetricize(m, "ld")), letters[1:3])
	checkEquals(colnames(symmetricize(m, "ld")), letters[1:3])
}

test.ud <- function(){	
	checkEqualsNumeric(symmetricize(m, "ud"), matrix(c(1,4,7,4,5,8,7,8,9), ncol=3))
	checkEquals(rownames(symmetricize(m, "ud")), letters[1:3])
	checkEquals(colnames(symmetricize(m, "ud")), letters[1:3])
}

test.avg <- function(){	
	checkEqualsNumeric(symmetricize(m, "avg"), matrix(c(1,3,5,3,5,7,5,7,9), ncol=3))
	checkEquals(rownames(symmetricize(m, "avg")), letters[1:3])
	checkEquals(colnames(symmetricize(m, "avg")), letters[1:3])
}

test.min <- function(){	
	checkEqualsNumeric(symmetricize(m, "min"), matrix(c(1:3,2,5,6,3,6,9), ncol=3))
	checkEquals(rownames(symmetricize(m, "min")), letters[1:3])
	checkEquals(colnames(symmetricize(m, "min")), letters[1:3])
}

test.max <- function(){	
	checkEqualsNumeric(symmetricize(m, "max"), matrix(c(1,4,7,4,5,8,7,8,9), ncol=3))
	checkEquals(rownames(symmetricize(m, "max")), letters[1:3])
	checkEquals(colnames(symmetricize(m, "max")), letters[1:3])
}

test.adj.ld <- function(){
	a <- symmetricize(m, "ld", adjacencyList=TRUE)
	checkEquals(a[a[,1] == letters[1] & a[,2] == letters[2],3], 2)
	checkEquals(a[a[,1] == letters[1] & a[,2] == letters[3],3], 3)
	checkEquals(a[a[,1] == letters[2] & a[,2] == letters[3],3], 6)
}

test.adj.avg <- function(){
	a <- symmetricize(m, "avg", adjacencyList=TRUE)
	checkEquals(a[a[,1] == letters[1] & a[,2] == letters[2],3], 3)
	checkEquals(a[a[,1] == letters[1] & a[,2] == letters[3],3], 5)
	checkEquals(a[a[,1] == letters[2] & a[,2] == letters[3],3], 7)
}

test.adj.noNames <- function(){
	m_no_name <- matrix(1:9, ncol=3)
	checkException(symmetricize(m_no_name, "ld", adjacencyList=TRUE))	
}

