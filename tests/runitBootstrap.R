test.bootstrap <- function(){
	b <- bootstrap(exp, "mockBuilder", iterations=5)
	checkEquals(dim(b), c(45, 3))
	checkEquals(b[b[,1] == "g1" & b[,2] == "g2",3], 0.2885390081777926552)
	checkTrue(all(b[b[,1] != "g1" & b[,2] != "g2",3] == 0.062525926423249500452))
}
