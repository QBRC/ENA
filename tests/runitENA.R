test.ena <- function(){
	net1 <- symmetricize(m, "ud", adjacencyList=TRUE)
	net2 <- symmetricize(m, "ld", adjacencyList=TRUE)
	net3 <- symmetricize(m, "avg", adjacencyList=TRUE)
	
	agg <- merge(net1, net2, by=c("Source", "Dest"))
	agg <- merge(agg, net3, by=c("Source", "Dest"))
	
	ena <- ena(agg[,3:5])
	checkEquals(ena, c(0.24044917348149391634, 0.30341307554227914256, 0.48089834696298783268))
	
}