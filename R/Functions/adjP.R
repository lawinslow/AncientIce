adjP <- function(x, checkDup=TRUE){
	require(multcomp)
	
	# define methods
	meths <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "none")
	
	# check duplicates
	if(checkDup){
		x0 <- x
		x <- unique(x)
	}
	
	# create container
	ps <- matrix(NA, ncol=length(meths), nrow=length(x), dimnames=list(NULL, meths))
	
	# perform all corrections
	for(i in 1:length(meths)){
		ps[,i] <- p.adjust(x, method=meths[i])
	}
	
	# prepare output
	if(checkDup){
		blah <- data.frame(id=x0)
		blah2 <- merge(data.frame(id=x, ps), blah, all=TRUE, sort=FALSE)
		blah2[,-1]
	}else{
		ps
	}
	
}