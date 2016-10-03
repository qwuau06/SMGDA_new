driver <- function(input,main){

	runs=length(input)
	Result <- data.frame(B=numeric(runs), PR=numeric(runs), DD=numeric(runs), stringsAsFactors=FALSE)
	out=mclapply(input,main,mc.cores=detectCores())
	for(i in 1:runs){
		if(!is.igraph(out[[i]])){
			print("error")
			save(list = ls(),file="error.rdata")
			source("Version6/driver.R")
		}
	}
	for(i in 1:runs){
		Result$B[i]=ks.test(tar.bw,betweenness(out[[i]]))$statistic
		Result$PR[i]=ks.test(tar.pr,page.rank(out[[i]])$vector)$statistic
		Result$DD[i]=ks.test(tar.dd,degree(out[[i]]))$statistic
	}

	return(Result)
}
