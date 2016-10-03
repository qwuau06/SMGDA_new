rm(list = ls())
options(digits = 3)
t1 = Sys.time()
library("igraph")
library("parallel")
library("mco")
library("gurobi")
source("action_without_cluster.R")
source("driver.R")
source("minNorm.R")
source("tempCode.R")

pm <- cbind(c(0.2, 0.005), c(0.005, 0.15))
args <- commandArgs(trailingOnly = TRUE)
startname <- paste("start_",args[1],".R")
print(startname)
source(startname)

m <<- ecount(read)
n <<- vcount(read)
g = sampleGraph(read)
tar.bw <<- betweenness(read)
tar.dd <<- degree(read)
tar.pr <<- page.rank(read)$vector
input = rep(list(g), detectCores())
runs <<- length(input)

mu <- runif(8, 0, 1)
mu <- mu/sum(mu)
muin <- mu
z <- mu
print(mu)
tol = 10^(-3)
k = 1
w = rep(1, 8)

Result <- driver(input, main)
y = colMeans(Result)
print(y)
oh = .05
h = oh
mu_tmp=mu
storedmus=list()
storedres=c()

while(norm(as.matrix(w),'2')>tol && k<=120){
	print("# of iteration:")
	print(k)
	temp=matrix(0,nrow=2,ncol=3)
	temp1=matrix(0,nrow=3,ncol=8)
	for(i1 in 1:8){
		mu[i1]=mu_tmp[i1]+h/2
		mu[which(mu<0)]=0
		mu<<-mu/sum(mu)
		Result <- driver(input,main)
		temp[1,]=colMeans(Result)
		mu[i1]=mu_tmp[i1]-h/2
		mu[which(mu<0)]=0
		mu<<-mu/sum(mu)
		Result <- driver(input,main)
		temp[2,]=colMeans(Result)
		temp1[,i1]=t((temp[1,]-temp[2,])/h)
	}
	X=temp1
	#source('SMGDA/minNorm1.R')
	w = minNorm(X)
	print(w)
	# Decide step size
	it = 1
	mu = mu_tmp-h*w
	mu[which(mu<0)]=0
	mu<<-mu/sum(mu)
	Result <- driver(input,main)
	newy = colMeans(Result)
	h = oh
	while(it<=10){
		if(all(newy<y)) h = h+h*(0.5^it)
		else h = h-h*(0.5^it)
		it = it+1
		mu = mu_tmp-h*w
		mu[which(mu<0)]=0
		mu<<-mu/sum(mu)
		Result <- driver(input,main)
		newy = colMeans(Result)
	}
	print("Step size:")
	print(h)
	# Size is h
	k=k+1
	y=newy
	mu_tmp=mu
	storedmus = c(storedmus,list(mu_tmp))
	storedres = rbind(storedres,y)
	print(y)
	print(norm(as.matrix(w),'2'))
	print(mu)
}
finalres = paretoFilter(storedres)
finalmus = c()

tmp = duplicated(rbind(finalres,storedres))
tmp1 = tmp[(nrow(finalres)+1):length(tmp)]
finalmus = storedmus[tmp1]

print(finalmus)
print(finalres)
save(list = ls(),file="fin_ff1.rdata")
store=finalres
eff=finalmus

b1=2
bin=TRUE
while(b1<=5 && bin){
	mu=matrix(nrow=b1,ncol=8)					#mu is the action matrix, pbar is the column
	if(b1==2)
		mu[1,]=finalmus[[ceiling(runif(1,0,length(finalmus)))]]
	else
		mu[1:(b1-1),]=finalmus1[[ceiling(runif(1,0,length(finalmus1)))]][,1:8]
	mu[b1,]=runif(8,0,1)
	mu[b1,]=mu[b1,]/sum(mu[b1,])
	pbar=runif(b1)
	pbar=pbar/sum(pbar)
	mu<<-cbind(mu,pbar)
	source("SMGDA/tempCode.R")
	b1=b1+1
	finalres1 = paretoFilter(storedres)
	finalmus1 = c()

	tmp = duplicated(rbind(finalres1,storedres))
	tmp1 = tmp[(nrow(finalres1)+1):length(tmp)]
	finalmus1 = storedmus[tmp1]
	store7=rbind(store,finalres1)
	store=paretoFilter(store7)
	temp=duplicated(rbind(store,store7))
	temp1=temp[(length(store[,1])+1):length(temp)]
	eff1=c(eff,finalmus1)
	eff=eff1[temp1]
	print(eff)
	print(store)
	temp2=tail(duplicated(rbind(store,finalres1)),n=length(finalmus1))
	temp4=c()
	if(sum(temp2)==0)
		bin=FALSE
	else{
		store6.eff=finalmus1[temp2]
		for(ab in 1:length(store6.eff)){
			temp4=c(temp4,tail(c(store6.eff[[ab]]),n=1)>0.05)
		}
		if(sum(temp4)==0)
			bin=FALSE
	}
}
save(list = ls(),file=paste("fin_",args[1],".rdata"))
