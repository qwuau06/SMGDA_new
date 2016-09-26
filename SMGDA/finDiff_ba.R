rm(list=ls())
source("SMGDA/start_ba.R")
options(digits=3)
mu<<-runif(8,0,1)
mu<<-mu/sum(mu)
muin<<-mu
print(mu)
z<<-length(mu)
t1=Sys.time()
tol=10^(-3)
k=1
w=rep(1,8)
source("SMGDA/driver.R")
y=colMeans(Result)
print(y)
oh=.05
h = oh
mu_tmp=mu
storedmus=c()
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
		source("SMGDA/driver.R")
		temp[1,]=colMeans(Result)
		mu[i1]=mu_tmp[i1]-h/2
		mu[which(mu<0)]=0
		mu<<-mu/sum(mu)
		source("SMGDA/driver.R")
		temp[2,]=colMeans(Result)
		temp1[,i1]=t((temp[1,]-temp[2,])/h)
	}
	X=temp1
	source('SMGDA/minNorm.R')
	print(w)
	# Decide step size
	it = 1
	mu = mu_tmp-h*w
	mu[which(mu<0)]=0
	mu<<-mu/sum(mu)
	source("SMGDA/driver.R")
	newy = colMeans(Result)
	h = oh
	while(it<=10){
		if(all(newy<y)) h = h+h*(0.5^it)
		else h = h-h*(0.5^it)
		it = it+1
		mu = mu_tmp-h*w
		mu[which(mu<0)]=0
		mu<<-mu/sum(mu)
		source("SMGDA/driver.R")
		newy = colMeans(Result)
	}
	print("Step size:")
	print(h)
	# Size is h
	k=k+1
	y=newy
	mu_tmp=mu
	storedmus = rbind(storedmus,mu_tmp)
	storedres = rbind(storedres,y)
	print(y)
	print(norm(as.matrix(w),'2'))
	print(mu)
}
finalres = paretoFilter(storedres)
finalmus = c()

tmp = duplicated(rbind(finalres,storedres))
tmp1 = tmp[(nrow(finalres)+1):length(tmp)]
finalmus = storedmus[tmp1,]

print(finalmus)
print(finalres)
save(list = ls(),file="fin_ba.rdata")
