k=1
w=rep(1,8)
Result <- driver(input,main)
y=colMeans(Result)
print(y)
oh=.05
h = oh
mu_tmp=mu
storedmus=list()
storedres=c()
while(norm(as.matrix(w),'2')>tol && k<=100){
#row update
	print("# of iteration:")
	print(k)
	temp=matrix(0,nrow=2,ncol=3)
	temp1=matrix(0,nrow=3,ncol=(8+b1))
	for(i1 in 1:(8+b1)){
		if(i1<=8){
			mu[b1,i1]=mu_tmp[b1,i1]+h/2
			mu[which(mu<0)]=0
			mu[b1,1:8]=mu[b1,1:8]/sum(mu[b1,1:8])
			mu<<-mu
			Result <- driver(input,main)
			temp[1,]=colMeans(Result)
			mu[b1,i1]=mu_tmp[b1,i1]-h/2
			mu[which(mu<0)]=0
			mu[b1,1:8]=mu[b1,1:8]/sum(mu[b1,1:8])
			mu<<-mu
			Result <- driver(input,main)
		}
		else{
			mu[(i1-8),9]=mu_tmp[(i1-8),9]+h/2
			mu[which(mu<0)]=0
			mu[,9]=mu[,9]/sum(mu[,9])
			mu<<-mu
			Result <- driver(input,main)
			temp[1,]=colMeans(Result)
			mu[(i1-8),9]=mu_tmp[(i1-8),9]-h/2
			mu[which(mu<0)]=0
			mu[,9]=mu[,9]/sum(mu[,9])
			mu<<-mu
			Result <- driver(input,main)
		}
		temp[2,]=colMeans(Result)
		temp1[,i1]=t((temp[1,]-temp[2,])/h)
	}
	X=temp1
	w <- minNorm(X,b1)
	mu[b1,1:8]=mu_tmp[b1,1:8]-.05*w[1:8]			#step size calculation needed
	mu[which(mu<0)]=0
	mu[b1,1:8]=mu[b1,1:8]/sum(mu[b1,1:8])
	mu[,9]=mu_tmp[,9]-.15*w[9:(8+b1)]			#step size calculation needed
	mu[which(mu<0)]=0
	mu[,9]=mu[,9]/sum(mu[,9])
	mu<<-mu
	Result <- driver(input,main)
	y=colMeans(Result)
	mu_tmp=mu
	storedmus = c(storedmus,list(mu_tmp))
	storedres = rbind(storedres,y)
	k=k+1
}
print(storedres)
print(storedmus)
