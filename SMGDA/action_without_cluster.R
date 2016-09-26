main<-function(g){
  p=createp(g,mu)
  return(makeGraph(g,p))
}

sampleGraph<-function(target){
  m1=ecount(target)
  n1=vcount(target)
  del=sample.int(m1,ceiling(m1-0.7*n1))
  return(delete.edges(target,del))
}

createp<-function(graph,mu){      #z is no of actions
  p=matrix(nrow=n,ncol=8)
  if(length(mu)>8){
	  u=runif(n,0,1)
	  p1=cumsum(mu[,9])
	  for(i in 1:n){
	  	b2=which(p1>=u[i])[1]
	  	p[i,]=mu[b2,1:8]
	  }
  }
  else{
	p=matrix(rep(mu,n),nrow=n,ncol=8,byrow=TRUE)
  }
  return(p)
}

makeGraph<-function(graph,p){
  d<-degree(graph)
  d1<-cumsum(d)
  d2<-numeric(n)
  for(y in 1:n){
    d2[y]<-mean(degree(graph,neighbors(graph,y)))
  }
  d2[which(is.na(d2))]<-0
  d3<-cumsum(d2)
  pr<-page.rank(graph)$vector
  pr1<-cumsum(pr)
  b<-betweenness(graph)      #can use estimate in case of large graphs
  b1<-cumsum(b)
  si=similarity.jaccard(graph)
  diag(si)=0
  si1=max.col(si,"random")
  si=similarity.invlogweighted(graph)
  si2=max.col(si,"random")
  g1<-graph
  u=runif(n,0,1)
  for(i in 1:n){
    id=which(cumsum(p[i,])>=u[i])[1]
    x=switch(id,action1,action2,action3,action4,action5,action6,action7,action8)
    data=switch(id,d3,d1,pr1,b1,0,si2,si1,0)
    j=x(i,graph,data)
    if(j!=-1){
      if(!(i==j||are.connected(graph,i,j))){
        g1<-g1+edge(i,j)
        g1<-simplify(g1)
        if(ecount(g1)==m)
          return(g1)
      }
    }
  }   
  if(ecount(g1)-ecount(graph)>0)
    makeGraph(g1,p)
  else
    return(g1)
}

action1<-function(i,graph,data){       #probability distributed on average degree of neighbors
  return(which(data>=runif(1,0,tail(data,1)))[1])
}
action2<-function(i,graph,data){       #probability distributed on degree
  return(which(data>=runif(1,0,tail(data,1)))[1])
}
action3<-function(i,graph,data){       #probability distributed on page rank
  return(which(data>=runif(1,0,1))[1])
}
action4<-function(i,graph,data){       #probability distributed on vertex betweenness
  return(which(data>=runif(1,0,tail(data,1)))[1])
}
action5<-function(i,graph,data){       #connect to a second neighbor
  a=neighborhood(graph,order=2,nodes=i)
  if(neighborhood.size(graph,2,i)==neighborhood.size(graph,1,i)){
    return(ceiling(runif(1,0,n)))
  }
  else{
    a1=a[[1]][(neighborhood.size(graph,1,i)+1):neighborhood.size(graph,2,i)]
    return(a1[ceiling(runif(1,0,length(a1)))])
  }
}
action6<-function(i,graph,data){       #inverse log-weighted similarity
  return(data[i])
}
action7<-function(i,graph,data){       #jaccard similarity
  return(data[i])
}
action8<-function(i,graph,data){       #no edge
  return(-1)
}
