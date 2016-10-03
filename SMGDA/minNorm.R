#X is a matrix with gradients as row vectors
minNorm_mult <-function(X,b1==NULL){
  if(b1!=NULL){
    model <- list()
    I=diag(1,nrow=(8+b1))
    t1=cbind(-I,t(X))
    t2=c(rep(0,(8+b1)),rep(1,3))
    model$A=rbind(t1,t2)
    model$Q=matrix(0,nrow=sum(dim(X)),ncol=sum(dim(X)))
    model$Q[1:(8+b1),1:(8+b1)]=I
    model$rhs=c(rep(0,(8+b1)),1)
    model$sense=c(rep('=',(8+b1+1)))
    model$modelsense='min'
    model$obj=c(rep(0,sum(dim(X))))
    model$lb=c(rep(-Inf,(8+b1)),rep(0,3))
    log <- capture.output({ result=gurobi(model); })
    w=result$x[1:(8+b1)]
    #print(result$x)
  }else{
    model <- list()
    I=diag(1,nrow=8)
    t1=cbind(-I,t(X))
    t2=c(rep(0,8),rep(1,3))
    model$A=rbind(t1,t2)
    model$Q=matrix(0,nrow=11,ncol=11)
    model$Q[1:8,1:8]=I
    model$rhs=c(rep(0,8),1)
    model$sense=c(rep('=',9))
    model$modelsense='min'
    model$obj=c(rep(0,11))
    model$lb=c(rep(-Inf,8),rep(0,3))
    log <- capture.output({ result=gurobi(model); })
    w=result$x[1:8]
  }
  return(w)
}
