temp=c(rep(1,15),rep(2,15),rep(3,15),rep(4,15),rep(5,15),rep(6,15),rep(7,15),rep(8,15))
setEPS(width=10,height=6) #unable to add legend
postscript("sm1.eps")
pairs(storedres,col=temp,labels=c('Betweenness','PageRank','Degree'),cex.axis=1.5,oma=c(4,4,6,12))
par(xpd=TRUE)
legend(x=.9, y=0.7,title='Iterations',c('1-15','16-30','31-45','46-60','61-75','76-90','91-105','106-120'),col = 1:8,pch=1)
dev.off()
