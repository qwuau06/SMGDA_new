library("igraph")
library("parallel")
library("mco")
library("gurobi")
#rm(list=ls())
source("action_without_cluster.R")
#source("test action.R")
pm <- cbind( c(.2, .005), c(.005, .15) )
#sample_sbm(100, pref.matrix=pm, block.sizes=c(30,70))
#adjnoun=read.graph("Data/adjnoun/adjnoun.gml","gml")
#football=read.graph("Data/football/football.gml","gml")
#static.power.law.game(100,500,3,-1)
#read = erdos.renyi.game(100,500,'gnm')
#watts.strogatz.game(1,100,5,.15)
#read = ba.game(100,1,5,,directed=FALSE)
read = forest.fire.game(100,.38,directed=FALSE)
#read=read.graph('Data/dolphins/dolphins.gml','gml')
#read=read.graph('Data/lesmis/lesmis.gml','gml')
#polbooks=read.graph('Data/polbooks/polbooks.gml','gml')
#read1=barabasi.game(100,1,5,,directed=FALSE)
#read=read.graph('Data/lesmis/lesmis.gml','gml')
m<<-ecount(read)
n<<-vcount(read)
g=sampleGraph(read)
tar.bw<<-betweenness(read)
tar.dd<<-degree(read)
tar.pr<<-page.rank(read)$vector
input=rep(list(g),detectCores())
runs<<-length(input)
