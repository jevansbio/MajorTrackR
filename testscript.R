library(reticulate)
library(igraph)
use_python("C:\\WPy-3710\\python-3.7.1.amd64")

# generate networks ----
#Generate a network of 200 nodes with all edges
g1=erdos.renyi.game(n=200,p.or.m=1)
#Assign nodes to groups
V(g1)$name=1:200
V(g1)$group=rep(1:4,each=50)
E(g1)$weight = 0

#repeat the network
allnets=rep(list(g1),6)
#move some individuals between groups


V(allnets[[2]])$group[V(g1)$group==2]=3

V(allnets[[3]])$group[V(g1)$group==2][1:20]=3
V(allnets[[3]])$group[V(g1)$group==3][1:20]=2
V(allnets[[3]])$group[V(g1)$group==3][21:45]=5
V(allnets[[3]])$group[V(g1)$group==4][1:10]=1
V(allnets[[3]])$group[V(g1)$group==4][41:50]=2

V(allnets[[4]])$group[V(g1)$group==1][1:10]=4
V(allnets[[4]])$group[V(g1)$group==3][1:20]=2
V(allnets[[4]])$group[V(g1)$group==3][21:50]=5
V(allnets[[4]])$group[V(g1)$group==4][1:30]=1
V(allnets[[4]])$group[V(g1)$group==4][31:50]=2

V(allnets[[5]])$group[V(g1)$group==1][1:10]=4
V(allnets[[5]])$group[V(g1)$group==1][40:50]=5
V(allnets[[5]])$group[V(g1)$group==3][1:20]=2
V(allnets[[5]])$group[V(g1)$group==3][21:50]=5

#V(allnets[[6]])$group[V(g1)$group==3][1:20]=2
V(allnets[[6]])$group[V(g1)$group==3][30:50]=1


#alter edge weights depending on if nodes they are in the same group, delete 0 edges
for(i in 1:length(allnets)){
  currdatalist=as_data_frame(allnets[[i]],"both")
  currdata=as_long_data_frame(allnets[[i]])
  currdata$same_group=currdata$from_group==currdata$to_group
  currdata$group=0
  currdata$group[currdata$same_group]=currdata$from_group[currdata$same_group]
  currdata$weight[currdata$same_group]=sample(c(0.9,0.7),sum(currdata$same_group),replace=T,prob=c(0.5,0.5))
  currdata$weight[!currdata$same_group]=sample(c(0,0.05),sum(!currdata$same_group),replace=T,prob=c(0.95,0.05))
  currdata=currdata[currdata$weight>0,]
  currdata=currdata[order(currdata$weight),]
  currdata$timestep=i
  currdatalist$vertices$timestep=i
  allnets[[i]]=graph_from_data_frame(currdata,directed=F,currdatalist$vertices)
}



#Com detection ----
#detect each networks communities
coms=lapply(allnets,function(x){
  cluster_louvain(x)
})


#apply community membership as node attribute
allnets=lapply(1:length(allnets),function(x){
  V(allnets[[x]])$com=coms[[x]]$membership
  allnets[[x]]
})

#For convenience, just pull out the memberships
allcoms=lapply(1:length(allnets),function(x){
  coms[[x]]$membership
})


#Do MT ----
track <- do_track(allnets, allcoms, history=1)#run MajorTrack
