library(cluster)
library(readr)
library(corrplot)
library(factoextra)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

malldt <- read_csv("Mall_Customers.csv")

malldt$CustomerID <- NULL
malldt$Gender <- as.factor(malldt$Gender)

malldt.dist<-daisy(malldt,metric="euclidean",stand=TRUE)

malldt.hc.com<-hclust(malldt.dist,method="complete") 
plot(malldt.hc.com) 
rect.hclust(malldt.hc.com,k=3,border=c("red","green","blue")) 

malldt.hc.sin<-hclust(malldt.dist,method="single") 
plot(malldt.hc.sin) 
rect.hclust(malldt.hc.sin,k=3,border=c("red","green","blue")) 

malldt.hc.ave<-hclust(malldt.dist,method="average") 
plot(malldt.hc.ave) 
rect.hclust(malldt.hc.ave,k=3,border=c("red","green","blue")) 

malldt.hc.cen<-hclust(malldt.dist,method="centroid") 
plot(malldt.hc.cen) 
rect.hclust(malldt.hc.cen,k=3,border=c("red","green","blue")) 

malldt.hc.ward<-hclust(malldt.dist,method="ward") 
plot(malldt.hc.ward) 
rect.hclust(malldt.hc.ward,k=3,border=c("red","green","blue")) 


malldtstd<-scale(malldt[,-1])

set.seed(123)
k.max<-15 

wss<-sapply(1:k.max,function(k){kmeans(malldtstd,k,nstart=50,iter.max=15)$tot.withinss})

plot(1:k.max,wss,type="b",pch=19,xlab="Number of groups",ylab="Within Deviation",col="blue") 

kmeans4<-kmeans(malldt[,-1],4) # k=4 groups
kmeans6<-kmeans(malldt[,-1],6) # k=6 groups


ris4<-eclust(malldt[,-1],"kmeans",k=4) # evaluation of the clustering composition
fviz_silhouette(ris4) # dimensions and average of group's silhouette
sil4<-ris4$silinfo$widths # silhouette measure of each observation
neg_sil_index4<-which(sil4[,'sil_width']<0) # position of observation of silhouette<0
sil4[neg_sil_index4,] # observations with silhouette<0, that means that the observation should belong to the closest cluster

ris6<-eclust(malldt[,-1],"kmeans",k=6)
fviz_silhouette(ris6)
sil6<-ris6$silinfo$widths 
neg_sil_index6<-which(sil6[,'sil_width']<0)
sil6[neg_sil_index6,]
