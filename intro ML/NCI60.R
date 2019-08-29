---
title: "NCI60 pca analysis"
author: "Yuta Hayashi"
date: "4/13/2019"
output: html_document
---


library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)


We begin by examining the cancer types for the cell lines.


nci.labs [1:4]
table(nci.labs)


We ﬁrst perform PCA on the data after scaling the variables (genes) to have standard deviation one, although one could reasonably argue that it is better not to scale the genes.


pr.out =prcomp (nci.data , scale=TRUE)
Cols=function (vec){
   cols=rainbow (length(unique(vec)))
   return (cols[as.numeric(as.factor(vec))])
}



par(mfrow =c(1,2)) #graph design
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch =19,xlab =" Z1" ,ylab=" Z2" )
plot(pr.out$x[,c(1,3)], col =Cols(nci.labs), pch =19,xlab =" Z1" ,ylab=" Z3" )



pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)
par(mfrow =c(1,2))
plot(pve,type ="o" , ylab=" PVE " , xlab="Principal Component" ,col =" blue" )
plot(cumsum(pve), type="o" ,
ylab =" Cumulative PVE",xlab="Principal Component ", col =" brown3 " )


