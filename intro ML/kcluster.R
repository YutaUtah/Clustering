---
title: "cluster"
author: "Yuta Hayashi"
date: "4/13/2019"
output: pdf_document
---



we create 25 by 2 matrix with two clusters


set.seed (2)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25,1]=x[1:25 ,1]+3
x[1:25,2]=x[1:25 ,2]-4


perform kmean cluster where K=2


km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col =(km.out$cluster) , main=" K-Means Clustering Results with K=2" , xlab =" " , ylab=" " , pch =20, cex =2)


