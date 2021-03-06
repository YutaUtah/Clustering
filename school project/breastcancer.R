---
title: "breast cancer"
author: "Yuta Hayashi"
date: "4/19/2019"
output: pdf_document
---




library(readr)
wdbc <- read_csv("data.csv")



library(dplyr)
library(corrplot)



wdbc.data <- as.matrix(wdbc[,c(3:32)])
wdbc.data
row.names(wdbc.data) <- wdbc$id
diagnosis <- as.numeric(wdbc$diagnosis == "M") #turn the diagnosis variable to numeric
table(wdbc$diagnosis)
round(colMeans(wdbc.data),2) #round the numeric value to 2 digits

#get each standard deviation of the mean by 2 digits
roundSD <- function(x){
    round(sd(x), 2)
}

apply(wdbc.data, 2, roundSD)



corMatrix <- wdbc[,c(3:32)]
cNames <- c("rad_m","txt_m","per_m",
                 "are_m","smt_m","cmp_m","con_m",
                 "ccp_m","sym_m","frd_m",
                 "rad_se","txt_se","per_se","are_se","smt_se",
                 "cmp_se","con_se","ccp_se","sym_se",
                 "frd_se","rad_w","txt_w","per_w",
                 "are_w","smt_w","cmp_w","con_w",
                 "ccp_w","sym_w","frd_w")
colnames(corMatrix) <- cNames
#get the correlation plot 
M <- round(cor(corMatrix), 2)
corrplot(M, diag = FALSE, method="shade", order="FPC", tl.srt = 90)


####PCA analysis

Using PCA we can combine our many variables into different linear combinations that each explain a part of the variance of the model. By choosing only the linear combinations that provide a majority (>= 85%) of the co-variance, we can reduce the complexity of our model.

The first step in doing a PCA, is to ask ourselves whether or not the data should be scaled to unit variance. That is, to bring all the numeric variables to the same scale. In other words, we are trying to determine whether we should use a correlation matrix or a covariance matrix in our calculations of eigen values and eigen vectors (aka principal components).


wdbc.pcov <- princomp(wdbc.data, scores = TRUE)
summary(wdbc.pcov)


#create biplot


cex.before <- par("cex")
par(cex = 0.6)
biplot(wdbc.pcov)
par(cex = cex.before)



# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))
pr.cvar <- wdbc.pcov$sdev ^ 2
pve_cov <- pr.cvar/sum(pr.cvar)
# Eigen values
round(pr.cvar, 2)
# Percent variance explained
round(pve_cov, 2)
# Cummulative percent explained
round(cumsum(pve_cov), 2)

plot(pve_cov, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")



# Plot cumulative proportion of variance explained
plot(cumsum(pve_cov), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")


####The Scree-plots suggest that using a covariance matrix is not the correct approach for calculating the principal components. We will now use the correlation matrix.



####Running PCA using correlation matrix: 

When the correlation matrix is used to calculate the eigen values and eigen vectors, we use the prcomp() function.


wdbc.pr <- prcomp(wdbc.data, scale = TRUE, center = TRUE)
summary(wdbc.pr)



# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wdbc.pr$sdev ^ 2

# Assign names to the columns to be consistent with princomp.
# This is done for reporting purposes.
names(pr.var) <- names(pr.cvar)

# Variance explained by each principal component: pve
pve <- pr.var/sum(pr.var)

# Assign names to the columns as it is not done by default.
# This is done to be consistent with princomp.
names(pve) <- names(pve_cov)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")



# Scatter plot observations by components 1 and 2
plot(wdbc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))


There is a clear seperation of diagnosis (M or B) that is evident in the PC1 vs PC2 plot.

By using PCA we took a complex model of 30 (or more) predictors and condensed the model down to six linear combinations of the various predictors.
