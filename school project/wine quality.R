---
title: "wine quality"
author: "Yuta Hayashi"
date: "4/12/2019"
output: pdf_document
---




library(dplyr)          # For data manipulation ,filtering etc
library(magrittr)       # For pipes
library(caret)          # For dummy variables, nearZeroVar(), findCorrleation()
library(ggplot2)        # For plotting
library(ggthemes)       # For a variety of plot-themes
library(gridExtra)      # Arranging ggplots in a grid
library(lattice)
library(vegan)
library(NbClust)
library(cluster)        # For silhoutte()
library(factoextra)     # get_clust_tendency() assesses hopkins stat
library(clustertend)    # Another package for hopkins() function
library(data.table)
library(GGally)
library(ggcorrplot)
library(mclust)
library(fpc)
library(ISLR)


#####We import the dataset "wine quality"

wineData <- read.csv("winequality.csv")



print("---Data preview---")
head(wineData)

print("---Number of rows---")
nrow(wineData)

print("---Column names---")
#Names of all columns
names(wineData)


#####Checking values in Columns "Color", "Quality" and "Good". Might be helpful in subsequest iterations.

print("---Values in color column")
unique(wineData$color)

print("---Values in good column")
unique(wineData$good)

print("---Values in quality column")
unique(wineData$quality)


#####change factor variables to numeric since clustering can be only executed through numeric variables


wineData$quality <- as.numeric(wineData$quality)
wineData$good <- as.numeric(wineData$good)

#for ease creating a data set without color column
wineData_no_color <- wineData[1:13]


#####We create two histograms of both red and white wines versus quality


ggplot(wineData, aes(quality,fill=color, color=c("red", "white"))) +
    geom_histogram(binwidth = .5,col="black") +  
        facet_grid(color ~ .)+
        labs(title="Histogram Showing Qulity of Wine", 
        subtitle="Wine Quality across Red and White colors of Wine")


#####show all scatterplot for all quality versus explanatory variable plots

for(i in 1:11){
    print(paste("---Plot for---", colnames(wineData)[i]))

#Overall distribution
  print(ggplot(wineData, aes_string("quality", colnames(wineData)[i]))+
        geom_count(col='blue1', show.legend=FALSE))

#Color wise scatter plot 
  print(ggplot(wineData, aes_string("quality", colnames(wineData)[i]))+
        geom_jitter(aes(col=as.factor(color))))
}




##data cleaning


#####By looking at data we can say that Columns 1, 4, 6, 7, 9, 11, 12 can cause issues, we will try to normalize them so that there value are in 0 to 1 range.


#normilizing data
norm_data <- sapply(wineData[,c(1,4,6,7,9,11,12)], function(x) (x - min(x))/(max(x) - min(x)))

#converting data from matrix to data.frame
norm_data <- data.frame(norm_data)    # norm_data is a 'matrix'

#Binding the normalised data with other data
wineData_norm <- cbind(wineData[,c(2,3,5,8,10,13)],norm_data)
head(wineData_norm)




#####next step is to find highly correlated variables
```{r,echo=FALSE,fig.height=3,fig.width=4}
corr_norm <- round(cor(wineData_norm),1)
```


ggcorrplot(corr_norm, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation of Wine Data")


#####find a variable that is too correlated to quality of wine

#Let us calculate it thought data.
#Again we will make use of Caret Library.
a<-findCorrelation(corr_norm, cutoff = 0.7, verbose = T)
#Returns an integer value

print("--- Columns number---")
a

print("---Column name we want to remove---")
colnames(wineData_norm)[a]


#####we remove the "good" variable


wineData_norm$good <- NULL


##clustering: find a number of clusters

#####In order to find a number of clusters there are a lot of methods to conducts. However, in this analysis we use Silhouette method to figure out which number of clusters is appropriate. 

#####first of all, we extract a portion of the data to attain the training data


wine_train_data <- sample_frac(wineData_norm, 0.5)


#####we plot the graph of a number of clusters and average silhouette width using NbCluster


fviz_nbclust(wine_train_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


#####since we attained the most average sihouette width with a number of clusters 2, we use 2 clusters in analysis.


km <- kmeans(wineData_norm, 2, iter.max = 140 , algorithm="Lloyd", nstart=100)


#####we visualize analysis of data using results from K-mean. We will try to plot different graphs for better understanding.



fviz_cluster(km, data = wineData_norm,
             ellipse.type = "convex",
             palette = "jco"
             )


#####we need to know if this clustering process is valid. In order to do it, we produce a random dta set and compare the results each other.


random_df <- apply(wineData_norm, 2, 
                function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)



# Plot faithful data set
fviz_pca_ind(prcomp(wineData_norm), title = "PCA - Wine data", 
             habillage = wineData$color,  palette = "jco",
             geom = "point", legend = "bottom")
# Plot the random df
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data", 
             geom = "point")


####Here, we present two R functions / packages to statistically evaluate clustering tendency by computing the Hopkins statistics:

#####get_clust_tendency() function [in factoextra package]. It returns the Hopkins statistics as defined in the formula above. The result is a list containing two elements:

#####hopkins_stat


# Compute Hopkins statistic for Wine dataset
res <- get_clust_tendency(head(wine_train_data, 500), n = 499, graph = TRUE)
res$hopkins_stat
plot(res$plot)



# Compute Hopkins statistic for random dataset
res <- get_clust_tendency(head(random_df, 500), n = 499, graph = TRUE)
res$hopkins_stat
plot(res$plot)


#####second graph has a highr Honpkins value and this proves that this clustering method successfully works.

