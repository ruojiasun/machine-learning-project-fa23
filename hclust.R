########################################
# Author: Ruojia Sun, adapted from code by Prof. Ami Gates
########################################

library(stats)
library(ggplot2)
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist
#install.packages("NbClust")
library(NbClust)
library(cluster)
library(mclust)
library(factoextra)
library(stylo)

library(ggplot2)
library(ggdendro)

setwd("C:/Users/sunru/Downloads/ml-project")
(NRI_SVI_Data<-read.csv("nri-svi-cleaned-2.csv"))

# # Distance Metric using dist
# (Eucl_Dist <- stats::dist(Data_Numeric,method="minkowski", p=2))

# Cosine Similarity
(CosSim <- stylo::dist.cosine(as.matrix(Data_Numeric)))

# Hierarchical Clustering
hc <- stats::hclust(CosSim, method="ward.D2")

# Plot
plot(hc, cex=.7, hang=-1,main = "Hierachical clustering dendrogram, k=2",labels=FALSE)
rect.hclust(hc, k=2)


# dend <- as.dendrogram(hc)
# par(mfrow=c(1,1))
# 
# plot(dend, main="Main tree")
# plot(cut(dend, h=100)$upper, 
#      main="Upper tree of cut at h=0.02")
# plot(cut(dend, h=0.02)$lower[[1]], 
#     main="Second branch of lower tree with cut at h=500")
