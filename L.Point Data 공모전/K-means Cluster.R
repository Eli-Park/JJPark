K <- data7
K <- K[,-1]
K[,2] <- as.factor(K[,2])
K <- centralImputation(K)
  
## Create dummy variables

library(dummies)

#x2.dum = dummy(zdungar$x2)
x2.dum = dummy(K$CLNT_GENDER)

head(x2.dum)

K = cbind(K, x2.dum)
head(K)

K2 = K[,-2]

install.packages("NbClust")
library(NbClust)

nc <- NbClust(K2, min.nc = 2, max.nc = 15, method = "kmeans")
########################
#### K-means clustering

k.clust = kmeans(K[,2:47], centers=8, nstart=20, iter.max=10)
k.clust$tot.withinss

k.clust2 = kmeans(zdungar2[,1:4], centers=3)
k.clust2$tot.withinss
str(data7)

## Showing the results

pie(k.clust$size, main="number of observations in segment")
k.clust$centers
dist(k.clust$centers, method = "euclidean", diag = TRUE)


plot3d(FA_RATIO, LE_RATIO, ST_RATIO, col=k.clust$cluster)
scatterplot3d(FA_RATIO, LE_RATIO, ST_RATIO, color=k.clust$cluster)


## Boxplot

seg1 = zdungar2[k.clust$cluster==1,]
seg2 = zdungar2[k.clust$cluster==2,]
seg3 = zdungar2[k.clust$cluster==3,]

par(mfrow=c(2,2))
boxplot(seg1[,1],seg2[,1],seg3[,1],ylab=names(seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,2],seg2[,2],seg3[,2],ylab=names(seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,3],seg2[,3],seg3[,3],ylab=names(seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,4],seg2[,4],seg3[,4],ylab=names(seg1)[4],xlab="segment",col="blue",names=c(1,2,3))
