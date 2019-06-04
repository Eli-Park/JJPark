setwd("C:/Users/pkmon/Desktop/Multivariate Team/Mulitvariate 0525")

### Multivariate_JW ###
complete.data <- read.csv("newresponse_0523.csv", header = T)

### DATA (150 col) ###
complete.data.a <- complete.data[,1:140] ## subjective
complete.data.b <- complete.data[,141:150] ## demographics

# smoking : 1(never smoked), 2(tried smoking), 3(former smoker), 4(current smoker)
# drinking : 1(never), 3(social drinker), 5(drink a lot)
# Punctuality : 1(i am often running late), 3(i am often early), 5(i am always on time)
# lying : 1(never), 2(only to avoid hurting someone), 3(sometimes), 4(everytime it suits me)
# Internet.usage : 1(no time at all), 2(less than an hour a day), 3(few hours a day), 4(most of the day)


#### Analyze each ####

### PCA ###
music <- complete.data[,1:19]
movies <- complete.data[,20:31]
hobbies <- complete.data[,32:63]
phobias <- complete.data[,64:73]
health <- complete.data[,74:76] 
personality <- complete.data[,77:133]
spending <- complete.data[,134:140]

### PCA ###

## music ##
pca.music <- princomp(music)
plot(c(1:length(music)), pca.music$sdev, type="b", main="Scree Plot",
     xlab="Component Number", ylab="Eigenvalues") 
summary(pca.music) #4PC

## movies ##
pca.movies <- princomp(movies)
plot(c(1:length(movies)), pca.movies$sdev, type="b", main="Scree Plot",
     xlab="Component Number", ylab="Eigenvalues") 
summary(pca.movies) #3PC

## hobbies ##
pca.hobbies <- princomp(hobbies)
plot(c(1:length(hobbies)), pca.hobbies$sdev, type="b", main="Scree Plot",
     xlab="Component Number", ylab="Eigenvalues") 
summary(pca.hobbies) #7PC

## phobias ##
pca.phobias <- princomp(phobias)
plot(c(1:length(phobias)), pca.phobias$sdev, type="b", main="Scree Plot",
     xlab="Component Number", ylab="Eigenvalues") 
summary(pca.phobias) #3PC

## health ##
#Use all 3

## personality ##
pca.personality <- princomp(personality)
plot(c(1:length(personality)), pca.personality$sdev, type="b", main="Scree Plot",
     xlab="Component Number", ylab="Eigenvalues") 
summary(pca.personality) #13PC

## spending ##
pca.spending <- princomp(spending)
plot(c(1:length(spending)), pca.spending$sdev, type="b", main="Scree Plot",
     xlab="Component Number", ylab="Eigenvalues") 
summary(pca.spending) #2PC


### EFA ###

## music ##
efa.music <- factanal(music, factors=4, rotation="varimax", scores="regression"); efa.music$scores

## movies ##
efa.movies <- factanal(movies, factors=3, rotation="varimax", scores="regression"); efa.movies$scores

## hobbies ##
efa.hobbies <- factanal(hobbies, factors=7, rotation="varimax", scores="regression"); efa.hobbies$scores

## phobias ##
efa.phobias <- factanal(phobias, factors=3, rotation="varimax", scores="regression"); efa.phobias$scores

## health ## -> scaling?
health.score <- scale(health); health.score

## personality ##
efa.personality <- factanal(personality, factors=13, rotation="varimax", scores="regression"); efa.personality$scores

## spending ##
efa.spending <- factanal(spending, factors=2, rotation="varimax", scores="regression"); efa.spending$scores



factorscores <- cbind(efa.music$scores, efa.movies$scores, efa.hobbies$scores, efa.phobias$scores, health.score, efa.personality$scores, efa.spending$scores)
colnames(factorscores) <- c("mus.quite", "mus.rock", "mus.noisy", "mus.pop",
                            "mov.fantasy", "mov.action", "mov.horror",
                            "hob.alone", "hob.science", "hob.liberalarts", "hob.IT", "hob.gossip", "hob.sports", "hob.math",
                            "pho.wild", "pho.animal", "pho.dog",
                            "heal.smoke", "heal.drink", "heal.lifestyle",
                            "per.social", "per.lonely", "per.work", "per.kind", "per.reliability", "per.HR", "per.angry",
                            "per.decisive&religious", "per.hypochondria", "per.family", "per.selfcriticism", "per.charity", "per.judgement",
                            "spend.on", "spend.at")

#################################################데이터셋은 그대로 썼습니다!#####################################################

### Clustering ###
library("cluster")
library("NbClust")
factorscores.s <- scale(factorscores)

#pseudo_F
str(factorscores.s)
n=nrow(factorscores.s)
withinss = pseudof = NULL
for(i in 1:20){
  km<-kmeans(factorscores.s, centers=i)
  withinss<-c(withinss, km$tot.withinss)
  f<-(km$betweenss/(i-1))/(km$tot.withinss/(n-i))
  pseudof<-c(pseudof,f)  
}
par(mfrow=c(1,1))
plot(pseudof, type="b", main='Pseudo-F plot')


library(tidyverse)
library(rgl)
library(GGally)

###How many variables?
##Using all variables
nb <- NbClust(factorscores.s, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=6, method = "kmeans", 
              index = "all", alphaBeale = 0.1) ## 2 clusters
par(mfrow=c(1,1))

factorscores.k$centers
?kmeans
factorscores.k <- kmeans(factorscores.s, 2, nstart = 100); factorscores.k
f <- as.data.frame(factorscores.s)
f <- cbind(f, complete.data.b, cluster = factorscores.k$cluster)
pc <- princomp(complete.data.a)
plot3d(pc$scores[,1:3], col=f$cluster) ###clus plot의 3d 버전

#cluster = 2

d.complete.data.a <- dist(complete.data.a, method = "euclidean")
plot(complete.data.a.ward <- hclust(d.complete.data.a, method="ward.D"), hang=-1,
     main="Cluster Dendrogram: Ward??s Method")
rect.hclust(complete.data.a.ward, 2)

cluster.h <- cutree(complete.data.a.ward, k=2)



#분포 및 시각
ggpairs(f[,1:4], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Music
ggpairs(f[,5:7], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Movie
ggpairs(f[,8:14], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Hobby
ggpairs(f[,15:17], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Phobia
ggpairs(f[,18:20], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Health
ggpairs(f[,21:33], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Personality
ggpairs(f[,34:35], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Spending
ggpairs(f[,36:45], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Demographic

ggpairs(f[,36:45], aes(colour = as.factor(cluster.h), alpha = 0.4)) #Demographic


table(factorscores.k$cluster, cluster.h)

#각 변수의 클러스터별 평균
A<-aggregate(f[, 1:35], list(f$cluster), mean); A
B<-aggregate(f[, 1:35], list(f$cluster), var); B
C<-colnames(A)

#Graph
par(mfrow=c(3,3))
for(i in 2:36) {
  barplot(A[,i], main = C[i])
}
par(mfrow=c(1,1))

#Cluster=2로도 잘 구분이 됨.

#Validation
a <- sample(nrow(f), 0.7*nrow(f))
tf <- f[a,]
vf <- f[-a,]

f <- as.data.frame(factorscores.s)
f <- cbind(f, complete.data.b, cluster = factorscores.k$cluster, cluster.h)

factorscores.k$centers
a <- kmeans(f, factorscores.k$centers)

b <- kmeans(vf, 2, nstart = 100)


?sample


## or ~phobia
nb <- NbClust(factorscores.s[,1:17], diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=6, method = "kmeans", 
              index = "all", alphaBeale = 0.1) ## 2 clusters

par(mfrow=c(1,1))
factorscores.k <- kmeans(factorscores.s[,1:17], 3, nstart = 100); factorscores.k
f <- as.data.frame(factorscores.s)
f <- cbind(f, complete.data.b, cluster = factorscores.k$cluster)
pc <- princomp(complete.data.a)
plot3d(pc$scores[,1:3], col=f$cluster) #잘 구분이 된 것이 보인다.



#cluster = 3

d.complete.data.a <- dist(complete.data.a, method = "euclidean")
plot(complete.data.a.ward <- hclust(d.complete.data.a, method="ward.D"), hang=-1,
     main="Cluster Dendrogram: Ward??s Method")
rect.hclust(complete.data.a.ward, 3)

f <- as.data.frame(factorscores.s)
f <- cbind(f, complete.data.b, cluster = factorscores.k$cluster)

ggpairs(f[,1:4], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Music
ggpairs(f[,5:7], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Movie
ggpairs(f[,8:14], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Hobby
ggpairs(f[,15:17], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Phobia
ggpairs(f[,18:20], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Health
ggpairs(f[,21:33], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Personality
ggpairs(f[,34:35], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Spending
ggpairs(f[,36:45], aes(colour = as.factor(factorscores.k$cluster), alpha = 0.4)) #Demographic

A<-aggregate(f[, 1:35], list(f$cluster), mean); A
B<-aggregate(f[, 1:35], list(f$cluster), var); B
C<-colnames(A)

par(mfrow=c(3,3))
for(i in 2:36) {
  barplot(A[,i], main = C[i])
}

par(mfrow=c(1,1))

#Cluster =3 로도 잘 구분이 된 것을 알 수 있다.