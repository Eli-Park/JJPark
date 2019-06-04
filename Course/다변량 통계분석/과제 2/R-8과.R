library(ggplot2)
library(cluster)
library(NbClust)
library(gridExtra)

#8.2
data <- read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch04/IRIS.txt")
colnames(data) <- c("Species", "sep.length", "sep.width", "pet.length", "pet.width")
data$Species <- as.factor(data$Species)
#(a)
str(data)
p1<-ggplot(data=data, aes(group=Species, y = sep.length))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  ggtitle("Boxplot of Sep.length by Species")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

p2<-ggplot(data=data, aes(group=Species, y = sep.width))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  ggtitle("Boxplot of Sep.width by Species")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

p3<-ggplot(data=data, aes(group=Species, y = pet.length))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  ggtitle("Boxplot of pet.length by Species")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

p4<-ggplot(data=data, aes(group=Species, y = pet.width))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  ggtitle("Boxplot of pet.width by Species")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2)



#Natural cluster 특징을 볼 수 있다.

#(b)
# Using hclust
par(mfrow=c(1,1))
iris2 <-cbind(scale(data[,2:5]), data[,1])
boxplot(iris2[,1:4])
hclust_iris<-hclust(dist(iris2[,1:4]), method="ward.D")
plotting <- plot(hclust_iris)
rect.hclust(hclust_iris, k=3)

# kmeans
kmean_iris<-kmeans(iris2[,1:4], 3)
clus=cbind(data ,kmean_iris$cluster)
table(data$Species, kmean_iris$cluster)
sum(diag(table(data$Species, kmean_iris$cluster)))/sum(table(data$Species, kmean_iris$cluster)) #분류율

identi
str(clus)
p1<-ggplot(data=clus, aes(x=sep.length, y = sep.width))+
  geom_point(aes(shape = as.factor(Species), color = as.factor(Species))) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle("Scatter Plot of Natural")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
p2<-ggplot(data=clus, aes(x=sep.length, y = sep.width))+
  geom_point(aes(shape = as.factor(kmean_iris$cluster), color = as.factor(kmean_iris$cluster))) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle("Scatter Plot of Kmeans")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
p3<-ggplot(data=clus, aes(x=pet.length, y = pet.width))+
  geom_point(aes(shape = as.factor(Species), color = as.factor(Species))) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle("Scatter Plot of Natural")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
p4<-ggplot(data=clus, aes(x=pet.length, y = pet.width))+
  geom_point(aes(shape = as.factor(kmean_iris$cluster), color = as.factor(kmean_iris$cluster))) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle("Scatter Plot Kmeans")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2)


#8.4
detach("package:ggplot2", unload=TRUE)
data2 <- read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch07/STORE_SHARE.txt")
colnames(data2) <- c(paste("S", c("03", "04", "07", "16", "18", "21", "24", "26", "29", "36", "10", "43", "45"), sep=""))
head(data2)
nc <- NbClust(data2, distance="euclidean", min.nc=2, max.nc=15, method = "ward.D")

par(mfrow=c(1,1))
d<- dist(data2, "euclidean")
h <- hclust(d, "ward.D")
plot(h)
rect.hclust(h, k=3)

n=nrow(data2)
withinss = pseudof = NULL
for(i in 1:20){
  km<-kmeans(data2, centers=i)
  withinss<-c(withinss, km$tot.withinss)
  f<-(km$betweenss/(i-1))/(km$tot.withinss/(n-i))
  pseudof<-c(pseudof,f)  
}
plot(pseudof, type="b", main='Pseudo-F plot')
kmean_store<-kmeans(data2, 3)
length(kmean_store$cluster)
clusplot(data2, kmean_store$cluster, color=TRUE, shade=TRUE, lines=0)

#8.5
par(mfrow=c(1,1))
shop<-read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch08/SHOPPING.txt", row.names=1)
colnames(shop) <- rownames(shop)
shop2 <- as.dist(shop)
shop2
h <- hclust(shop2, method = "ward.D")
plot(h)
rect.hclust(h, k =3)

##해석
#같은 클러스터 - 낮은 컴페티션
#다른 클러스터 - 높은 컴페티션
#S07이 가장 경쟁력 있는 스토어(ex.)

#8.8

book<-read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch09/FACTBOOK.txt")
book <- book[,2:6]
colnames(book) <- c("PENET", "PCYCLE", "PRICE", "PVTSH", "PUR/HH")
d<-dist(book, "euclidean")
hclust_book<-hclust(d, method="ward.D")
plot(hclust_book)                    
rect.hclust(hclust_book, 2)

#k=2 가 가장 나아보인다.
n=nrow(book)
withinss = pseudof = NULL
for(i in 1:20){
  km<-kmeans(book, centers=i)
  withinss<-c(withinss, km$tot.withinss)
  f<-(km$betweenss/(i-1))/(km$tot.withinss/(n-i))
  pseudof<-c(pseudof,f)  
}
plot(pseudof , type="b")
kmean_book<-kmeans(book, 2)
kmean_book
a
clusplot(book, kmean_book$cluster, color=TRUE, shade=TRUE, lines=0)

library("GGally")
a<-cbind(book, kmean_book$cluster)
ggpairs(book, aes(colour = as.factor(kmean_book$cluster), alpha = 0.4))

#3가지 변수들은 클러스터링에 의미있는 변수가 되지만, 나머지는 아닐 수 있다.
#8번의 저자의 말이 맞다.

#8.10

coffee<-read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch08/COFFEE.txt", row.names=1)
a <- rownames(coffee)
Fmat = matrix(0, nrow(coffee), ncol(coffee))
ni = apply(coffee, 1, sum)
nj = apply(coffee, 2, sum)
n = sum(coffee)
for(i in 1:nrow(coffee)) {
  for(j in 1:ncol(coffee)) {
    Fmat[i, j] <- coffee[i,j]/((ni[i]*nj[j])/n)
  }
}
dimnames(Fmat) <- list(a,a)
Fmat
Fmat <- as.dist(Fmat)
?as.dist
round(Fmat, 3)
hclust_coffee<-hclust(Fmat, method="ward.D")
plot(hclust_coffee)
rect.hclust(hclust_coffee, k=3)

#해석을 잘 해볼것!
kmean_coffee<-kmeans(dist(Fmat), 3)
kmean_coffee
clusplot(coffee, kmean_coffee$cluster, color=TRUE, shade=TRUE, lines=0)
