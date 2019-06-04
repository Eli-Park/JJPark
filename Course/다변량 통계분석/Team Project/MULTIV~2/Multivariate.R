library("naniar")

setwd("C:/Users/pkmon/Desktop/Multivariate Team/MULTIV~2")
data <- read.csv("responses.csv")

data1 <- data[,1:73]  #PCA, EFA등 돌릴 변수
data2 <- data[,74:150] #추후 클러스터링 후에 각 클러스터 특징 분석시 사용할 변수

library("VIM")
library("Amelia")

aggr(data1,prop=FALSE,numbers=TRUE, cex.axis = par("cex"))
missmap(data1, y.cex = 0.5, x.cex = 0.5) #NA를 매핑 
Misscol <- apply(data1, 2, function(x) sum(is.na(x))) #열별 NA수
barplot(sort(Misscol))

Missrow <- apply(data1, 1, function(x) sum(is.na(x))) #행별 NA수
table(Missrow) #행 자체를 지우기에는 상당히 많은 행에 NA 분포

library("mice")
methods(mice) #Mice에서 사용하는 다중대입법의 종류들
imputed.data1 <- mice(data1, method = "rf", seed=1234)
imputed.data2 <- mice(data2, seed=1234)#mice package 이용, 5번 다중대입번 사용
imputed.data1$imp$Politics #실제로 어떤 값이 대입되었는지 보여줌
imputed.data1$method
complete.data1 <- complete(imputed.data1, 2) #2번째 값 사용
complete.data2 <- complete(imputed.data2, 2)
#complete.data1 <- complete(imputed.data1, sample(1:5,1)) #랜덤한 n번째 대입된 값 사용
#complete.data2 <- complete(imputed.data2, sample(1:5,1)) #랜덤한 n번째 대입된 값 사용
missmap(complete.data, y.cex = 0.5, x.cex = 0.5) #NA가 다 메꾸어짐

complete.data <- cbind(complete.data1, complete.data2)
table(complete.data$Gender)

# NA 처리된 것 이외의 소수 결측치 행 삭제
norow <- numeric()
temp <- 1
for(i in 1:nrow(complete.data)){
  
  if(sum(complete.data2[i,] == "")>=1) {
    norow[temp] <- i
    temp <- temp+1
  }  
}
norow 

complete.data <- complete.data[-norow,]
################################################

write.csv(complete.data, file="newresponse.csv", row.names =FALSE) #새로운 데이터셋 작성
complete.data <- read.csv("newresponse.csv")

data1 <- complete.data[,1:73]  #PCA, EFA등 돌릴 변수
data2 <- complete.data[,74:150] #추후 클러스터링 후에 각 클러스터 특징 분석시 사용할 변수

data11 <- data1[,1:19]
data12 <- data1[,20:31]
data13 <- data1[,32:63]
data14 <- data1[,64:73]
str(complete.data$Number.of.siblings)
#EDA 시각화
library("tidyverse")
library("ggplot2")
table(complete.data$Gender)
table(complete.data$Age)
table(complete.data$Education)
ggplot(complete.data, aes(x=Gender, fill=Gender))+
  geom_bar()
ggplot(complete.data, aes(x=Age, fill=Gender))+
  geom_bar()
ggplot(complete.data, aes(x=Age, fill=Education))+
  geom_bar()
ggplot(complete.data, aes(x=Only.child, fill = "black"))+
  geom_bar(fill = c("black", "white"))
ggplot(complete.data, aes(x=Village...town))+
  geom_bar(fill = c("orange", "green"))
ggplot(complete.data, aes(x=Weight, fill=Gender))+
  geom_bar()
ggplot(complete.data, aes(x=Height, fill=Gender))+
  geom_bar()

library("ggpubr")
a <- colnames(complete.data)
par(mfrow=c(1,1))
ggqqplot(complete.data[,5])

library(corrplot)
corr <- cor(data1)
corrplot(corr, method="ellipse", tl.cex = 0.7)

str(data2)

#PCA
PCA.data <- princomp(data1, scale=T)
detach(package:ggplot2)
summary(PCA.data)
plot(x=c(1:73),PCA.data$sdev^2, type="l")
points(PCA.data$sdev^2)
abline(h=1, col="red")
PCA.data11 <- princomp(data11, scale=T)
PCA.data12 <- princomp(data12, scale=T)
PCA.data13 <- princomp(data13, scale=T)
PCA.data14 <- princomp(data14, scale=T)
PCA.data11$scores
summary(PCA.data11)
summary(PCA.data12)
summary(PCA.data13)
summary(PCA.data14)




#EFA
options(max.print=1000000)
?factanal
data11 <- data1[,1:19]
data12 <- data1[,20:31]
data13 <- data1[,32:63]
data14 <- data1[,64:73]

EFA.data <- factanal(data1, 13)
EFA.data11 <- factanal(data11, 4, scores= "regression")
EFA.data12 <- factanal(data12, 3, scores= "regression")
EFA.data13 <- factanal(data13, 7, scores= "regression")
EFA.data14 <- factanal(data14, 3, scores= "regression")
EFA.data12$scores
EFA.data14

d<-cbind(EFA.data11$scores, EFA.data12$scores, EFA.data13$scores, EFA.data14$scores)
d<-scale(d)

library(NbClust)
nb <- NbClust(d, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

d<-scale(d)
a<- kmeans(d, 3)

Final<-cbind(d,data2,a$cluster)
f3 <- subset(Final, Final$`a$cluster`==3)
getmode(f3$Education)

##최빈
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}