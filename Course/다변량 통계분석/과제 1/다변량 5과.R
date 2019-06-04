library("tools")
library("HSAUR2")
library("MVA")
library("ggfortify")
library("corrplot")

#4
#(a)
six <- as.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch05/SIX_VARIABLES.txt"))
colnames(six) <- paste("X", 1:6, sep="")

corrplot(cor(six))
pca.six <- princomp(six, retx=T, center=T, scale.=T)
summary(pca.six)
screeplot(pca.six, type="lines", main = "Scree Plot for Six")
abline(h=1, col = "red")

var<-pca.six$sdev**2
plot(var, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")

##3variables

#(b)
efa.six <- factanal(six, factors=3, rotation="varimax")
efa.six
communality <- apply(efa.six$loadings**2, 1, sum) ; communality

var.fa <- apply(efa.six$loadings^2, 2, sum) ; var.fa
sum(var.fa)/6

#(c)
efa.six2 <- factanal(six, factors = 2, rotation = "varimax")
communality.2 <- apply(efa.six2$loadings**2, 1, sum) ; communality.2
efa.six2
efa.six3 <- factanal(six, factors=3, rotation="varimax")
efa.six3
communality.3 <- apply(efa.six3$loadings**2, 1, sum) ; communality.3


#두개 보다는 세개의 factor 이용하는 것이 더 대표성이 있다.

#(d)
efa.six4 = factanal(six, factors = 3, rotation = "varimax", scores = "Bartlett")
head(efa.six4$scores, 2)
head(round(efa.six4$scores, 3), 2)
six[1:2,]


#6
#(a)
foda <- as.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch05/FOOD_RESEARCH_A.txt"))
foda <- foda[,-1]
colnames(foda) <- paste("X", 1:10, sep="")
corrplot(cor(foda))
pca.foda <- prcomp(foda, retx=T, center=T, scale.=T)
summary(pca.foda)
screeplot(pca.foda, type = "lines", main = "Scree plot")
abline(h=1, col = "red")

efa.foda <- factanal(foda, factors=3, rotation="varimax", score="regression")
efa.foda
#f1 : X3, 4, 7, 10
#f2 : X2, 5, 8
#f3 : X1, 9 ->where is V6 ??

efa.foda$loadings
communality <-  apply(efa.foda$loadings^2, 1, sum)
round(communality,3)
efa.foda$scores
#X6 communiality is  very low. this mean is X6 can't be explained by 3 factor.
#but other factors are well explained.

efa.foda2 <- factanal(foda, factors=4, rotation="varimax", score="regression")
efa.foda2
efa.foda2$loadings
communality2 <-  apply(efa.foda2$loadings^2, 1, sum)
round(communality2,3)

#애매한 factor를 4개 써서 비교하는 것도 좋음


#(b)
fodb <- as.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch05/FOOD_RESEARCH_B.txt"))
fodb <- fodb[,-1]
colnames(fodb) <- paste("X", 1:10, sep="")
corrplot(cor(fodb))

pca.fodb <- princomp(scale(fodb), retx=T, center=T, scale.=T)
summary(pca.fodb)
screeplot(pca.fodb, type = "lines", main = "Scree plot")
abline(h=1, col = "red")


efa.fodb <- factanal(fodb, factors=3, rotation="varimax")
efa.fodb
efa.fodb$loadings
communalityb <-  apply(efa.fodb$loadings^2, 1, sum)
round(communalityb,3)

comdif <- round(rbind(communality, communalityb), 3)
comdif
#1번 변수, 9번째 변수에서만 차이가 있음 -> 1, 9번 변수가 바뀌었을 가능성이 있다!


#7
#(a)
mba <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch05/MBA_CAR_ATTRIB.txt")
n <- nrow(mba)
a <- numeric()
for(i in 1:n) {
  if(sum(mba[i,]==".")) a[i] <- i 
}
a <- unique(a)
a <- a[-1]
mba <- mba[-a,]
colnames(mba) <- c("SID", "CID", paste("X", 1:16, sep=""))

X <- mba[,3:18]
for(i in 1:16){
  X[,i] <- as.numeric(X[,i])
}
X <- as.matrix(X)
pca.mba <- princomp(X)
summary(pca.mba)
corrplot(cor(X))

efa.mba <- factanal(X, factors=3, rotation = "varimax", score="Bartlett")
1-efa.mba$uniquenesses
print(efa.mba$loadings, cutoff=0)
#Factor1은 브랜드 이미지 
#2는 durability(안정성)
#3은 Outdoor 야외활동

#(b)
brands <- mba[,2]
brands
brandsnam <- c(1:10)
brandsname <- c("BMW_328i", "Ford_Explorer", "Infiniti_J30", "Jeep_Grand_Cheroke", "Lexus_ES300", "Chrysler_Town_&_Country",
                "Mercedes_C280", "Saab_9000", "Porsche_Boxster", "Volvo_V90")
n.brands <- 10         
avg.fscore <- matrix(0, 10, 4)
dimnames(avg.fscore) <- list(brandsname, c("n.obs", "F1", "F2", "F3"))
for(i in 1:n.brands) {
  w <- brands == brandsnam[i]
  avg.fscore[i,1] <- sum(w)
  avg.fscore[i,2:4] <- apply(efa.mba$scores[w,], 2, mean)
}
colnames(avg.fscore) <- c("obs", "Brand", "Durability", "Outdoor")
avg.fscore

par(mfrow=c(1,1))
plot(avg.fscore[,2], avg.fscore[,3], main="", xlim=c(-2, 2), ylim=c(-2, 2),
     xlab="Brand", ylab="Durability", pch=16)
text(avg.fscore[,2]+.07, avg.fscore[,3]+.07, brandsname, col="black", cex=0.7)
abline(h=0, v=0)
plot(avg.fscore[,2], avg.fscore[,4], main="", xlim=c(-2, 2), ylim=c(-2, 2),
     xlab="Brand", ylab="Outdoor")
text(avg.fscore[,2]+.07, avg.fscore[,4]+.07, brandsname, col="black", cex=0.7)
abline(h=0, v=0)
plot(avg.fscore[,3], avg.fscore[,4], main="", xlim=c(-2, 2), ylim=c(-2, 2),
     xlab="Durability", ylab="Outdoor")
text(avg.fscore[,3]+.07, avg.fscore[,4]+.07, brandsname, col="black", cex=0.7)
abline(h=0, v=0)

#8
emo <- data.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch05/EMOTIONS.txt", fill =T, col.names = paste("X", 1:10, sep="")))
emo
emo[upper.tri(emo)] <- t(emo)[upper.tri(emo)]
corrplot(emo)
emo
pca.emo <- princomp(cov=emo)
summary(pca.emo)
screeplot(pca.emo, type = "lines", main = "Scree plot")
abline(h=1, col = "red")


efa.emo <- factanal(factors=4, covmat=emo, rotation = "varimax")
efa.emo

efa.emo$loadings
communality <- round(apply(efa.emo$loadings**2,1,sum),3); communality
#3개의 factor로는 interest, joy variable by using 3 factor