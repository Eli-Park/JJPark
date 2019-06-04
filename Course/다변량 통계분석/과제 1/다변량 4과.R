library("tools")
library("HSAUR2")
library("MVA")
library("ggfortify")
demo("Ch-PCA")

share <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/GSP_SHARE.txt")
raw <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/GSP_RAW.txt")
gov <- data.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch04/GOVERNMENT_1.txt", fill =T, col.names = paste("X", 1:6)))
gov[upper.tri(gov)] <- t(gov)[upper.tri(gov)]
iris <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch04/IRIS.txt")
voc <- data.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch04/VOCATIONS.txt", fill =T, col.names = paste("X", 1:22)))
voc[upper.tri(voc)] <- t(voc)[upper.tri(voc)]
rec <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch04/RECORDS.txt")
drug <- data.matrix(read.table("C:/Users/pkmon/Desktop/다변량1/DATA/Ch04/DRUG_USE.txt", fill =T, col.names = paste("X", 1:13)))
drug[upper.tri(drug)] <- t(drug)[upper.tri(drug)]


#3
#(a)
X <- share[,2:14]
X <- as.matrix(X)
dimnames(X) <-list(share[,1], c("Agriculture", "Mining", "Construction", "MFR_DUR", "MFR_NON", "Transport", "Commun", "Utilities"
                                ,"Wholesale", "Retail", "Fiduciary", "Services", "Gov"))
corX <- cor(X) #R (correlation)을 구함
pca.share <- eigen(corX) #R로부터 eigen vector, Value 를 구함
plot(c(1:13), pca.share$values, type = "b", main="Scree Plot", xlab="component number", ylab = "Eigenvalues")
abline(h=1, col="red") #5번째 factor 까지 사용하는 것이 좋아보임 
pca.share$vectors #rotation matrix
round(pca.share$values, 2) 
round(cumsum(pca.share$values/sum(pca.share$values)), 2) #설명력
loadings.share <- pca.share$vectors %*% sqrt(diag(pca.share$values)) #loadings
apply(round(loadings.share, 2)**2, 2, sum)
scores <- scale(X)%*%pca.share$vectors #Score
plot(scores[,1],scores[,2], pch=3)
text(scores[,1],scores[,2], labels=rownames(X), pos=2)
abline(h=0, v=0)
pcashare$loadings



pcashare <- princomp(scale(X), retx=T, center=T, scale. = T)
summary(pcashare)
screeplot(pcashare, type="lines", main = "Scree Plot for GSP")
abline(h=1, col = "red")
scores
autoplot(pcashare, data = X, loadings = TRUE, label = TRUE, loadings.label=TRUE)
scores = pcashare$scores

plot(scores[,1],scores[,2], pch=3)
text(scores[,1],scores[,2], labels=rownames(X), pos=2)
abline(h=0, v=0)

?prcomp
#(b)
X1 <- X
rownames(X1) <- share[,1]
X1 <- X1[-50,] #Delete Wyoming
X1 <- X1[-2,] #Delete Alaska


corX1 <- cor(X1)
pca.share1 <- eigen(corX1)
round(pca.share1$values, 2)
round(pca.share1$values/sum(pca.share1$values), 2)
plot(c(1:13), pca.share1$values, type = "b", main="Scree Plot", xlab="component number", ylab = "Eigenvalues")
abline(h=1, col = "red")
loadings.share1 <- pca.share1$vectors %*% sqrt(diag(pca.share1$values))
round(loadings.share1, 2)


cm.2.pca1 <- apply(loadings.share1[,1:2]**2, 1, sum)
round(cm.2.pca1, 2)
var2.pca1 <- apply(loadings.share1**2, 2, sum)
round(var2.pca1, 2)

pcashare1 <- prcomp(X1, retx=T, center=T, scale. = T)
summary(pcashare1)
plot(c(1:13), pcashare1$sdev**2, type = "b", main="Scree Plot", xlab="component number", ylab = "Eigenvalues")
abline(h=1, col="red") #5번째 factor 까지 사용하는 것이 좋아보임 

autoplot(pcashare1, data = X1, loadings = TRUE, loadings.label=TRUE, label = TRUE)
scores = pcashare1$x
plot(scores[,1],scores[,2], pch=3)
text(scores[,1],scores[,2], labels=rownames(X), pos=2)
abline(v=0, h=0)

#4

X <- as.matrix(gov)
pca.gov <- eigen(X)
t(eigen(X)$vectors)%*%eigen(X)$vectors
round(pca.gov$values, 2)
loadings.gov <- pca.gov$vectors %*% sqrt(diag(pca.gov$values))
apply(round(loadings.gov,2)**2, 2, sum)
cm.2.gov <- apply(loadings.gov[,1:2]**2, 2, sum)
sum(round(cm.2.gov, 2))/6
pca.gov$vectors
pca <- prcomp(covmat = X)
summary(pca)
round(cumsum(pca.gov$values/sum(pca.gov$values)), 2)

var = eigen(X)$vectors
var
plot(var, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")
pca$loadings
pcagov <- prcomp(cov=X)
pcagov
pcagov$sdev
?prcomp
#5
X <- iris
X$V1 <- as.factor(X$V1)
colnames(X) <- c("species","sepal length", "sepal width", "petal length", "petal width")

#(a)
pairs(X[,2:5])
X <- as.matrix(X[2:5])
head(X)
corX <- cor(X)
corX
pca.iris <- princomp(X, retx=T, center=T, scale.=T)
summary(pca.iris)
pca.iris$loadings
autoplot(pca.iris,  data = X, loadings = TRUE, loadings.label=TRUE)
plot(var, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")

var <- eigen(corX)$values
rot <- eigen(corX)$vectors
apply((rot%*%diag(sqrt(var)))^2, 2, sum)

iris1 <- iris
iris1[,2:5] <- pca.iris$x
autoplot(pca.iris, X, loadings = TRUE, loadings.label=TRUE, labels = TRUE)


scores <- as.data.frame(cbind(species=iris[,1], pca.iris$scores))
mscores <- aggregate(scores[,2:5], list(scores$species), mean)
plot(mscores[,2], mscores[,3], xlab = "First PC", ylab="Second PC")
text(mscores[,2], mscores[,3], labels = mscores[,1])
abline(v=0,h=0)
mscores

attach(mscores)
plot(scores$Comp.1, scores$Comp.2)
points(Comp.1, Comp.2, col="red", pch=16)

#7

pca.voc <- princomp(cov = voc)
summary(pca.voc)
pca.voc$loadings
princomp
plot(1:22, pca.voc$sdev**2, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")
pca.voc$loadings[,1:10]

round(eigen(voc)$vectors%*%sqrt(diag(eigen(voc)$value)),2)
round(eigen(voc)$value^2/sum(eigen(voc)$value^2),2)
eigen(voc)$value
apply(m**2, 2, sum)
m <- round(eigen(voc)$vectors%*%sqrt(diag(eigen(voc)$value)),2)[1:7]
m

ms
plot(pca.voc$loadings[,1], pca.voc$loadings[,2])
text(pca.voc$loadings[,1], pca.voc$loadings[,2], labels = rownames(pca.voc$loadings))
abline(h=0, v=0)

#8
colnames(rec) <- c("country", paste("X", sep="", 1:8))
X <- as.matrix(rec[,2:9])
X<-scale(X)
pca.rec <- princomp(X, retx=T, center=T, scale.=T)
summary(pca.rec)
pca.rec$loadings
pca.rec$sdev

eigen(cov(X))
var <- pca.rec$sdev**2
plot(1:8, pca.rec$sdev**2, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")
autoplot(pca.rec, X, loadings = TRUE, loadings.label=TRUE, labels = TRUE)
pca.rec$loadings

var <- eigen(cor(X))$values
rot <- eigen(cor(X))$vectors
apply((rot%*%diag(sqrt(var)))^2, 2, sum)


#9
pca.drug <- princomp(cov=drug)
summary(pca.drug)
plot(1:13, pca.drug$sdev**2, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")

autoplot(pca.drug, loadings = TRUE, loadings.label=TRUE, labels = TRUE)
plot(pca.drug$loadings[,1], pca.drug$loadings[,2])
text(pca.drug$loadings[,1], pca.drug$loadings[,2]+0.01, labels = rownames(pca.drug$loadings))
abline(h=0, v=0)
