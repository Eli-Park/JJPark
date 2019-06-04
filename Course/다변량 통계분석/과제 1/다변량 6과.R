library("tools")
library("HSAUR2")
library("MVA")
library("sem")

# 6.1
lam<-c(0.725,0.65,0.58,0.605,0.81)
theta<-c(.474,.578,.664,.634,.344)
sum(lam^2)/(sum(lam^2)+sum(theta^2))

# 6.2
lam <- c(0.78, 0.69, 0.73, 0.75, 0.84, 0.72)
corr <- 0.36

#(a)
lam[2]*corr*lam[5]

#(b)

# 6.4
AT <- data.matrix(read.table("C:/Users/pkmon/Desktop/19-1/다변량/DATA/Ch06/ATTITUDE.txt", fill =T, col.names = paste("X", 1:6)))
AT[upper.tri(AT)] <- t(AT)[upper.tri(AT)]

dimnames(AT) <- list(paste("X", 1:6, sep=""), paste("X", 1:6, sep=""))
AT
#(A)
model1 <- specifyModel()
  A1 -> X1, lam11, NA 
  A1 -> X2, lam21, NA
  A1 -> X3, lam31, NA
  A2 -> X4, lam42, NA
  A2 -> X5, lam52, NA
  A2 -> X6, lam62, NA
  X1 <-> X1, theta11, NA
  X2 <-> X2, theta22, NA
  X3 <-> X3, theta33, NA
  X4 <-> X4, theta44, NA
  X5 <-> X5, theta55, NA
  X6 <-> X6, theta66, NA
  X1 <-> X4, theta14, NA
  X2 <-> X5, theta25, NA
  X3 <-> X6, theta36, NA
  A1 <-> A1, NA, 1
  A2 <-> A2, NA, 1
  A1 <-> A2, phi12, NA
  

#Model
cfa1 <- sem(model1, S=AT, N=100)
summary(cfa1)

library("DiagrammeR")
pathDiagram(cfa1, style="ram", ignore.double = FALSE, edge.labels="values")
pathDiagram(cfa1, style="traditional", 
              ignore.double = FALSE, error.nodes = TRUE,
              edge.labels="values", 
              min.rank=c("A1", "A2"), 
              same.rank=c("X1", "X2", "X3", "X4", "X5", "X6"),
              max.rank=c("X1.error","X2.error","X3.error","X4.error","X5.error","X6.error"))

#(b)
library("sem")
model1 <- specifyModel()
A1 -> X1, lam11, NA 
A1 -> X2, lam21, NA
A1 -> X3, lam31, NA
A2 -> X4, lam42, NA
A2 -> X5, lam52, NA
A2 -> X6, lam62, NA
X1 <-> X1, theta11, NA
X2 <-> X2, theta22, NA
X3 <-> X3, theta33, NA
X4 <-> X4, theta44, NA
X5 <-> X5, theta55, NA
X6 <-> X6, theta66, NA
X1 <-> X4, theta14, NA
X2 <-> X5, theta25, NA
X3 <-> X6, theta36, NA
A1 <-> A1, NA, 1
A2 <-> A2, NA, 1


#Model
cfa1 <- sem(model1, S=AT, N=100)
summary(cfa1)
pathDiagram(cfa1, style="traditional", 
            ignore.double = FALSE, error.nodes = TRUE,
            edge.labels="values", 
            min.rank=c("A1", "A2"), 
            same.rank=c("X1", "X2", "X3", "X4", "X5", "X6"),
            max.rank=c("X1.error","X2.error","X3.error","X4.error","X5.error","X6.error"))



#(c)
model1 <- specifyModel()
A1 -> X1, lam11, NA 
A1 -> X2, lam21, NA
A1 -> X3, lam31, NA
A2 -> X4, lam42, NA
A2 -> X5, lam52, NA
A2 -> X6, lam62, NA
X1 <-> X1, theta11, NA
X2 <-> X2, theta22, NA
X3 <-> X3, theta33, NA
X4 <-> X4, theta44, NA
X5 <-> X5, theta55, NA
X6 <-> X6, theta66, NA
A1 <-> A1, NA, 1
A2 <-> A2, NA, 1
A1 <-> A2, phi12, NA

#Model
options(fit.indices=c("GFI", "AGFI", "RMSEA", "AIC", "BIC", "CAIC"))
cfa1 <- sem(model1, S=AT, N=100)
summary(cfa1)
pathDiagram(cfa1, style="traditional", 
            ignore.double = FALSE, error.nodes = TRUE,
            edge.labels="values", 
            min.rank=c("A1", "A2"), 
            same.rank=c("X1", "X2", "X3", "X4", "X5", "X6"),
            max.rank=c("X1.error","X2.error","X3.error","X4.error","X5.error","X6.error"))


#(d)
pca1 <- princomp(cov=AT, retx=T, center=T, scale.=T)
pca1
summary(pca1)
plot(pca1$sdev, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")
eg <- eigen(AT)
loading <- eg$vectors%*%sqrt(diag(eg$values))
loading[,1:2]


##6.6
gov <- data.matrix(read.table("C:/Users/pkmon/Desktop/19-1/다변량/DATA/Ch04/GOVERNMENT_1.txt", fill =T, col.names = paste("X", 1:6)))
gov[upper.tri(gov)] <- t(gov)[upper.tri(gov)]
dimnames(gov) <- list(paste("X", 1:6, sep=""), paste("X", 1:6, sep=""))

corrplot(gov)
#PCA
X <- as.matrix(gov)
pca.gov <- eigen(X)
t(eigen(X)$vectors)%*%eigen(X)$vectors
round(pca.gov$values, 2)
loadings.gov <- pca.gov$vectors %*% sqrt(diag(pca.gov$values))
loadings.gov
apply(round(loadings.gov,2)**2, 2, sum)
cm.2.gov <- apply(loadings.gov[,1:2]**2, 2, sum)
sum(round(cm.2.gov, 2))/6
pca <- princomp(covmat = X)
summary(pca)
round(cumsum(pca.gov$values/sum(pca.gov$values)), 2)

plot(pca$sdev**2, type ="b", main="Scree plot", xlab = "Factors", ylab = "Eigen Value")
abline(h=1, col = "red")

#EFA
efa1 <- factanal(cov=gov, factors=3,rotation="promax")
communality <- apply(efa1$loadings**2, 1, sum) ; communality

efa2 <- factanal(cov=gov, factors=2,rotation="promax")
efa2
communality <- apply(efa2$loadings**2, 1, sum) ; communality

#CFA
model1 <- specifyModel()
  A1 -> X1, lam11, NA 
  A1 -> X2, lam21, NA
  A1 -> X3, lam31, NA
  A2 -> X4, lam42, NA
  A2 -> X5, lam52, NA
  A2 -> X6, lam62, NA
  X1 <-> X1, theta11, NA
  X2 <-> X2, theta22, NA
  X3 <-> X3, theta33, NA
  X4 <-> X4, theta44, NA
  X5 <-> X5, theta55, NA
  X6 <-> X6, theta66, NA
  A1 <-> A1, NA, 1
  A2 <-> A2, NA, 1
  A1 <-> A2, phi12, NA
  #model
cfa1 <- sem(model1, S=gov, N=100, optimizer=optimizerOptim)
summary(cfa1)
cfa1
pathDiagram(cfa1, style="ram", ignore.double = FALSE, edge.labels="values")
pathDiagram(cfa1, style="traditional", 
            ignore.double = FALSE, error.nodes = TRUE,
            edge.labels="values", 
            min.rank=c("A1", "A2"), 
            same.rank=c("X1", "X2", "X3", "X4", "X5", "X6"),
            max.rank=c("X1.error","X2.error","X3.error","X4.error","X5.error","X6.error"))


  #(c)
gov1 <- data.matrix(read.table("C:/Users/pkmon/Desktop/19-1/다변량/DATA/Ch06/GOVERNMENT_2.txt", fill =T, col.names = paste("X", 1:6)))
gov1[upper.tri(gov1)] <- t(gov1)[upper.tri(gov1)]
dimnames(gov1) <- list(paste("X", 1:6, sep=""), paste("X", 1:6, sep=""))
cfa2 <- sem(model1, S=gov1, N=100)
summary(cfa2)
pathDiagram(cfa2, style="ram", ignore.double = FALSE, edge.labels="values")





#6.8
cog <- read.table("C:/Users/pkmon/Desktop/19-1/다변량/DATA/Ch04/COGNITION.txt")
cog <- cog[,-1]
temp<-0
rows<-numeric()
for(i in 1:201){
  if(sum(cog[i,]==".")>=1) {
    temp <- temp+1
    rows[temp] <- i
  }
}
cog <- cog[-rows,]
for(i in 1:18){
  cog[,i] <- as.numeric(cog[,i])
}

#(a)
cov_cog <- cov(cog)
dimnames(cov_cog) <- list(paste("X", 1:18, sep = ""),paste("X", 1:18, sep = ""))
corrplot(cor(cov_cog))

model1 <- specifyModel()
A1 -> X1, lam1, NA 
A1 -> X2, lam2, NA
A1 -> X3, lam3, NA
A1 -> X4, lam4, NA
A1 -> X5, lam5, NA 
A1 -> X6, lam6, NA
A1 -> X7, lam7, NA
A1 -> X8, lam8, NA
A1 -> X9, lam9, NA 
A1 -> X10, lam10, NA
A1 -> X11, lam11, NA
A1 -> X12, lam12, NA
A1 -> X13, lam13, NA 
A1 -> X14, lam14, NA
A1 -> X15, lam15, NA
A1 -> X16, lam16, NA
A1 -> X17, lam17, NA
A1 -> X18, lam18, NA
X1 <-> X1, theta1, NA
X2 <-> X2, theta2, NA
X3 <-> X3, theta3, NA
X4 <-> X4, theta4, NA
X5 <-> X5, theta5, NA
X6 <-> X6, theta6, NA
X7 <-> X7, theta7, NA
X8 <-> X8, theta8, NA
X9 <-> X9, theta9, NA
X10 <-> X10, theta10, NA
X11 <-> X11, theta11, NA
X12 <-> X12, theta12, NA
X13 <-> X13, theta13, NA
X14 <-> X14, theta14, NA
X15 <-> X15, theta15, NA
X16 <-> X16, theta16, NA
X17 <-> X17, theta17, NA
X18 <-> X18, theta18, NA
A1 <-> A1, NA, 1

#model test
cfa3 <- sem(model1, S=cov_cog, N=195)
summary(cfa3)
pathDiagram(cfa3, style="ram", ignore.double = FALSE, edge.labels="values")

a<-sum(cfa3$coeff[1:18])
b<-sum(cfa3$coeff[19:36]^2)
pc2 <- a^2/(a^2+b) 
pc2

#(b)
cor(cog)
solve(cor(cog))%*%matrix(cfa3$coeff[1:18],18,1)
cfa3$coeff[1:18]
