edu <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/EDUC_SCORES.txt")
ran <- read.table("C:/Users/pkmon/Desktop/다변량1/DATA/RANDOM_1.txt")

#2-3

rownames(edu) <- edu[,1]
#(a)
w <- c(0.25, 0.25, 0.5)
w <- w/sqrt(sum(w^2))
w

#(b)
X <- edu[,2:4]
X <- as.matrix(X)
colnames(X) <- c("X1", "X2", "X3")
z1 <- X%*%w
Xs<- scale(X)
z2 <- Xs%*%w
X
cbind(z1, z2)
round(Xs,3)
#(c)
cbind(z1, z2)
data.frame(z1 = z1[c(3,4),], z2 = z2[c(3,4),])

#2-4

#(a)
Xm <- edu[edu$V5==1,2:4]
Xm <- as.matrix(Xm)
Xm.bar <- apply(Xm, 2, mean)
names(Xm.bar) <- c("x1.bar","x2.bar","x3.bar")
Xm.bar
Xm.bar <- matrix(Xm.bar,nrow=nrow(Xm), ncol=3, byrow=T)
Xm-Xm.bar

Xf <- edu[edu$V5==0,2:4]
Xf <- as.matrix(Xf)
Xf.bar <- apply(Xf, 2, mean)
names(Xf.bar) <- c("x1.bar","x2.bar","x3.bar")
Xf.bar
Xf.bar <- matrix(Xf.bar,nrow=nrow(Xm), ncol=3, byrow=T)
Xf-Xf.bar

#(b)
Xmd <- Xm-Xm.bar
Sm = (1/(nrow(Xm)-1))*(t(Xmd)%*%(Xmd)); Sm
Xfd <- Xf-Xf.bar
Sf = (1/(nrow(Xf)-1))*(t(Xfd)%*%(Xfd)); Sf

Sm
Sf

#(c)


#2-5
W <- c(0.8, 0.6)
x4 <- W[1]*ran[,1] + W[2]*ran[,2]
x5 <- W[1]*ran[,1] + W[2]*ran[,3]
X <- cbind(x4, x5)
X
W <- matrix(c(0.866, 0.500, -0.500, 0.866), nrow = 2)
Z <- X%*%W
Z
#(a)
plot(Z, xlab = "z1", ylab="z2")
abline(h=0, v=0)

#(b)
cov(Z)
Z
S <- (1/(nrow(Z)-1))*(t(Z)%*%Z) #already mean-centered
S

#(c)
det(S)
det(cov(X))
det(W)

#2-6
X <- ran[,1:2]
X <- as.matrix(X)
W <- matrix(c(0.866, 0.500, 0.500, 0.866), nrow = 2)
Z <- X%*%W
#(a)
plot(Z, pch=16)
abline(v=0, h=0)

#(b)
n <- nrow(Z)-1
cov_Z = 1/(n-1) * t(Z)%*%Z
cov_Z

#(c)
det(cov_Z)
det(var(X))
det(W)

#determinant W의 값이 5번과 6번은 다름 