library(ggplot2)
library("gcookbook")
library("agricolae")
library("faraway")
library(rgl)

#1
#a, b
A <- rep(c(-1, 1, -1, 1, -1, 1, -1, 1), 3)
B <- rep(c(-1, -1, 1, 1, -1, -1, 1, 1), 3)
C <- rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 3)
Rep <- rep(c(1, 2, 3), each = 8)
Y <- c(22, 32, 35, 55, 44, 40, 60, 39, 31, 43, 34, 47, 45, 37, 50, 41,
       25, 29, 50, 46, 38, 36, 54, 47)
hw4.1<-as.data.frame(cbind(A, B, C, Rep, Y))

x1  <- hw4.1$A
x2  <- hw4.1$B
x3  <- hw4.1$C
x12  <- x1*x2
x13  <- x1*x3
x23  <- x2*x3
x123 <- x1*x2*x3


Name <- c("int","A","B","C","AB","AC","BC", "ABC")

design <- matrix(cbind(rep(1,length(x1)),x1,x2,x3,x12,x13,x23,x123), nrow = length(x1))
Factor_Effect <- (t(design)%*%Y)/(4*3)

Effect <- as.data.frame(cbind(Name, Factor_Effect))
Effect
qqnorm(Factor_Effect[-1])
qqline(Factor_Effect[-1])

hw4.1_res <- aov(Y ~ factor(A)*factor(B)*factor(C), data=hw4.1)

summary(hw4.1_res)

plot(hw4.1_res$fitted.values,hw4.1_res$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

Residual <- hw4.1_res$residuals
qqnorm(Residual)
qqline(Residual)

#c
hw4.1_res <- aov(Y ~ factor(A)+factor(B)+factor(C)+factor(A)*factor(C), data=hw4.1)
summary(hw4.1_res)

#d
hw4.1_reg <- lm(Y ~ x1 + x2 + x3 + x13)
summary(hw4.1_reg)

#e

beta0 <- hw4.1_reg$coefficients[1]
beta1 <- hw4.1_reg$coefficients[2]
beta2 <- hw4.1_reg$coefficients[3]
beta3 <- hw4.1_reg$coefficients[4]
beta13 <- hw4.1_reg$coefficients[5]

x_1 <- x_2 <- x_3 <- seq(-1,1,length=21)

haty12 <- matrix(rep(0,21**2),21,21)

for (i in 1:21) { 
  for(j in 1:21)  {
    {
      haty12[i,j] =  beta0 + beta1*x_1[i] + beta2*x_2[j] + beta3*1 + beta13*1
    }
  }
}

haty13 <- matrix(rep(0,21**2),21,21)

for (i in 1:21){ 
  for(j in 1:21)  {
    {
      haty13[i,j] =  beta0 + beta1*x_1[i] + beta2*1 + beta3*x_3[j] + beta13*x_1[i]*x_3[j]
    }
  }
}

haty23 <- matrix(rep(0,21**2),21,21)

for (i in 1:21){ 
  for(j in 1:21)  {
    {
      haty23[i,j] =  beta0 + beta1*1 + beta2*x_2[i] + beta3*x_3[j] + beta13*1*x_3[j]
    }
  }
}

contour(x_1, x_2, haty12, xlab="A", ylab="B", main="(A*B)")
contour(x_1, x_3, haty13, xlab="A", ylab="C", main="(A*C)")
contour(x_2, x_3, haty23, xlab="B", ylab="C", main="(B*C)")


pred<- function(x1, x3) {
  z= beta0 + beta1*x1 + beta2*1 + beta3*x3 + beta13*x1*x3
  return(z)
}

z<-outer(x_1,x_3,pred)

persp(x_1,x_3,z, theta=135, phi = 45, xlab="A", ylab="C")

#f
interaction.plot(x1, x2, Y, xlab="A", trace.label = "B")
interaction.plot(x2, x3, Y, xlab="B", trace.label = "C")
interaction.plot(x1, x3, Y, xlab="A", trace.label = "C")


#2

A <- rep(c(-1, 1), 16)
B <- rep(c(-1, -1, 1, 1), 8)
C <- rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 4)
D <- rep(rep(c(-1, 1), each=8),2)
E <- rep(c(-1,1), each = 16)

f <- cbind(A, B, C, D, E)
a <- data.frame(nrow = 32)
for(i in 1:32){
  for(j in 1:5){
  if(f[i,j] == 1) a[i,j] <- LETTERS[j]
    else if(f[i,j] == -1) a[i,j] <- ""
  }
}
a
b <- character(length = 32)
design <- for(i in 1:32) {
  b[i] <- paste(a[i,1], a[i,2], sep="")
  b[i] <- paste(b[i], a[i,3], sep="")
  b[i] <- paste(b[i], a[i,4], sep="")
  b[i] <- paste(b[i], a[i,5], sep="")
}
Y <- c(7, 9, 34, 55, 16, 20, 40, 60, 8, 10, 32, 50, 18,
       21, 44, 61, 8, 12, 35, 52, 15, 22, 45, 65, 6, 10,
       30, 53, 15, 20, 41, 63)
hw4.2 <- as.data.frame(cbind(A,B,C,D,E,Y))

designx <- matrix(cbind(rep(1,32), A, B, A*B, C, A*C, B*C, A*B*C, D, A*D, B*D, A*B*D, C*D, A*C*D, B*C*D, A*B*C*D, E, A*E, B*E, A*B*E,
                        C*E, A*C*E, B*C*E, A*B*C*E, D*E, A*D*E, B*D*E, A*B*D*E, C*D*E, A*C*D*E, B*C*D*E, A*B*C*D*E),nrow=32)
designx 
Factor_Effect <- (t(designx)%*%Y)/(16)
tail(as.data.frame(cbind(b,Factor_Effect)), 16)
a<-qqnorm(Factor_Effect[-1])
qqline(Factor_Effect[-1])
text(a[[1]], a[[2]], b[-1], pos = 2)

hw4.2_res <- aov(Y ~ factor(A)+factor(B)+factor(C)+factor(A)*factor(B), data=hw4.2)
summary(hw4.2_res)
#d
x1<-A
x2<-B
x3<-C
x12 <- A*B
hw4.2_reg <- lm(Y ~ x1 + x2 + x3 + x12)
summary(hw4.2_reg)

plot(hw4.2_res$fitted.values,hw4.2_res$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")


Residual <- hw4.2_res$residuals
qqnorm(Residual)
qqline(Residual)

interaction.plot(x1, x2, Y, xlab="A", trace.label = "B")


#3
block <- A*B*C*D*E
hw4.3 <- cbind(hw4.2, block)

hw4.3_res <- aov(Y ~ factor(A)+factor(B)+factor(C)+factor(A)*factor(B)+factor(block), data=hw4.3)
summary(hw4.3_res)

Residual <- hw4.3_res$residuals
qqnorm(Residual)
qqline(Residual)

#4
id <- c("I", "ad", "bd", "ab", "cd", "ac", "bc", "abcd")
A <- c(-1, 1, -1, 1, -1, 1, -1, 1)
B <- c(-1, -1, 1, 1, -1, -1, 1, 1)
C <- c(-1, -1, -1, -1, 1, 1, 1, 1)
D <- c(-1, 1, 1, -1, 1, -1, -1, 1)
Y <- c(90, 72, 87, 83, 99, 81, 88, 80)
hw4.4 <- data.frame(A, B ,C, Y, D)
hw4.4 <- cbind(id, hw4.4)
hw4.4

hw4.4_reg <- lm(Y~A+B+C+D+A*B+A*C+A*D, data=hw4.4)
Effects1  <- round(2*hw4.4_reg$coefficients,3)
Effects1
qqnorm(Effects1[-1])
qqline(Effects1[-1])

hw4.4_res <- aov(Y~factor(A)+factor(B)++factor(D)+factor(A)*factor(B)+factor(A)*factor(D), data=hw4.4)
summary(hw4.4_res)

hw4.4_res1 <- aov(Y~factor(A), data=hw4.4)
summary(hw4.4_res1)
resd <- hw4.4_res1$residuals
qqnorm(resd)
qqline(resd)

plot(hw4.4_res1$fitted.values,hw4.4_res1$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")
