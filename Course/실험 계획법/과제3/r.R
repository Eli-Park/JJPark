library(ggplot2)
library("gcookbook")
library(agricolae)
library("faraway")

#1
temper <- c(rep(150,6), rep(160, 6), rep(170,6))
temper2 <- temper
press <- rep(c(200,215,230), 6)
temper2 <- temper^2
press2 <- press^2

data <- c(90.4, 90.7, 90.2, 90.2, 90.6, 90.4,
          90.1, 90.5, 89.9, 90.3, 90.6, 90.1,
          90.5, 90.8, 90.4, 90.7, 90.9, 90.1)
A <- as.data.frame(cbind(temper, temper2, press, press2, data))
str(A)

#(a)
ANO <- aov(data ~ factor(temper) + factor(press) + factor(temper)*factor(press)  , data=A)
ANO
summary(ANO)

ANO <- aov(data ~ factor(temper) + factor(press)  , data=A)
ANO
summary(ANO)


#(b)
Residual <- ANO$residuals
qqnorm(Residual)
qqline(Residual)

plot(ANO$fitted.values,ANO$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

plot(temper,ANO$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

plot(press,ANO$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")


#(c)
interaction.plot(temper,press,data, main="Interation Plot")
points(temper[1:6],data[1:6],col="black",pch=16,cex=1)
points(temper[7:12],data[7:12],col="red",pch=16,cex=1)
points(temper[13:18],data[13:18],col="blue",pch=16,cex=1)

#(d)

ANO_cov <- aov(data ~ temper + temper2 + press + press2 + temper*press, data=A)
ANO_cov
summary(ANO_cov)

ANO_cov$coefficients
x1<-seq(min(temper),max(temper), length=15)
x2 <- seq(min(press),max(press), length=15)
lm(data~temper + press)
pred<- function(x1, x2) {
  z= 48.5462 - 0.6404*x1 +0.002417*(x1^2)+ 0.86759*x2 -0.0018149*(x2^2)
  return(z)
}
z<-outer(x1,x2,pred)

persp(x1,x2,z, theta=30, phi = 45)

qqnorm(Residual)
qqline(Residual)

plot(ANO_cov$fitted.values,ANO_cov$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

plot(temper,ANO_cov$residuals, xlab="Temperature", 
     ylab="Rediduals", main="Residual Plot")

plot(press,ANO_cov$residuals, xlab="Pressure", 
     ylab="Rediduals", main="Residual Plot")



#2
glass <- c(rep(1,9), rep(2,9), rep(3,9))
temp <- rep(c(100, 125, 150), 9)
data <- c(580, 1090, 1392, 568, 1087, 1380, 570, 1085, 1386,
          550, 1070, 1328, 530, 1035, 1312, 579, 1000, 1299,
          546, 1045, 867, 575, 1053, 904, 599, 1066, 889)
temp2 <- temp^2

B <- as.data.frame(cbind(glass, temp, temp2, data))
ANO <- aov(data ~ factor(glass)*factor(temp), data=B)
ANO$coefficients
summary(ANO)

interaction.plot(temp,glass,data, main="Interation Plot")


#(b)
ANO <- aov(data ~ factor(glass) + temp + temp2 + factor(glass)*temp + factor(glass)*temp2, data=B)
ANO
summary(ANO)
ANO$coefficients

#(c)
Residual <- ANO$residuals
qqnorm(Residual)
qqline(Residual)

plot(ANO$fitted.values,ANO$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

plot(temp,ANO$residuals, xlab="Temperature",
     ylab="Rediduals", main="Residual Plot")
plot(glass,ANO$residuals, xlab="Glass", 
     ylab="Rediduals", main="Residual Plot")



#3
Oper <- c(rep(1,8),rep(2,8),rep(3,8))
Mach <- rep(1:4,6)
data <- c(109, 110, 108, 110, 110, 115, 109, 108,
          110, 110, 111, 114, 112, 111, 109, 112,
          116, 112, 114, 120 ,114, 115, 119, 117)
C <- as.data.frame(cbind(Oper, Mach, data))
#(a)
ANO <- aov(data ~ factor(Oper)*factor(Mach), data=C)
ANO
summary(ANO)

ANO <- aov(data ~ factor(Oper), data=C)
ANO
summary(ANO)

#(b)
Residual <- ANO$residuals
qqnorm(Residual)
qqline(Residual)

plot(ANO$fitted.values,ANO$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

plot(Oper,ANO$residuals, xlab="Operator", 
     ylab="Rediduals", main="Residual Plot")

plot(Mach, ANO$residuals, xlab="Machine", 
     ylab="Rediduals", main="Residual Plot")



#4
temp <- c(rep(0,6), rep(1,6), rep(2,6))
pres <- rep(c(250,260,270),6)
day <- rep(c(1,1,1,2,2,2),3)
data <- c(86.3, 84, 85.8, 86.1, 85.2, 87.3,
          88.5, 87.3, 89, 89.4, 89.9, 90.3,
          89.1, 90.2, 91.3, 91.7, 93.2, 93.7)
D <- as.data.frame(cbind(temp, pres, day, data))
D[,4] <- as.numeric(D[,4])
ANO <- aov(data ~ factor(temp) + factor(pres) + factor(day) + factor(temp) * factor(pres), data=D)
ANO
summary(ANO)

ANO <- aov(data ~ factor(temp) + factor(pres) + factor(day), data=D)
ANO
summary(ANO)


plot(ANO$fitted.values,ANO$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")
plot(temp,ANO$residuals, xlab="Temperature", 
     ylab="Rediduals", main="Residual Plot")
plot(pres,ANO$residuals, xlab="Pressure", 
     ylab="Rediduals", main="Residual Plot")


Residual <- ANO$residuals
qqnorm(Residual)
qqline(Residual)

