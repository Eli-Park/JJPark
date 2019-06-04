library(agricolae)
#1
Perform <- c(14, 12, 13, 11, 17, 14, 11, 12, 14, 13, 11, 10
          , 13, 13, 12, 12, 12, 10, 9, 8)
Additives <- c(2, 3, 4, 5, 1, 2, 4, 5, 1, 3, 4, 5, 1, 2, 3, 4,
               1, 2, 3, 5)
Car <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 
         5, 5, 5, 5)
data <- cbind(Additives, Car, Perform)
data <- as.data.frame(data)
head(data)
xtabs(Perform ~ Additives + Car, data=data)


res  <- aov(Perform ~ factor(Additives) + factor(Car), data=data)
res
summary(res)

par(mfrow=c(1,2),pch=1)	

Residual <- res$residuals
qqnorm(Residual)
qqline(Residual)

Predicted <- res$fitted.values
plot(Predicted, Residual, main="Residual Plot")
abline(h=0)


res_1 <- BIB.test(factor(Car), factor(Additives), Perform, console=TRUE, alpha = 0.05)


#2
Method <- c("C", "B", "A", "D", "D", "C", "B", "A", "A", "D", "C", "B",
            "B", "A", "D", "C")
Order <- rep(1:4, 4)
Operator <- c(rep(1,4), rep(2, 4), rep(3, 4), rep(4, 4))
Time <- c(10, 7, 5, 10, 14, 18, 10, 10, 7, 11, 11, 12, 8, 8, 9, 14)
data <- cbind(Order, Operator, Method, Time)
data <- as.data.frame(data)
data[,4] <- Time
head(data)
str(data)

lres <- aov(Time ~ factor(Order) + Method + factor(Operator)  , data=data)
lres
summary(lres)

par(mfrow=c(1,2),pch=1)	

Residual <- lres$residuals
qqnorm(Residual)
qqline(Residual)

Predicted <- lres$fitted.values
plot(Predicted, Residual, main="Residual Plot")
abline(h=0)

df <- df.residual(lres)
MSerror <- deviance(lres)/df

LSD     <- LSD.test(lres, "Method" ,console=TRUE)
TUKEY   <- TukeyHSD(lres, "Method")
TUKEY
SCHEFFE <- scheffe.test(lres, "Method", group=TRUE, console=TRUE)



#3
Order <- c(rep(1:4,4))
Operator <- c(rep(1,4), rep(2, 4), rep(3, 4), rep(4, 4))
Method <- c("C", "B", "A", "D", "B", "C", "D", "A", "D", "A", "B", "C",
            "A", "D", "C", "B")
Workpl <- c("b", "a", "d", "c", "c", "d", "a", "b", "d", "c", "b", "a",
            "a", "b", "c", "d")
Time <- c(11, 8, 9, 9, 10, 12, 11, 8, 14, 10, 7, 18, 8, 12, 15, 6)
data <- cbind(Order, Operator, Method, Workpl, Time)
data <- as.data.frame(data)
data[,5] <- Time
head(data)

glres <- aov(Time ~ factor(Order) + Method + factor(Operator) + Workpl, data=data)
glres
summary(glres)
par(mfrow=c(1,2),pch=1)	

Residual <- glres$residuals
qqnorm(Residual)
qqline(Residual)

Predicted <- glres$fitted.values
plot(Predicted, Residual, main="Residual Plot")
abline(h=0)



#4
SR <- c(rep(seq(5, 20, 5),4))
Furnace <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4))
Size <- c(8, 14, 14, 17, 4, 5, 6, 9, 5, 6, 9, 3, 6, 9, 2, 6)
data <- cbind(SR, Furnace, Size)
data <- as.data.frame(data)
data[,3] <- Size
head(data)
#(A)
rres <- aov(Size ~ factor(SR) + factor(Furnace)  , data=data)
rres
summary(rres)

#(B)
par(mfrow=c(1,1),pch=1)	
Residual <- rres$residuals
qqnorm(Residual)
qqline(Residual)

#(C)
plot(Residual, factor(SR), main="Residual VS Stirring Rate", pch = 16)
plot(Residual, factor(Furnace), main = "Residual VS Furnace", pch = 16)

#(D)
LSD     <- LSD.test(rres, "factor(Furnace)",console=TRUE)
TUKEY   <- TukeyHSD(rres, "factor(Furnace)")
TUKEY
SCHEFFE <- scheffe.test(rres,"factor(Furnace)", group=TRUE, console=TRUE)

rres_2 <- aov(Size ~ factor(Furnace), data=data)
rres_2
summary(rres_2)
