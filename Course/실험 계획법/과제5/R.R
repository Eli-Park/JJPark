#1

A <- rep(c(-1,1), 8)
B <- rep(rep(c(-1,1), each=2), 4)
C <- rep(rep(c(-1,1), each=4), 2)
D <- rep(c(-1,1), each=8)
E <- c(-1, 1, 1, -1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1, -1, 1)
F <- c(-1, -1, 1, 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, -1, 1, 1)
y <- c(74, 190, 133, 127, 115, 101, 54, 144, 121, 188, 135, 170, 126, 175, 126, 193)

hw5.1 <- as.data.frame(cbind(A,B,C,D,E,F,y))
#1)
lm1 <- lm(y~A+B+C+D+E+F+A*B*C*D*E*F, data=hw5.1)

Effects1  <- round(2*lm1$coefficients,3)
Effects2 <- Effects1[is.na(Effects1) != T]
Effects2

QQ_y=qqnorm(Effects1[-1])
identify(QQ_y)

#2)
ao1 <- aov(y ~ factor(A)+factor(C)+factor(D)+factor(E)+factor(C*D) + factor(D*E), data=hw5.1)
ao1
summary(ao1)

#3)
Residual <- ao1$residuals
qqnorm(Residual)
qqline(Residual)
plot(ao1$fitted.values,ao1$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")


#4)
lm11 <- lm(y ~ A+C+D+E+C*D +D*E, data=hw5.1)
lm11$coefficients


#2
library("lme4")
loom <- rep(c(1,2,3,4,5), each=5)
i <- rep(c(1,2,3,4,5), 5)
st <- c(14, 14.1, 14.2, 14, 14.1, 13.9, 13.8, 13.9, 14, 14,
        14.1, 14.2, 14.1, 14, 13.9, 13.6, 13.8, 14, 13.9, 13.7,
        13.8, 13.6, 13.9, 13.8, 14)
hw5.2 <- as.data.frame(cbind(loom,i,st))

#1)
ao2 <- aov(lm(st ~ factor(loom),data=hw5.2))
summary(ao2)

#2)
hw5.2_re <- lmer(st ~ (1|loom), hw5.2)
summary(hw5.2_re)

#4)
Residual <- ao2$residuals
qqnorm(Residual)
qqline(Residual)
plot(ao2$fitted.values,ao2$residuals, xlab="Predicted Values", 
     ylab="Rediduals", main="Residual Plot")

#3
po <- rep(c(1,2), each = 9)
temp <- rep(rep(c(800, 825, 850), each=3),2)
y <- c(570, 565, 583, 1063, 1080, 1043, 565, 510, 590,
       528, 547, 521, 988, 1026, 1004, 526, 538, 532)
hw5.3 <- as.data.frame(cbind(po,temp,y))
hw5.3_re <- lmer(y ~ factor(temp) + (factor(temp) | factor(po)), hw5.3)
summary(hw5.3_re)
anova(hw5.3_re)
hw5.3

#4
day <- rep(c(1,2,3), each = 12)
app <- rep(rep(c(1,2,3), each = 4),3)
mix <- rep(rep(c(1,2,3,4),3), 3)
y <- c(64.5, 66.3, 74.1, 66.5, 68.3, 69.5, 73.8, 70.0, 70.3, 73.1, 78.0, 72.3,
       65.2, 65.0, 73.8, 64.8, 69.2, 70.3, 74.5, 68.3, 71.2, 72.8, 79.1, 71.5,
       66.2, 66.5, 72.3, 67.7, 69.0, 69.0, 75.4, 68.6, 70.8, 74.2, 80.1, 72.4)
hw5.4 <- as.data.frame(cbind(day,app,mix,y))