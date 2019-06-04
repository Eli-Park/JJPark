#1
m <- c(108, 138, 124, 163, 124, 159, 106, 134, 115, 139)
(mean(m)-120)/(sd(m)/sqrt(length(m)))
t.test(m, alternative = "greater", mu = 120, conf.level = 0.99)
qt(0.99, df = 9)
pt(1.7798, df= 9)
mean(m)-(qt(0.99, df = 9)*(sd(m)/sqrt(length(m))))

#2
##a
g1 <- c(16.03, 16.01, 16.04, 15.96, 16.05, 15.98, 16.05, 16.02, 16.02, 15.99) 
g2 <- c(16.02, 16.03, 15.97, 16.04, 15.96, 16.02, 16.01, 16.01, 15.99, 16.00)
pv <- ((length(g1)-1)*(0.015^2)+(length(g2)-1)*(0.018^2))/(length(g1)+length(g2)-2)
z <- (mean(g1)-mean(g2))/(sqrt(pv)*sqrt((1/length(g1))+(1/length(g2))))
z

pnorm(0.975)
(1-pnorm(z))*2

##d
(mean(g1)-mean(g2))+qnorm(0.975)*(sqrt(pv)*sqrt((1/length(g1))+(1/length(g2))))
(mean(g1)-mean(g2))-qnorm(0.975)*(sqrt(pv)*sqrt((1/length(g1))+(1/length(g2))))

#3
g1 <- c(9.9, 10.6, 9.4, 10.3, 10.0, 9.3, 10.3, 9.8) 
g2 <- c(10.2, 10.6, 10.0, 10.2, 10.7, 10.4, 10.5, 10.3)
t.test(g1, g2, mu = 0, alternative = "two.sided", conf.level = 0.95, var.equal=TRUE)

pt(-2.3016, df= 14)

par(mfrow=c(1,1))
qqnorm(g1)
qqline(g1)
qqnorm(g2)
qqline(g2)


#4

M <- c(50, 60, 50, 60)

library(pwr)
f <- function(s) {
  b <- 0
  for(i in 1:4) {
      b<- b+((M[i]-mean(M))^2)/(4*s)
  }
  b<-sqrt(b)
  return(b)
}

f(25)
pwr.anova.test(power=0.9, k=4, sig.level=0.05, f=f(25))
pwr.anova.test(power=0.9, k=4, sig.level=0.05, f=f(36))
pwr.anova.test(power=0.9, k=4, sig.level=0.05, f=f(49))




#5
p15 <- c(7, 7, 15, 11, 9)
p20 <- c(12, 17, 12, 18, 18)
p25 <- c(14, 19, 19, 18, 18)
p30 <- c(19, 25, 22, 19, 23)
p35 <- c(7, 10, 11, 15, 11)

library(agricolae)

d<- stack(list(p15=p15, p20=p20, p25=p25,p30=p30, p35=p35))
res <- aov(values ~ ind, data=d)
res
summary(res)

df <- df.residual(res)
MSerror <- deviance(res)/df

LSD     <- LSD.test(d$values, d$ind, df, MSerror)
LSD

qqnorm(d$values)
qqline(d$values)

par(mfrow=c(2,2))
qqnorm(p15)
qqline(p15)

qqnorm(p20)
qqline(p20)

qqnorm(p25)
qqline(p25)

qqnorm(p30)
qqline(p30)

par(mfrow=c(1,1))
qqnorm(p35)
qqline(p35)

resid <- res$residuals
predicted <- res$fitted.values
plot(predicted, resid)
abline(h=0)
bartlett.test(values ~ ind, data=d)

TUKEY   <- TukeyHSD(res)
TUKEY


#6
t1 <- c(143, 141, 150, 146)
t2 <- c(152, 149, 137, 143)
t3 <- c(134, 136, 132, 127)
t4 <- c(129, 127, 132, 129)

d<- stack(list(t1=t1,t2=t2,t3=t3,t4=t4))
res <- aov(values ~ ind, data=d)
res
summary(res)

#b
mean(d$values)
mean(t1)-mean(d$values)
mean(t2)-mean(d$values)
mean(t3)-mean(d$values)
mean(t4)-mean(d$values)


#c

library(gmodels)
ci(t4, confidence = 0.95)
mean(t4)+qt(0.975, df=3)*(sd(t4)*sqrt((1/length(t4))))
mean(t4)-qt(0.975, df=3)*(sd(t4)*sqrt((1/length(t4))))
t.test(t4, conf.level = 0.95)

pv <- ((length(t1)-1)*(var(t1))+(length(t4)-1)*(var(t4)))/(length(t1)+length(t4)-2)
(mean(t1)-mean(t4))+qt(0.995, df=length(t4)+length(t1)-2)*(sqrt(pv)*sqrt((1/length(t1))+(1/length(t4))))
(mean(t1)-mean(t4))-qt(0.995, df=length(t4)+length(t1)-2)*(sqrt(pv)*sqrt((1/length(t1))+(1/length(t4))))
t.test(t1, t4, conf.level = 0.99, var.equal = T)


#d
df <- df.residual(res)
MSerror <- deviance(res)/df

LSD     <- LSD.test(d$values, d$ind, df, MSerror)
LSD

#e
TUKEY   <- TukeyHSD(res)
TUKEY

#f
qqnorm(d$values)
qqline(d$values)

par(mfrow=c(2,2))
qqnorm(t1)
qqline(t1)

qqnorm(t2)
qqline(t2)

qqnorm(t3)
qqline(t3)

qqnorm(t4)
qqline(t4)

resid <- res$residuals
predicted <- res$fitted.values
plot(predicted, resid)
abline(h=0)
bartlett.test(values ~ ind, data=d)
