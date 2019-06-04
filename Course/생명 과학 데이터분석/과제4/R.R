setwd("C:/Users/pkmon/Desktop/생과데 과제4")
ex <- read.csv("exercise.csv")
med <- read.csv("medicine.csv")

#6-2
library("car")
ex[,1] <- as.factor(ex[,1])
med[,1] <- as.factor(med[,1])


time<-factor(rep(c(10,20,30,40),2))
method<-factor(sort(rep(c("1","2"),4)))
idata<-data.frame(time,method)

mod.ex <- lm(cbind(trt1_m_10,trt1_m_20,trt1_m_30,trt1_m_40,trt2_m_10,trt2_m_20,trt2_m_30,trt2_m_40)~Gender, data=ex)

options(contrasts = c("contr.sum", "contr.poly"))
av.ex <- Anova(mod.ex, idata=idata,idesign=~time*method, type=3)
summary(av.ex)


#6-6
time<-factor(c(0,1,2,3,4))

score <- stack(med)
trt0 <- as.factor(rep(rep(c("N","P"), each=19),5))
sub <- as.factor(rep(1:38, 5))
uni.med <- cbind(sub, trt0 ,score)
uni.med
uni.fit <- aov(values ~ trt0 + ind + Error(sub/ind) + trt0*ind, data = uni.med)
summary(uni.fit)

mod.med <- lm(cbind(m0, m1, m2, m3, m4) ~ trt, data=med)
av.med <- Anova(mod.med, idata=data.frame(time), idesign=~time, type=3)
summary(av.med)
