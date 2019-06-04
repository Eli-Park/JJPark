library("car")
library("lsmeans")
library("ordinal")


setwd("C:/Users/pkmon/Desktop/생과데 과제3")
na <- read.csv("na.csv")
baka <- read.csv("baka.csv")
study <- read.csv("study.csv")

#5-3
na.fit <- lm(Y ~ X*med, data=na)
na.anco <- Anova(na.fit,type="III")
na.anco

na.fit.r <- lm(Y~ X + med, data=na)
na.anco.r <- Anova(na.fit.r,type="III")
na.anco.r
na.fit.r
lsmeans(na.fit.r,"med")
mean(na$X)

#5-5
str(baka)
baka$temp <- as.factor(baka$temp)

baka.fit <- lm(growth ~ water*temp + fert*temp, data=baka)
baka.anco <- Anova(baka.fit,type="III")
baka.anco

baka.fit.r <- lm(growth ~ temp + water + fert, data=baka)
baka.anco.r <- Anova(baka.fit.r,type="III")
baka.anco.r
baka.fit.r
lsm<-lsmeans(baka.fit.r,"temp", adjust = "bonferroni")
c(mean(baka$water),mean(baka$fert))

contrast(lsm, alpha=0.05, method="pairwise", adjust=NULL)

#5-7

study.fit <- lm(ach ~ meth + inte + mot + meth*inte + meth*mot , data=study)
study.ano <- Anova(study.fit, type="III")
study.ano

study.fit.r <- lm(ach ~ meth + inte +mot, data=study)
study.anco.r <- Anova(study.fit.r, type="III")
study.anco.r
study.fit.r
lsmeans(study.fit.r, "meth")
