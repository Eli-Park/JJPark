#Data Read
setwd("C:/Users/pkmon/Desktop/»ý°úµ¥1")
App <- read.csv("Approval.csv")
Can <- read.csv("CANCER.csv")
Coron <- read.csv("COR.csv")
Dep <- read.csv("Depress.csv")
Ery <- read.csv("ERY.csv")
Hiv <- read.csv("HIV.csv")

#3-1
#(a)
library("epiR")
Dep$thr <- as.factor(Dep$thr)
Dep$dep <- as.factor(Dep$dep)
Rdep <- xtabs(obs ~ thr + dep, data=Dep)
Rdep
epi.2by2(Rdep, method="cohort.count", conf.level = 0.95)

#3-4
library("DescTools")
Rhiv <- xtabs(obs ~ hpv + hiv, data=Hiv)
Rhiv
chisq.test(Rhiv)


#3-5
Rcan <- xtabs(obs ~ treat + result, data=Can)
Rcan
fisher.test(Rcan)


#3-7
Rapp <- xtabs(count ~ first + second, data=App)
Rapp
mcnemar.test(Rapp, correct = FALSE)


#3-8
str(Ery)
Ery$Severity = factor(Ery$Severity, levels=unique(Ery$Severity))

Rery = xtabs(count ~ Gender + Survival + Severity, data=Ery)
Rery
mantelhaen.test(Rery, correct = FALSE)

Rery1 = xtabs(count ~ Gender + Survival, data=Ery)
chisq.test(Rery1)

#3-9
Rcor <- xtabs(count ~ dignosis + disease, data = Coron)
Rcor
Sensitivity <- Rcor[1,1]/sum(Rcor[,1])
Specificity <- Rcor[2,2]/sum(Rcor[,2])
PPT <- (Sensitivity*0.1)/((Sensitivity*0.1)+((1-Specificity)*(1-0.1)))
PNT <- (Specificity*0.9)/((Specificity*0.9)+((1-Sensitivity)*(0.1)))
result <- c(Sensitivity, Specificity, PPT, PNT)
names(result) <- c("Sensitivity", "Specificity", "PPT", "PNT")
result
