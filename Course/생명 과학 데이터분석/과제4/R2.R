library(KMsurv)
library(survival)


#7-2
time <- c(0:15 , NA)
death <- c(82,30,27,22,26,25,20,11,14,13,5,5,5,2,3,3)
cens <- c(0,8,8,7,7,28,31,32,24,27,22,23,18,9,7,11)
sur <- lifetab(tis = time, ninit = 555 , nlost = cens, nevent = death)
sur
plot(stepfun(x=1:15, y = sur$surv) , pch="", main = "Survival Curve",
     ylab = "Survival Probability", xlab="Time")

#7-3
data <- read.csv("cancer2.csv")
surfit <- Surv(data$time,data$censor==1)
sur <- survfit(surfit~1)
plot(sur , conf.int = F, mark=1 , main = "Survival Curve")
legend("topright" , legend="censored", pch=1)
summary(sur)

#7-4
data <- read.csv("Hod.csv")
time <- data$time
cens <- data$censor
group <- data$group
surfit <- Surv(time , cens==1)
sur <- survfit(surfit~group)
plot(sur, col=c("red", "blue"),mark=1 , main = "Survival time" , xlab="time" ,ylab="survival distribution function")
legend(35,1.0 , legend=c("A","B"), lty=1, col=c("red", "blue") , title="Group", bty="n")
survdiff(surfit~group)

#7-5
library("flexsurv")

data <- read.csv("dia.csv")
data
time <- data$time
cens <- data$censor
surfit <- Surv(time , cens==1)
sur <- survfit(surfit~1)
S <- sur$surv
time<-unique(sort(time))

data1 <- cbind(S=S, logs=-log(S), weibul=log(-log(S)), lognorm=qnorm(1-S), loglogit=log((1-S)/S), logt=log(time), time=time)
data1 <- as.data.frame(data1)
library(ggplot2)
library(gridExtra)

#지수
p1<-ggplot(data=data1, aes(x=time, y=logs))+
  geom_line(size=1, col="blue")+
  geom_point(size=2, col="skyblue")+
  ggtitle("지수분포")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

#와이블
p2<-ggplot(data=data1, aes(x=logt, y=weibul))+
  geom_line(size=1, col="blue")+
  geom_point(size=2, col="skyblue")+
  ggtitle("와이블")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


#로그-정규분포
p3<-ggplot(data=data1, aes(x=logt, y=lognorm))+
  geom_line(size=1, col="blue")+
  geom_point(size=2, col="skyblue")+
  ggtitle("로그정규")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

#로그-로지스틱
p4<-ggplot(data=data1, aes(x=logt, y=loglogit))+
  geom_line(size=1, col="blue")+
  geom_point(size=2, col="skyblue")+
  ggtitle("로그 로지스틱")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2)

#와이블 분포
time <- data$time
cens <- data$censor
A<-survreg(Surv(time, censor) ~ age + BMI + dage + factor(smoke), data = data, dist = "weibull")
summary(A)

#7-6
data <- read.csv("Leukemia.csv")
time <- data$time
cens <- data$censor
age <- data$age
marr <- data$x
sur = Surv(time, cens==1)
coxph(sur~age+marr, method="breslow")
