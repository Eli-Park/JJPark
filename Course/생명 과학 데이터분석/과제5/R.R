##8-1
p <- c(0.025, 0.31, 0.009, 0.28, 0.345, 0.42, 0.06)
#order stat
min(p)
1-(1-0.05)^(1/length(p)) 
min(p) > 1-(1-0.05)^(1/length(p)) 


#inverse chi-squre
logp <- -2*log(p)
sum(logp)
qchisq(0.975, 2*length(p))
sum(logp)>qchisq(0.975, 2*length(p))

#inverse normal method
invnorm <-qnorm(1-p)
Z <- sum(invnorm)/sqrt(length(p))
Z
qnorm(0.975)
Z>qnorm(0.975)

#logit 
k<-length(p)
L <- sum(log(p/(1-p)))
L.st <- L*sqrt((3*(5*k+4))/((pi^2)*k*(5*k+2)))
L.st
-qt(0.975, 5*k+4)
L.st < -qt(0.975, 5*k+4)


##8-2
Q <- sum((invnorm-mean(invnorm))^2)
Q
qchisq(0.95, k-1)
Q > qchisq(0.95, k-1)

##8-5
ni <- c(90,40,36,20,22,10,10,10,39,50)
di <- c(-0.581,0.263,0.381,0.505,0.275,0.147,0.039,0.284,-0.088,-0.116)
k <- length(ni)
h1 <- function(x) sqrt(2)*asinh(x/sqrt(8))
h2 <- function(x) sqrt(8)*sinh(x/sqrt(2))
hp <- sum(2*ni*h1(di)/(2*sum(ni)))
delta <- h2(hp)
alpha <- 0.05
lowerbound <- hp - qnorm(1-alpha/2)/sqrt(sum(2*ni))
upperbound <- hp + qnorm(1-alpha/2)/sqrt(sum(2*ni))
lowerbound
upperbound
lower <- h2(lowerbound)
upper <- h2(upperbound)
lower
upper

##8-6
Q <- 2*sum(ni*(h1(di)-hp)^2)
pvalue <- 1-pchisq(Q,df=k-1)
alpha <- 0.05
Q
qchisq(1-alpha,df=k-1)
