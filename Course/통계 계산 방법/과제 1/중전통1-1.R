#1
  #a


a <- function(x, n) {
  1/(sqrt(2*pi)*factorial(n))*((-x/2)^n)*((n+1)/(2*n+1))
}




F <- function(x) {
  sum <- 0
  temp <- 2
  eps <- 0.000001
  n<-0
  while(abs(temp) > eps) {
    temp <- a(x,n)*(x^(n+1))/(n+1)
    sum <- sum + temp
    n <- n+1
  }
  return(sum)
}

F(1)
pnorm(1) - 0.5

  #b
n <- rnorm(2000)
y <- subset(n, n > 0)
b <- length(y)
u <- numeric(b)
for(i in 1:b) {
  u[i] <- F(y[i])
}


rng.chisq.test <- function(x,m) {
  Obs.1 <- table(trunc(m*x)/m)
  Obs <- c(Obs.1, rep(0, m-length(Obs.1)))
  Exv <- length(x)*rep(1,m)/m
  chival <- sum((Obs-Exv)^2/Exv)
  pval <- 1-pchisq(chival,m-1)
  list(test.stat=chival, p.value=pval, degf=m-1)}

rng.chisq.test(u, 10)

  #c

rng.chisq.test.1 <- function(x,m){
  Obs <- table(trunc(m*x)/m)
  Exv <- length(x)*rep(1,m/2)/(m/2)
  chival <- sum((Obs-Exv)^2/Exv)
  pval <- 1-pchisq(chival,m-1)
  list(test.stat=chival, p.value=pval, degf=m-1)}

rng.chisq.test.1(u,10)



myrng <- function(n,a,c,m,seed){
  x <- numeric(n)
  x[1] <- seed
  for (i in 1:n) {
    x[i+1]<-(a*x[i]+c)%%m
  }
  x[1:n]/m }

u.1 <- myrng(1000, 171, 0, 30269, 27218)/2

rng.chisq.test.1(u.1, 10)

plot(u.1[2:1000], u.1[1:999], pch=20)


  #d

g <- function(x, n) {
  a <- lgamma((n+1)/2)-(1/2)*log(n*pi)-lgamma(n/2)
  b <- exp(a)*(((1-((x^2)/n))^(-(n+1)/2)))
  return(b)
}

g(0,1)

f <- function(x) {
  (1/sqrt(2*pi))*exp((-(x^2)/2))
}

x <- sort(runif(1000, -5, 5))


plot(x, g(x,1), type = "n", xlab = "X", ylab = "gn(X)")
lines(x, g(x, 9))
for(i in 1:30) {
  lines(x, g(x, i), col = i)
    
}

plot(x, f(x), type = "n", xlab="X", ylab ="f(X)")
lines(x, f(x))


  #e

h <- function(n) {
  a <-0
  if(n == 1) return(0)
  else {
    a<-((1/2)*(log(2)+log(pi)+log(n-1)-log(2)))+((n-1)/2)*(log(n-1)-log(2)-1)
    return(a)
  }
}


h.1 <- numeric(1000)
for(i in 1:1000) h.1[i] <- lgamma((i+1)/2) - h(i)

head(exp(h.1))
tail(exp(h.1))

h.2 <- numeric(1000)
for(i in 1:1000) h.2[i] <- h(i)

h.3 <- h.2 + log(exp(1)) - log(10)
table((10^h.3) %/% (10^(floor(h.3))))/sum(table((10^h.3) %/% (10^(floor(h.3)))))

