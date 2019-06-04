#(a)

poi.sim <- function(l) {
  X <- 0
  px <- exp(-l)*(l^X)/factorial(X)
  Fx <- px
  U <- runif(1)
  while(Fx < U) {
    X <- X+1
    px <- px*l/X
    Fx <- Fx+px
  }
  return(X)
}

P <- numeric(1000)

for(i in 1:1000) {
  P[i] <- poi.sim(3)
}

#(b)

binom.sim <- function(n, p) {
  X <- 0;  px <- (1-p)^n; Fx <- px; U <- runif(1)
  while (Fx < U) {
    X <- X + 1
    px <- px*p/(1-p)*(n-X+1)/X
    Fx <- Fx + px }
  return(X) }

P.1 <- numeric(1000)
lamda <- 3
n <- 100000

for(i in 1:1000) {
  P.1[i] <- binom.sim(n, lamda/n)
}

#(c)

genPoi <- function(lambda,at) {
  x<-rexp(100,lambda)
  if(sum(x)<at)
    return(NA)
  if(x[1]>at)
    return(0)
  else
    return(max(which(cumsum(x)<at)))
}

P.2 <- numeric(1000)
for(i in 1:1000) {
  P.2[i] <- genPoi(3,1)
}

barplot(table(P))
barplot(table(P.1))
barplot(table(P.2))
