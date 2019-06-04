#(a)
f1 <- function(x) 1/(pi*sqrt(1-x^2))
f2 <- function(x) 2*sqrt(1-x^2)/(pi)

integrate(f1, -1, 1)
integrate(f2, -1, 1)

mix <- function(x) {
  u <- runif(1)
  k <- (u < 1/3)
  X <- k*f1(x) + (1-k)*f2(x)
  return(X)
}

Mx <- runif(1000, -1, 1)
for(i in 1:1000) {
  Mx[i] <- mix(Mx[i])
}







#(b)

##(1)
fx <- function(r, b) {
  y <- rgamma(1, r, 1/b)
  X <- rpois(1, y)
  return(X)
}

NB <- replicate(1000, fx(5,2))

##(2)
geo.sim <- function(p) {
  x <- 0;   px <- p*(1-p)^x; Fx <- px
  u <- runif(1)
  while (Fx < u) {
    x <- x + 1; px <- px*(1-p); Fx <- Fx + px
  }
  return(x)}

rbi.sim <- function(r, p) {
  x <- replicate(r,geo.sim(p))
  y <- sum(x)
  return(y)
}

NB.1 <- replicate(1000, rbi.sim(5,1/3))

hist(NB)
hist(NB.1)
