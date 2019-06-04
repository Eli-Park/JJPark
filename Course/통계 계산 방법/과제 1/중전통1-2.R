#2
  #a
B <- function(n, b) {
  if(n != b) {
  a <- lfactorial(365) + (n-b)*log(365-b) - lfactorial(365-b) - n*log(365)
  return(exp(a))
  }
  else return(0)
}


Bn <- numeric(79)
for(i in 1:79) Bn[i] <- B(80,i)
names(Bn) <- 1:79

par(mfrow = c(1,1))
barplot(Bn, main ="Probability of (Bn > b) when n is 80", xlab = "b", ylab = "Probability")

  #b

EB <- function(n) {
  sum <- 0
  for(b in 1:n) {
    temp <- b*(B(n, b-1) - B(n, b))
    sum <- sum + temp
    }
  return(sum)
}

EBn <- numeric(76)
for(i in 1:76) EBn[i] <- EB(i+24)
head(EBn)
tail(EBn)
names(EBn) <- 25:100
barplot(EBn, main ="Expectations of E(Bn)", xlab = "n", ylab = "E(Bn)")

  #c
B.t <- function(n) {
  a <- rep(0, 365)
  t <- 1
  repeat{
    b <- sample(1:365, 1)
    a[b] <- a[b] + 1
    if(max(a) > 1) break
    else if(n == t) break
    else t <- t+1
    
  }
  return(t)
}

PB.t <- function(n) {
  a <- replicate(10000, B.t(n))
  c <- numeric(n)
  for(i in 1:n) {
    c[i] <- length(subset(a, a>(i))) 
  }
  p <- c/10000
  return(p)
}


EB.t <- function(n) {
  sum <- 0
  p <- numeric(n+1)
  p <- c(1, PB.t(n))
  for(b in 1:n) {
    temp <- b*(p[b] - p[b+1])
    sum <- sum + temp
    }
  return(sum)
}

EB.tn <- numeric(76)
for(i in 1:76) EB.tn[i] <- EB.t(i+24)
names(EB.tn) <- 25:100

barplot(EB.tn, main = "Expectations of tested E(Bn)", xlab = "n", ylab = "E(Bn)")
        