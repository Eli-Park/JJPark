#(a)
CC <- function(n) {
  a <- sample(letters[1:12], n, replace = T)
  b <- unique(a)
  c <- sum(table(b))
  return(c)
}

MC <- replicate(10000, CC(20))
ED <- sum(MC)/10000

#(b)
RCC <- function(N, k, n) {
  sum <- 0
  for(i in 1:(k-1)) {
    sum <- sum + choose(k, i)*(((k-i)/k)^n)*((-1)^(i+1))
  }
  D <- choose(N,k)*((k/N)^n)*(1-sum)
  return(D)
}

ED.1 <-0
for(i in 1:12) {
  ED.1 <- ED.1 + i*RCC(12, i, 20)
}


#(c)
ED.2 <- function(N, n) {
  a <- N*(1-(1-(1/N))^n)
  return(a)
}
ED.2(12,20)
