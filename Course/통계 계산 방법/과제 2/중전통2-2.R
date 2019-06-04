#(a)
p1j <- c(0, 1, 0, 0, 0, 0)
p2j <- c(1/4, 0, 1/4, 1/4, 1/4, 0)
p3j <- c(0, 1/4, 0, 1/4, 1/4, 1/4)
p4j <- c(0, 1/4, 1/4, 0, 1/4, 1/4)
p5j <- c(0, 1/3, 1/3, 1/3, 0, 0)
p6j <- c(0, 0, 1/2, 1/2, 0, 0)

P <- rbind(p1j, p2j, p3j, p4j, p5j, p6j)
colnames(P) <- 1:6

(P%^%100)[1,]


#(b)

P1 <- matrix(rep(0,36), nrow=6)
for(i in 1:6) {
  for(j in 1:6) {
    if((i-j)%%6 == 1 || (i-j)%%6 == 5) {
      P1[i,j] <- 1/2
    }
  }
}

colnames(P1) <- 1:6
(P1%^%100)[1,]


#(c)

fa <- function(x0) {
  r <- 0
  if(x0 == 1) r <- 2   
  else if(x0 == 2) r <- sample(c(1, 3, 4, 5), 1, prob = c(1/4, 1/4, 1/4, 1/4))
  else if(x0 == 3) r <- sample(c(2, 4, 5, 6), 1, prob = c(1/4, 1/4, 1/4, 1/4))
  else if(x0 == 4) r <- sample(c(2, 3, 5, 6), 1, prob = c(1/4, 1/4, 1/4, 1/4))
  else if(x0 == 5) r <- sample(c(2, 3, 4), 1, prob = c(1/3, 1/3, 1/3))
  else if(x0 == 6) r <- sample(c(3, 4), 1, prob = c(1/2, 1/2))
  return(r)
}

Fa <- function(x0, n) {
  c <- 1
  a <- fa(x0)
  repeat{
    a <- fa(a)
    c <- c+1
    if(c > n-1) break
  }
  return(a)
}

re <- table(replicate(1000, Fa(1, 100)))/1000


for(i in 2:6) {
  re <- rbind(re, table(replicate(1000, Fa(i, 100)))/1000)
}

re[1,]

fb <- function(x0) {
  r <- 0
  if(x0 == 1) r <- sample(c(2, 6), 1, prob = c(1/2, 1/2))
  else if(x0 == 2) r <- sample(c(1, 3), 1, prob = c(1/2, 1/2))
  else if(x0 == 3) r <- sample(c(2, 4), 1, prob = c(1/2, 1/2))
  else if(x0 == 4) r <- sample(c(3, 5), 1, prob = c(1/2, 1/2))
  else if(x0 == 5) r <- sample(c(4, 6), 1, prob = c(1/2, 1/2))
  else if(x0 == 6) r <- sample(c(1, 5), 1, prob = c(1/2, 1/2))
  return(r)
}


Fb <- function(x0, n) {
  c <- 1
  a <- fb(x0)
  repeat{
    a <- fb(a)
    c <- c+1
    if(c > n-1) break
  }
  return(a)
}

re1 <- matrix(rep(0, 36), nrow=6)

for(i in 1:6) {
  temp <- table(replicate(1000, Fb(i, 100)))/1000
  re1[i, as.numeric(names(temp))] <- temp[rank(names(temp))]
}

re1[1,]
