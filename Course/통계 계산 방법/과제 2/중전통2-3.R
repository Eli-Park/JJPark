#(a)

t1j <- c(0, 8/36, 4/36, 3/36, 4/36, 5/36, 5/36, 4/36, 3/36)
t2j <- c(0, 1, 0, 0, 0, 0, 0, 0, 0)
t3j <- c(0, 0, 1, 0, 0, 0, 0, 0, 0)

T <- matrix(c(t1j, t2j, t3j, rep(0, 6*9)), byrow=T, nrow=9)

for(i in 4:9) {
  T[i, 2] <- T[1, i]
  T[i, 3] <- 6/36
  T[i, i] <- 1-T[i,2]-T[i,3]
}
colnames(T) <- c("S", "W", "L", "4", "5", "6", "8", "9", "10")
rownames(T) <- c("S", "W", "L", "4", "5", "6", "8", "9", "10")
W <- T%^%100
W[1,2]


#(b)

for(i in 1:10000) {
a[i]<-0
d1 <- sample(1:6, 1)
d2 <- sample(1:6, 1)
fds <- d1+d2
if(fds == 7 || fds == 11) a[i] <- 1
else if(fds == 2 || fds == 3 || fds == 12) a[i] <- 0
else repeat{
  d1 <- sample(1:6, 1)
  d2 <- sample(1:6, 1)
  ds <- d1 + d2
  if(ds == fds) {a[i]<- 1; break}
  else if(ds == 7) {a[i]<- 0; break}
}
}

sum(a)/10000
