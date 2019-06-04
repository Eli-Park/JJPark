#(a)



y.gen <- function(x) 1 + 2*x - 3*x^2 + 4*x^3 - 5*x^4 + rnorm(1, 0, 0.01)
y <- numeric(100)

for(i in 1:100) {
  y[i] <- y.gen(i/100)
}
head(y)
tail(y)





#(b)

x.gen <- function(x) {
  return(c(1, x, x^2, x^3, x^4))
}

X <- x.gen(1/100)
for(i in 2:100) {
  X<- rbind(X, x.gen(i/100))
}


beta <- matrix(c(1,2,-3,4,-5), nrow=5)
e <- matrix(rnorm(100, 0, 0.01), ncol=1)

Y <- X%*%beta + e
y <- as.vector(X%*%beta + e)

beta.hat <- solve(t(X)%*%X)%*%t(X)%*%Y

a <- cbind(beta.hat, beta)
colnames(a) <- c("estimated", "real")
a

lm(y~X)



#(c)

A <- diag(x=1, nrow=100) - X%*%solve(t(X)%*%X)%*%t(X)


#install.packages(expm)
library(expm)
eigen(A%^%10)$values

A1 <- A%^%10

vnorm<- function(x) {
  sqrt(sum(x * x))
}

x0 <- as.vector(c(1,rep(0,99)))
diff <- 1
eps <- 0.0001
count <- 0

while(diff > eps){
  x1 <- A1 %*% x0
  lambda1 <- vnorm(x1)/vnorm(x0)
  x1 <- x1/vnorm(x1)
  diff <- vnorm(x1-x0)
  x0 <- x1
  count <- count+1
}

A2 <- A1 - lambda1*x0%*%t(x0) 



x0 <- as.vector(c(1,rep(0,99)))
diff <- 1
eps <- 0.0001
count <- 0

while(diff > eps){
  x1 <- A2 %*% x0
  lambda2 <- vnorm(x1)/vnorm(x0)
  x1 <- x1/vnorm(x1)
  diff <- vnorm(x1-x0)
  x0 <- x1
  count <- count+1
}

lambda2






#(d)
fxi.gen <- function(x) {
  return(1000*(0.3*(x^2)*((1-x)^6)+0.7*x^6*((1-x)^2)))
}

xi <- numeric(200)
for(i in 1:200) {
  xi[i] <- (i-(1/2))/200
}

fxi <- numeric(200)
for(i in 1:200) {
  fxi[i] <- fxi.gen(xi[i])
}

ei <- rnorm(200)

yi <- fxi + ei

cx.gen <- function(x) {
  return(c(1, cos(pi*x), cos(pi*x*2), cos(pi*x*3), cos(pi*x*4)))
}

Xi <- cx.gen(xi[1])
for(i in 2:200) {
  Xi <- rbind(Xi, cx.gen(xi[i]))
}

Yi <- matrix(yi, ncol=1)

betai.hat <- solve(t(Xi)%*%Xi)%*%t(Xi)%*%Yi

fxi.hat <- Xi%*%betai.hat

plot(xi, fxi, main = "Graph for real vs fitted", xlab = "X", ylab = "Y", pch = 2)
points(xi, fxi.hat, col = "blue", pch = 3)
legend("topleft", c("real", "fitted"), col = c("black", "blue"), pch = c(2, 3))
