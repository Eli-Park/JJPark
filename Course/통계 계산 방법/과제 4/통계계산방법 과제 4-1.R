#f0

n <- 1000

g <- function(x) exp(-x)/(1+x^2)
f0 <- 1
x0 <- runif(n)

mean(g(x0)/f0)
sd(g(x0)/f0)


integrate(g, 0, 1)

#f1
f1 <- function(x) exp(-x)
u <- runif(n)
x1 <- -log(u)


mean((g(x1)/f1(x1)*(x1<1)))
sd((g(x1)/f1(x1)*(x1<1)))

#f2

f2 <- function(x) 1/((1+(x^2))*pi)


u <- runif(n)
x2 <- tan(pi*(u-0.5))
a <- (g(x2)/f2(x2))
a[x2 < 0 | x2 >1] <- 0
mean(a)
sd(a)


#f3
f3 <- function(x) exp(-x)/(1-exp(-1))

u <- runif(n)
x3 <- -log(u)
x3 <- x3[x3 <=1]

mean((g(x3)/f3(x3)))
sd((g(x3)/f3(x3)))

#f4
f4 <- function(x) 4/((1+x^2)*pi)

u <- runif(n)
x4 <- tan((1/4)*pi*u)

mean(g(x4)/f4(x4))
sd(g(x4)/f4(x4))
