#(a)

cau.sim <- function(u) {
  X.c <- tan(pi*(u-0.5))
  return(X.c)
}

U <- runif(1000)
C <- round(cau.sim(U), 7)





#(b)

tgen2 <- function(n){
  v <- rgamma(1,n/2,1/2)
  tn <- rnorm(1,0,sqrt(n/v))
  return(tn)
}

C.1 <- replicate(1000, round(tgen2(1),7))



#(c)

fx <- function(x) 1/(pi*(1+x^2))


hist(C, xlim= c(-500, 1000))
hist(C.1, xlim= c(-500, 1000))
plot(fx, xlim = c(-500,1000))



#(d)

fx.1 <- function(x) 1/sqrt(2*pi)*exp(-x^2/2)

M <-optimize(f=function(x){fx.1(x)/fx(x)},maximum=T,interval=c(-100,100))$objective



rej1 <- function(fx, gx) {
  while (TRUE) {
    x <- runif(1); y <- cau.sim(x)
    z <- runif(1, 0, M*gx(y))
    if (fx(y)/(M*gx(y)) > z) return(y)} 
}

N.x <- replicate(1000, rej1(fx.1, fx)) 
N.y <- N.x[1:500]
N.x <- N.x[501:1000]

C.2 <- N.x/N.y
hist(C.2, xlim = c(-500, 1000))

