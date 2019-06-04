#3
 #Fixed point iteration

fx <- function(x, n, a) x^n - a
fprimex <- function(x,n) n*x^(n-1)

gx <- function(x, n, a) x - fx(x, n, a)/fprimex(x, n)

fp <- function(ftn, x0, tol = 1e-9, max.iter = 100, n, a) {
  n <- n
  a <- a
  xold <- x0
  xnew <- ftn(xold, n, a)
  iter <- 1
    while ((abs(xnew-xold) > tol) && (iter < max.iter)) {
    xold <- xnew;
    xnew <- ftn(xold, n, a);
    iter <- iter + 1
    }
  if (abs(xnew-xold) > tol) {
    return(NULL)
  } else {
    return(xnew)
  }
}

  #Newtwon-Raphson's method

fx <- function(x, n, a) x^n - a
fprimex <- function(x,n) n*x^(n-1)

newt <- function(fx,fprimex,x0,epsilon, max.iter = 100, n, a){
  diff <- 1
  iter <-  0
  x <- x0
  while ((abs(diff) > epsilon) && (iter < max.iter)) {
    if(abs(fprimex(x0, n)) < epsilon) return("incorrect specification")
    diff <- -fx(x, n, a)/fprimex(x, n)
    x <- x + diff
    iter <- iter + 1
  }
  return(x)   
}


an <- numeric(1000)
for(i in 1:1000) an[i] <- ceiling(1/((exp(1)^(1/i))-1))

head(an)
tail(an)



fn <- vector(length = 1000)
for(i in 1:1000) {
  fn[i] <- ceiling(1/(fp(gx, 1, tol = 1e-9, max.iter = 100, i, exp(1))-1))
}

head(fn)
tail(fn)

nn <- vector(length = 1000)
for(i in 1:1000) {
nn[i] <- ceiling(1/(newt(fx, fprimex, 2, 0.00005, max.iter = 100, i, exp(1))-1))
}

head(nn)
tail(nn)


#Use fixed point

