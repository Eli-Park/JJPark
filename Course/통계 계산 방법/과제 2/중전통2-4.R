#4

fx <- function(x) log(x) - exp(-x)
fxp <- function(x) 1/x + exp(-x)
fx2p <- function(x) -1/x^2 - exp(-x)
fx3p <- function(x) 2*(1/x^3) + exp(-x)
fx4p <- function(x) -6*(1/x^4) - exp(-x)
fx5p <- function(x) 24*(1/x^5) + exp(-x)
D <- function(x, h) fxp(x) + h^2/factorial(3)*fx3p(x) + h^4/factorial(5)*fx5p(x)
R <- function(x, h = .Machine$double.eps, p = 2) (2^p*D(x,h) - D(x, 2*h))/(2^p-1)


secant <- function(ftn, x0, x1, tol = 1e-9, max.iter = 100){
  f0 <- ftn(x0); f1 <- ftn(x1); iter <-  0
  while ((abs(f1) > tol) && (iter < max.iter)) {
    if (f0 == f1) {
      return("Algorithm failed with f0 == f1")}
    x2 <- x1 - f1*(x1 - x0)/(f1 - f0)
    x0 <- x1; f0 <- f1; x1 <- x2
    f1 <- ftn(x1);  iter <-  iter + 1 }
  return(x1) }

h <- function(h) (4/3)*D(1, h) - (1/3)*D(1, 2*h) - exp(-1)


secant(h,0,1)

1-fx(1)/R(1, 1.053358)

#Richardson extrapolation(p=2)
## x0 = 1, x1 = 2

secant.Richardson <- function(x0, h = 1.053358, tol = 1e-9, max.iter = 100) {
  nx <-0
  ox <- x0
  iter <- 0
  while((abs(fx(ox)/R(ox,h)) > tol && (iter < max.iter))) {
    nx <- ox - fx(ox)/R(ox,h)
    ox <- nx
    iter <- iter +1
  }
  return(nx)
}

secant.Richardson(1)
