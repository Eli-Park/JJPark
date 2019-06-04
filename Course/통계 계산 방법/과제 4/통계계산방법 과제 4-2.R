#(a)
G <- function(x, m, n) {
  gamma((1/2)*(m+n))*(m^(m/2))*(n^(n/2))*(x^((m/2)-1))/(gamma(m/2)*gamma(n/2)*((m*x+n)^((m+n)/2)))
} 


rF <- function(n) {
  x <- numeric(n)
  for(i in 1:n) {
    x[i] <- (sum(rnorm(20)^2))/20/((sum(rnorm(9)^2))/9)
  }
  return(x)
}




c <- rF(100000)
print(sum(table(c[c>15]))/100000, digits = 10)

1-pf(15, 20, 9)

#(b)

f <- function(x, n=9) ((n/2)*15^(n/2))/((x^((n/2)+1)))

incdf <- function(x, n=9) (15^(-n/2)-(3^(-n/2))*x)^(-2/n)
u <- runif(100000, 0, 0.00071)
X <- incdf(u)


P <- mean((G(X, 20, 9)/f(X, 9)))
P

