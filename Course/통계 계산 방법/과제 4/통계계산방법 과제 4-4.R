#Normal



t.normal =function(N,b,init, v){
  x<-rep(init,N)
  for (i in 2:N)
  {
    y<-rnorm(1,x[i-1],b)
    alpha<-(((1+y^2)/(1+x[i-1]^2))^(-(v+1)/2))
    u<-runif(1)
    if (u<alpha) x[i]<-y
    else x[i]<-x[i-1]
  }
  return(x)}

plot(t.normal(1000, sqrt(0.1), 5, 4), xlab="i", ylab="X[i]", main="sigma = 0.1", type="l")
plot(t.normal(1000, 1, 5, 4), xlab="i", ylab="X[i]", main="sigma = 1",type="l")
plot(t.normal(1000, sqrt(2), 5, 4), xlab="i", ylab="X[i]", main="sigma = 2",type="l")
plot(t.normal(1000, sqrt(10), 5, 4), xlab="i", ylab="X[i]", main="sigma = 10",type="l")

t.cau =function(N,init, v){
  x<-rep(init,N)
  for (i in 2:N)
  {
    u <- runif(1)
    y <- tan(pi*(u-0.5))
    alpha<-(((1+y^2)/(1+x[i-1]^2))^(-(v+1)/2))*(dcauchy(x[i-1])/dcauchy(y))
    u<-runif(1)
    if (u<alpha) x[i]<-y
    else x[i]<-x[i-1]
  }
  return(x)}

plot(t.cau(1000, 5, 4), xlab="i", ylab="X[i]", main="Cauchy", type="l")

