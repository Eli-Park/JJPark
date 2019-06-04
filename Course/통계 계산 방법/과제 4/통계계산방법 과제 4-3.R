g <- function(x) {
  if(x < 1/6) return(1)
  else if(x < 2/6) return(2)
  else if(x < 3/6) return(3)
  else if(x < 4/6) return(4)
  else if(x < 5/6) return(5)
  else return(6)
}


fp <- function(x) {
  if(x ==1 ) return(0.01)
  else if(x ==2) return(0.39)
  else if(x ==3) return(0.11)
  else if(x ==4) return(0.18)
  else if(x ==5) return(0.26)
  else return(0.05)
}

MC <- function(N, init){
  x<-numeric(N)
  x[1] <- init
  for (i in 2:N)
  {
    y<- g(runif(1))
    alpha<-fp(y)/fp(x[i-1])
    u<-runif(1)
    if (u<alpha) x[i]<-y
    else x[i]<-x[i-1]
  }
  return(x)}
a <- MC(10000,2)
table(a)/10000

table(a[9801:10000])/200

