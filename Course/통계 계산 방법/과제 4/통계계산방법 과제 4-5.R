m<-1000;
invcdfx<-function(u,y){
  -log(u)/(y+1)}
invcdfy<-function(u,x){
  -log(u)/(x+1)} 
x <- matrix(nrow=m, ncol = 2);
  x[1,2] <- runif(1,0,-log(runif(1)))
  u1<-runif(1)
  x[1,1] <- invcdfx(u1,x[1,2])
  for (j in 2:m) {
    a <- x[j-1, 1]
    u2<-runif(1,0, 1)
    x[j,2] <- invcdfy(u2,x[j-1,1])
    b <- x[j, 2]
    u3<-runif(1,0,1)
    x[j,1] <- invcdfx(u3,x[j,2])
  }


a <- apply(x,2,mean)
a<-as.matrix(a)
rownames(a) <- c("E(X1)", "E(X2)")
print(a)
