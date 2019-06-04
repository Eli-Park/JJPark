library(sem)
library(DiagrammeR)

#10.1
#(a)
28 - 17

#(b)
0.88*0.82*(0.43 - 0.29*0.26)

#(c)
gam = matrix(c(0.43, -0.29), 1, 2)
phi = matrix(c(1, 0.26, 0.26, 1), 2, 2)
phi
reliability = gam%*%phi%*%t(gam)
#reliabiliy = 0.204156

#10.3, n=200
data1 <- read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch09/CHANNELS.txt")
a<-c(paste("X", 1:6, sep=""),paste("Y", 1:6, sep=""))
dimnames(data1) <- list(a, a)
data.1 <- as.matrix(data1)
head(data.1)

#Modeling
model1 <- specifyModel()
  manufacturer -> X1, lamda.x1, NA
  manufacturer -> X2, lamda.x2, NA
  manufacturer -> X3, lamda.x3, NA
  retailer -> X4, lamda.x4, NA
  retailer -> X5, lamda.x5, NA
  retailer -> X6, lamda.x6, NA
  dissatisfaction -> Y1, NA, 1
  dissatisfaction -> Y2, lamda.y2, NA
  dissatisfaction -> Y3, lamda.y3, NA
  intention -> Y4, NA, 1
  intention -> Y5, lamda.y5, NA
  intention -> Y6, lamda.y6, NA
  manufacturer <-> manufacturer, NA, 1
  retailer <-> retailer, NA, 1
  manufacturer -> dissatisfaction, gamma.11, NA
  manufacturer -> intention, gamma.12, NA
  retailer -> dissatisfaction, gamma.21, NA
  retailer -> intention, gamma.22, NA
  manufacturer <-> retailer, phi12, NA
  dissatisfaction <-> dissatisfaction, zeta1, NA
  intention <-> intention, zeta2, NA
  X1 <-> X1, theta.delta.1, NA
  X2 <-> X2, theta.delta.2, NA
  X3 <-> X3, theta.delta.3, NA
  X4 <-> X4, theta.delta.4, NA
  X5 <-> X5, theta.delta.5, NA
  X6 <-> X6, theta.delta.6, NA
  Y1 <-> Y1, theta.epsilon.1, NA
  Y2 <-> Y2, theta.epsilon.2, NA
  Y3 <-> Y3, theta.epsilon.3, NA
  Y4 <-> Y4, theta.epsilon.4, NA
  Y5 <-> Y5, theta.epsilon.5, NA
  Y6 <-> Y6, theta.epsilon.6, NA
  
#conduct SEM
options(fit.indices=c("GFI", "AGFI", "RMSEA", "AIC", "BIC", "CAIC"))
  
fit <- sem(model1, S=data.1, N=200)
## P-value가 너무 작아.. -> 모형에 다른 모수를 추가? or 적합도가 떨어진다는 결론
summary(fit)
pathDiagram(fit, style = "ram", ignore.double=FALSE, 
            error.nodes=TRUE, edge.labels='values')

  
#10.5, n=236
  data2 <- readMoments("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch10/ATT_INT_BEH.txt", diag=TRUE,
                                   names=c("X1", "X2", "Y1", "Y2", "Y3", "Y4")) 
  for(i in 1:nrow(data2)){
     for(j in i:nrow(data2)){
        data2[i,j] <- data2[j,i]
      }
    }

model.att <- specifyModel()
  intention -> Y1, NA, 1 
  intention -> Y2, lambda.y2, NA
  behavior -> Y3, NA, 1
  behavior -> Y4, lambda.y4, NA
  attitude -> X1, lambda.x1, NA
  attitude -> X2, lambda.x2, NA
  attitude -> intention, gamma.11, NA
  attitude -> behavior, gamma.12, NA
  intention -> behavior, gamma.2, NA
  attitude <-> attitude, NA, 1
  intention <-> intention, zeta.1, NA
  behavior <-> behavior, zeta.2, NA
  Y1 <-> Y1, theta.epsilon.11, NA
  Y2 <-> Y2, theta.epsilon.22, NA
  Y3 <-> Y3, theta.epsilon.33, NA
  Y4 <-> Y4, theta.epsilon.44, NA
  X1 <-> X1, theta.delta.11, NA
  X2 <-> X2, theta.delta.22, NA
  
  
#conduct SEM
options(fit.indices=c("GFI", "AGFI", "RMSEA", "AIC", "BIC", "CAIC"))
  
fit <- sem(model.att, S=data2, N=236)
summary(fit)
  
pathDiagram(fit, style="ram", 
              ignore.double = FALSE, error.nodes = TRUE,
              edge.labels="values")
#(b)
fit$coeff
  
#10.7
data3 <- read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch10/MATH_ATTITUDE.txt", row.names=1)
colnames(data3) <- c(paste("X",c(11, 12, 13, 21, 22, 23),sep=""), paste("Y",1:3,sep=""), "score")  
math.cat <- data3[,-10]
head(math.cat)
math = read.table("C:/Users/pkmon/Desktop/다변량과제2/DATA/Ch10/MATH_ATTITUDE.txt", col.names = c("ID", paste0("X", c(11:13, 21:23)), paste0("Y",c(1:3)), "score"))
math.cat=math[,-c(1,11)]
pairs(math[,-c(1,11)])
ggpairs(math[,-c(1,11)],  alpha = 0.4)

model.math <- specifyModel()
  Fall -> X11, lambda.xf1, NA
  Fall -> X12, lambda.xf2, NA
  Fall -> X13, lambda.xf3, NA
  Spring -> X21,  lambda.xs1, NA
  Spring -> X22,  lambda.xs2, NA
  Spring -> X23,  lambda.xs3, NA
  Confident -> X11, lambda.x1, NA
  Confident -> X21, lambda.x1, NA
  Inept -> X12, lambda.x2, NA
  Inept -> X22, lambda.x2, NA
  Happy -> X13, lambda.x3, NA
  Happy -> X23, lambda.x3, NA
  Evaluate -> Y1, NA, 1
  Evaluate -> Y2, lambda.y2, NA
  Evaluate -> Y3, lambda.y3, NA
  Fall -> Evaluate, gamma1, NA
  Spring -> Evaluate, gamma2, NA
  Confident <-> Confident, NA, 1
  Inept <-> Inept, NA, 1
  Happy <-> Happy, NA, 1
  Fall <-> Fall, NA, 1
  Spring <-> Spring, NA, 1
  Fall <-> Spring, phi.fs, NA
  Evaluate <-> Evaluate, zeta, NA
  Y1 <-> Y1, theta.epsilon.11, NA
  Y2 <-> Y2, theta.epsilon.22, NA
  Y3 <-> Y3, theta.epsilon.33, NA
  X11 <-> X11, theta.delta.111, NA
  X12 <-> X12, theta.delta.122, NA
  X13 <-> X13, theta.delta.133, NA
  X21 <-> X21, theta.delta.211, NA
  X22 <-> X22, theta.delta.222, NA
  X23 <-> X23, theta.delta.233, NA
 


#(a)
math.fit = sem(model.math, data = math.cat) ; 
summary(math.fit)
pathDiagram(math.fit, style="ram", 
            ignore.double = FALSE, error.nodes = TRUE,
            edge.labels="values")

#(b)
hist(data3$score)
group = data3$score > median(data3$score)
math.cat[group,]
fit1 <- sem(model.math, data=math.cat[group,], standardized=T)
summary(fit1)
pathDiagram(fit1, style="ram", 
            ignore.double = FALSE, error.nodes = TRUE,
            edge.labels="values")

fit2 <- sem(model.math, data=math.cat[!group,], standardized=T)
summary(fit2)
pathDiagram(fit2, style="ram", 
            ignore.double = FALSE, error.nodes = TRUE,
            edge.labels="values")
