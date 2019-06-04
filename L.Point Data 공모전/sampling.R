library("plyr")
library("data.table")

setwd("C:/Users/pkmon/Desktop/BIGDATA2")
data7 <- fread("data7.csv", sep = ",", stringsAsFactors = F, data.table = F)
data7 <- data7[,-1]
data7[,2] <- as.factor(data7[,2])
data7 <- data7[data7$TotalExpense < 4000000,]



IND <- table(data7$CLNT_GENDER, data7$CLNT_AGE)/nrow(data7)
IND <- IND * 5000
f10 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 10, ]
m10 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 10, ]
f20 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 20, ]
m20 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 20, ]
f30 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 30, ]
m30 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 30, ]
f40 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 40, ]
m40 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 40, ]
f50 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 50, ]
m50 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 50, ]
f60 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 60, ]
m60 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 60, ]
f70 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 70, ]
m70 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 70, ]
f80 <- data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == 80, ]
m80 <- data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == 80, ]

f10 <- f10[sample(1:nrow(f10), round(IND[1,1])),]
f20 <- f20[sample(1:nrow(f20), round(IND[1,2])),]
f30 <- f30[sample(1:nrow(f30), round(IND[1,3])),]
f40 <- f40[sample(1:nrow(f40), round(IND[1,4])),]
f50 <- f50[sample(1:nrow(f50), round(IND[1,5])),]
f60 <- f60[sample(1:nrow(f60), round(IND[1,6])),]
f70 <- f70[sample(1:nrow(f70), round(IND[1,7])),]
f80 <- f80[sample(1:nrow(f80), round(IND[1,8])),]
m10 <- m10[sample(1:nrow(m10), round(IND[2,1])),]
m20 <- m20[sample(1:nrow(m20), round(IND[2,2])),]
m30 <- m30[sample(1:nrow(m30), round(IND[2,3])),]
m40 <- m40[sample(1:nrow(m40), round(IND[2,4])),]
m50 <- m50[sample(1:nrow(m50), round(IND[2,5])),]
m60 <- m60[sample(1:nrow(m60), round(IND[2,6])),]
m70 <- m70[sample(1:nrow(m70), round(IND[2,7])),]
m80 <- m80[sample(1:nrow(m80), round(IND[2,8])),]

sampling <- rbind(f10, f20, f30, f40, f50, f60, f70, f80, m10, m20, m30, m40, m50, m60, m70, m80)
write.csv(sampling, "5000sampling.csv")
paste("a", 10, sep="") <- 20
samplin <- function(x){
  for(i in 1:8){
  assign(paste("f", i*10, sep=""), data7[data7$CLNT_GENDER == "F" & data7$CLNT_AGE == (i*10), ])
  assign(paste("m", i*10, sep=""), data7[data7$CLNT_GENDER == "M" & data7$CLNT_AGE == (i*10), ])
  }
  IND <- table(data7$CLNT_GENDER, data7$CLNT_AGE)/nrow(data7)
  IND <- IND * x
  f10 <- f10[sample(1:nrow(f10), round(IND[1,1])),]
  f20 <- f20[sample(1:nrow(f20), round(IND[1,2])),]
  f30 <- f30[sample(1:nrow(f30), round(IND[1,3])),]
  f40 <- f40[sample(1:nrow(f40), round(IND[1,4])),]
  f50 <- f50[sample(1:nrow(f50), round(IND[1,5])),]
  f60 <- f60[sample(1:nrow(f60), round(IND[1,6])),]
  f70 <- f70[sample(1:nrow(f70), round(IND[1,7])),]
  f80 <- f80[sample(1:nrow(f80), round(IND[1,8])),]
  m10 <- m10[sample(1:nrow(m10), round(IND[2,1])),]
  m20 <- m20[sample(1:nrow(m20), round(IND[2,2])),]
  m30 <- m30[sample(1:nrow(m30), round(IND[2,3])),]
  m40 <- m40[sample(1:nrow(m40), round(IND[2,4])),]
  m50 <- m50[sample(1:nrow(m50), round(IND[2,5])),]
  m60 <- m60[sample(1:nrow(m60), round(IND[2,6])),]
  m70 <- m70[sample(1:nrow(m70), round(IND[2,7])),]
  m80 <- m80[sample(1:nrow(m80), round(IND[2,8])),]
  sampling <- rbind(f10, f20, f30, f40, f50, f60, f70, f80, m10, m20, m30, m40, m50, m60, m70, m80)
  write.csv(sampling, paste(paste(x,"sampling",sep=""),"csv", sep=","))
}
sampli <- fread("5000sampling.csv", sep = ",", stringsAsFactors = F, data.table = F)
summary(sampling$TotalExpense)
plot(sort(sampling$TotalExpense))
