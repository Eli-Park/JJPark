a<-tapply(data7$TotalExpense, data7$CLNT_GENDER, sum)
a[2]/sum(a)

for(i in 6:11){
  Gender_Month<- rbind(a, tapply(data7[,i], data7$CLNT_GENDER, sum))
}
table(data1.3$SESS_DT)
for(i in 6:11){
  MonthlySales<- rbind(MonthlySales, tapply(data7[,i], data7$CLNT_GENDER, sum))
}
table(data7$CLNT_GENDER)
tapply(data7$TotalExpense, data7$CLNT_GENDER, mean)
tapply(data7$TotalExpense, data7$CLNT_AGE, mean)
table(data5$DVC_CTG_NM)
par()
barplot(table(data1.33$ZON_NM), las=1)

?barplot

library("ggplot2")
library("kohonen")

table(data7$CLNT_GENDER)
ggplot(data7, mapping = aes(x=CLNT_GENDER, color = CLNT_GENDER, fill = CLNT_GENDER)) +
  geom_histogram(stat="count", bindwidth = 1)

ggplot(data7, mapping = aes(x=CLNT_AGE, color = CLNT_GENDER, fill = CLNT_GENDER)) +
  geom_histogram(stat="count", bindwidth = 1)
                 
ggplot(data8, mapping = aes(x=CLNT_AGE, fill = DVC_CTG_NM)) +
  geom_histogram(stat="count", bindwidth = .5)
