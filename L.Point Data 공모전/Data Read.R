library("data.table")
library("plyr")
library("recommenderlab")
setwd("C:/Users/pkmon/Desktop/BIGDATA2")

data1 <- fread("01.product.csv", sep=",", stringsAsFactors = F, data.table = F)
data2 <- fread("02.Search1.csv", sep=",", stringsAsFactors = F, data.table = F)
data3 <- fread("03.Search2.csv", sep=",", stringsAsFactors = F, data.table = F)
data4 <- fread("04.Custom.csv", sep=",", stringsAsFactors = F, data.table = F)
data5 <- fread("05.Session.csv", sep=",", stringsAsFactors = F, data.table = F)
data6 <- fread("06.Master.csv", sep=",", stringsAsFactors = F, data.table = F)


#Customer?— ?žˆ?Š” ?• ?“¤ë§? ì¶”ì¶œ
ID <- data4$CLNT_ID
data1.1 <- data1[data1$CLNT_ID %in% ID,]


data1.1$PD_BUY_AM <- gsub(",", "",data1.1$PD_BUY_AM)
data1.1$PD_BUY_AM <- as.numeric(data1.1$PD_BUY_AM)
data1.1$PD_BUY_CT <- as.numeric(data1.1$PD_BUY_CT)
data1.1$PD_BUY_CT[which(is.na(data1.1$PD_BUY_CT))] <- 0

##Total Expense
#data1.2 <- ddply(data1.1, .(CLNT_ID), summarise, TotalExpense = sum(PD_BUY_AM * PD_BUY_CT))

data1.2 <- fread("data1.2.csv", sep = ",", stringsAsFactors = F, data.table = F)
data1.2 <-data1.2[,-1]
#Device, ì§€?—­, Session
data1.3 <- data5[data5$CLNT_ID %in% ID, c(1,2,4,7,8)]

##? ‘?† ?›”
#data1.3$SESS_DT[grep(201809, data1.3$SESS_DT)] <- 9
#data1.3$SESS_DT[grep(201808, data1.3$SESS_DT)] <- 8
#data1.3$SESS_DT[grep(201807, data1.3$SESS_DT)] <- 7
#data1.3$SESS_DT[grep(201806, data1.3$SESS_DT)] <- 6
#data1.3$SESS_DT[grep(201805, data1.3$SESS_DT)] <- 5
#data1.3$SESS_DT[grep(201804, data1.3$SESS_DT)] <- 4
#write.csv(data1.3, "data1.3.csv")
data1.3 <- fread("data1.3.csv", sep = ",", stringsAsFactors = F, data.table = F)
data1.33 <- merge(data1.11, data1.3, by="SESS_ID", all=TRUE)
data1.33 <- data1.33[,-13]
#data1.31 <- table(data1.3$CLNT_ID, data1.3$SESS_DT)
#data1.31 <- as.data.frame.matrix(data1.31)
#data1.31[,7] <- row.names(data1.31)
#colnames(data1.31)[7] <- "CLNT_ID"
data1.31 <- fread("data1.31.csv", sep = ",", stringsAsFactors = F, data.table = F)
data1.31 <- data1.31[,-1]
##?ƒ?’ˆ ??€ë¶„ë¥˜
data6.1 <- data6[ ,c(1,3,4)]
data1.11 <- merge(x = data1.1, y = data6, by="PD_C",all=TRUE)
data1.11 <- data1.11[order(data1.11$CLNT_ID),]
data1.11 <- fread("data1.11.csv", sep = ",", stringsAsFactors = F, data.table = F)

#data1.11[data1.11$CLAC1_NM == "¿©¼ºÀÇ·ù", 10] <- data1.11[data1.11$CLAC1_NM == "¿©¼ºÀÇ·ù", 11] 
#write.csv(data1.11, "data1.11.csv")


data1.4 <- as.data.frame.matrix(table(data1.11$CLNT_ID, data1.11$CLAC1_NM))
data1.4[,41] <- row.names(data1.4)
colnames(data1.4)[41] <- "CLNT_ID"
data1.4[,41] <- as.numeric(data1.4[,41])
data1.4 <- fread("data1.4.csv", sep = ",", stringsAsFactors = F, data.table = F)

##Merge
data7 <- merge(data4, data1.2, by="CLNT_ID")
data7 <- merge(data7, data1.31, by="CLNT_ID")
data7 <- merge(data7, data1.4, by="CLNT_ID")
data7 <- fread("data7.csv", sep = ",", stringsAsFactors = F, data.table = F)
data7 <- data7[,-1]
data7[,2] <- as.factor(data7[,2])

#data8 <- merge(data7, data1.3, by="CLNT_ID")
data8 <- fread("data8.csv", sep = ",", stringsAsFactors = F, data.table = F)
#write.csv(data1.4, "data1.4.csv")

sum(table(unique(data1$CLNT_ID)))
sum(table(unique(data2$CLNT_ID)))
sum(table(data4$CLNT_ID))
sum(table(unique(data5$CLNT_ID)))

summary(data1)
summary(data2)
summary(data3)
summary(data4)
summary(data5)
summary(data6)

