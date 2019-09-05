library("data.table")
library("tidyverse")

library("sampling")

setwd("C:/Users/pkmon/Desktop/PoC")

product <- fread("01.product.csv", sep=",", stringsAsFactors = F, data.table = F)
search1 <- fread("02.Search1.csv", sep=",", stringsAsFactors = F, data.table = F)
search2 <- fread("03.Search2.csv", sep=",", stringsAsFactors = F, data.table = F)
search2$SEARCH_CNT <- as.numeric(search2$SEARCH_CNT)
custom <- fread("04.Custom.csv", sep=",", stringsAsFactors = F, data.table = F)
session <- fread("05.Session.csv", sep=",", stringsAsFactors = F, data.table = F)
master <- fread("06.Master.csv", sep=",", stringsAsFactors = F, data.table = F)

custom <- custom[custom$CLNT_AGE == 20 | custom$CLNT_AGE == 30,]
scustom <- strata(custom, stratanames = c("CLNT_AGE", "CLNT_GENDER"),
                  size = c(2500, 2500, 2500, 2500), method = "srswor") 
custom <- getdata(custom, scustom)
custom <- custom[,1:3] #남녀, 20, 30대 2500명씩

ID <- custom$CLNT_ID
prcod <- product$PD_C

product <- product[product$CLNT_ID %in% ID,]
session <- session[session$CLNT_ID %in% ID,]
search1 <- search1[search1$CLNT_ID %in% ID,]
master <- master[master$PD_C %in% prcod,]
search2 <- search2[search2$SEARCH_CNT > 5, ]

#write.csv(custom, "scustom.csv")
#write.csv(product, "sproduct.csv")
#write.csv(session, "ssession.csv")
#write.csv(master, "smaster.csv")
#write.csv(search1, "ssearch1.csv")
#write.csv(search2, "ssearch2.csv")

custom <- fread("scustom.csv")
product <- fread("sproduct.csv")
session <- fread("ssession.csv")
master <- fread("smaster.csv")
search1 <- fread("ssearch1.csv")
search2 <- fread("ssearch2.csv")

custom <- custom[,-1]
product <- product[,-1]
session <- session[,-1]
master <- master[,-1]
search1 <- search1[,-1]
search2 <- search2[,-1]
