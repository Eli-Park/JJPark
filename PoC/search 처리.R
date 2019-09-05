install.packages("KoNLP")
library("KoNLP")
library("tm")
library("stringr")





#ID별로 검색어 분류
#Search1
search11 <- search1
for(i in 1:nrow(search11)){
  search11[i, 3] <- paste(unlist(rep(search11[i, 3], search11[i, 4])), collapse = ", ")
}

search11 <- aggregate(search1$KWD_NM, by = list(search1$CLNT_ID), FUN = paste, sep =" ")

for(i in 1:nrow(search11)){
  search11[i,2] <- paste(unlist(search11[i,2]), collapse = " ")
}


colnames(search11) <- c("CLNT_ID", "KWD")

for(i in 1:nrow(search11)){
  text <- search11[i,2]
  write.table(text, paste("C:/Users/pkmon/Desktop/PoC/clntcorp/", search11[i,1],".txt", sep=""),
            col.names = FALSE, row.names = FALSE)
}
customercorp <- VCorpus(DirSource("C:/Users/pkmon/Desktop/PoC/clntcorp/"))
customercorp


##Search2
filt <- unique(search1$KWD_NM)
search22 <- search2[search2$KWD_NM %in% filt, ]
search22 <- search22[complete.cases(search22), ]

search2$SESS_DT <- as.character(search2$SESS_DT)
search2$SESS_DT <- as.Date(search2$SESS_DT, "%Y%m%d")

for(i in 1:nrow(search22)){
  search22[i, 2] <- paste(unlist(rep(search22[i, 2], a)), collapse = ", ")
}

search22 <- aggregate(search22$KWD_NM, by = list(search22$SESS_DT), FUN = paste, sep =" ")

for(i in 1:nrow(search22)){
  search22[i,2] <- paste(unlist(search22[i,2]), collapse = ", ")
}


colnames(search11) <- c("CLNT_ID", "KWD")

for(i in 1:nrow(search11)){
  text <- search11[i,2]
  write.table(text, paste("C:/Users/pkmon/Desktop/PoC/clntcorp/", search11[i,1],".txt", sep=""),
              col.names = FALSE, row.names = FALSE)
}
customercorp <- VCorpus(DirSource("C:/Users/pkmon/Desktop/PoC/clntcorp/"))

mycorp <- Corpus(VectorSource(search2$KWD_NM)) 
inspect(mycorp)

mycorp <- tm_map(mycorp, removeNumbers)

mytempfunct <- function(myobject, oldexp, newexp){
  newobject <- tm_map(myobject, content_transformer(function(x,pattern) gsub(pattern,newexp,x)), oldexp)
  return(newobject)
}

mycorp <- mytempfunct(mycorp, "[[:lower:]]", "")
mycorp <- mytempfunct(mycorp, "[[:upper:]]", "")
mycorp <- mytempfunct(mycorp, "\\(","")
mycorp <- mytempfunct(mycorp, "'","")
mycorp <- mytempfunct(mycorp, ".","")
mycorp <- mytempfunct(mycorp, "-","")
mycorp <- mytempfunct(mycorp, "/","")



search11 <- 