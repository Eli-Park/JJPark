rm(list=ls())
setwd('C:/Users/pkmon/Desktop/FreePJ/TOMATO~1/review_data')

library(rvest)
library(stringr)
library(tm)
library(RTextTools)
library(tidytext)
library(tidyr)
library(dplyr)

# Crawler
CrawlerforLexi <- function(movie_name) {
  dir.create(movie_name)
  directory <- getwd()
  url0<-paste("https://www.rottentomatoes.com/m/",movie_name,"/reviews/?page=",sep="")
  htxt0<-read_html(url0)
  pageinfo<-html_node(htxt0,".pageInfo")
  pagenum0<-html_text(pageinfo)
  pages<-word(pagenum0,-1)
  for(page in 1:pages){
    url <- paste("https://www.rottentomatoes.com/m/", movie_name,
                 "/reviews/?page=", page, "&sort=", sep="")
    htxt <- read_html(url)
    icon <- html_nodes(htxt, ".review_icon")
    tomato <- character()
    for (i in 1:length(icon))
      tomato[i] <- xml_attrs(icon[[i]])[["class"]]
    fi <- (tomato == "review_icon icon small fresh")
    ri <- (tomato == "review_icon icon small rotten")
    table <- html_nodes(htxt, ".review_table") 
    content <- html_nodes(table, ".the_review")
    review <- html_text(content)
    fresh <- review[fi]
    for (j in 1:length(fresh)) {
      write.table(paste(fresh[j], "dlrlfwo"),
                  file=paste(directory,"/",movie_name,"/",movie_name,"_fresh_",page,"_",j,".txt", sep=""),
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    rotten <- review[ri]
    for (k in 1:length(rotten)) {
      write.table(rotten[k],
                  file=paste(directory,"/",movie_name,"/",movie_name,"_rotten_",page,"_",k,".txt", sep=""),
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    if(length(fresh) == 0 & length(rotten) == 0) {break}
    print(page)
  }
}

# Classifier by Lexicon
LexiClassifier <- function(movie_name) {
  directory <- paste(getwd(),'/', movie_name, sep= '')

  review <- VCorpus(DirSource(directory), readerControl =list(language="en"))
  
  n <- length(review)
  mytxt <- rep(NA,n)
  
  for (i in 1:n) mytxt[i] <- as.character(review[[i]][1])
  
  my.df.text <- data_frame(review.id=1:n, doc=mytxt)
  
  my.df.text.word <- my.df.text %>%
    unnest_tokens(word,doc)
  
  myresult.sa <- my.df.text.word %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word,review.id,sentiment) %>%
    spread(sentiment,n,fill=0)
  
  emotion <- summarise(group_by(myresult.sa, review.id),
                       fresh.sum = sum(positive),
                       rotten.sum = sum(negative),
                       fresh.sent = fresh.sum - rotten.sum)
  
  
  myfilenames <- list.files(path=directory,
                            pattern=NULL,all.files = TRUE)
  review.id <- 1:n
  review.name <- myfilenames[3:(n+2)]
  review.inf.df <- data.frame(review.id, review.name)
  emotion <- emotion %>% full_join(review.inf.df)
  sent <- c(emotion[,4])[[1]]
  split <- strsplit(as.character(c(emotion[,5])[[1]]), "_")
  label <- unlist(strsplit(as.character(c(emotion[,5])[[1]]), "_"))[(1:n) *  length(split[[1]]) - 2]
  labelall <- label
  k <- min(which(is.na(sent))) - 1
  sent <- sent[1:k]; label <- label[1:k];
  label <- label[sent != 0]; sent <- sent[sent != 0]
  
  fcorrect <- sum(label[sent > 0] == "fresh")
  rcorrect <- sum(label[sent < 0] == "rotten")
  
  accuracy <- (fcorrect + rcorrect) / length(sent)
  actualmeter <- sum(labelall == "fresh")/ length(labelall)
  estimeter <- sum(sent > 0) / length(sent)
  
  output <- data.frame(accuracy, actualmeter, estimeter)
  colnames(output) <- c("accuracy", "Tomatometer_actual", "Tomatometer_estimator")
  return(output)
}

# Trials

CrawlerforLexi("300_rise_of_an_empire")
LexiClassifier("300_rise_of_an_empire")

CrawlerforLexi("oceans_8")
LexiClassifier("oceans_8")

CrawlerforLexi("superfly_2018")
LexiClassifier("superfly_2018")

CrawlerforLexi("annabelle")
LexiClassifier("annabelle")
