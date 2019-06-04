rm(list=ls())
setwd('C:/Users/pkmon/Desktop/FreePJ/TOMATO~1/review_data')

library(rvest)
library(stringr)
library(tm)
library(RTextTools)

# Crawler
CrawlerbySuper <- function(movie_name, directory) {
  if (missing(directory)) directory <- getwd()
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
                  file=paste(directory,"/",movie_name,"_fresh_",page,"_",j,".txt", sep=""),
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    rotten <- review[ri]
    for (k in 1:length(rotten)) {
      write.table(rotten[k],
                  file=paste(directory,"/",movie_name,"_rotten_",page,"_",k,".txt", sep=""),
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    if(length(fresh) == 0 & length(rotten) == 0) {break}
    print(page)
  }
}

# ÀüÃ³¸®
TomatoProcessor <- function(review) {
  ppreview <- tm_map(review, removeNumbers)
  ppreview <- tm_map(ppreview, removePunctuation)
  ppreview <- tm_map(ppreview, stripWhitespace)
  ppreview <- tm_map(ppreview, content_transformer(tolower))
  ppreview <- tm_map(ppreview, removeWords, words=stopwords("SMART"))
  ppreview <- tm_map(ppreview, stemDocument, language="en")
  return(ppreview)
}

# Text -> Matrix
TomatoDTM <- function(directory) {
  if (missing(directory)) directory <- getwd()
  else directory <- paste(getwd(),directory,sep='/')
  label <- numeric()
  review <- VCorpus(DirSource(directory))
  ppreview <- TomatoProcessor(review)
  for (i in 1:length(ppreview))
    if (sum(strsplit(ppreview[[i]]$content, split = " ")[[1]] == "dlrlfwo") != 0)
      label[i] <- 1
  else label[i] <- 0
  ppreview <- tm_map(ppreview, removeWords, words=("dlrlfwo"))
  DTMreview <- DocumentTermMatrix(ppreview, control = list(weighting = function(x) weightTfIdf(x, normalize = "FALSE")))
  DTMreview <- removeSparseTerms(DTMreview, 0.9)
  return(list(DTMreview, label))
}

# Train data vs Test data
TrainTestSize <- function(DTM, ratio) {
  find <- which(DTM[[2]] == 1)
  rind <- which(DTM[[2]] == 0)
  fdiv <- 1:round(length(find) * ratio[1]/sum(ratio), 0)
  rdiv <- 1:round(length(rind) * ratio[1]/sum(ratio), 0)
  ftrain <- find[fdiv]; ftest <- find[-fdiv]; rtrain <- rind[rdiv]; rtest <- rind[-rdiv]
  return(list(trainSize = c(ftrain, rtrain),testSize = c(ftest, rtest)))
}

CrawlerbySuper("bohemian_rhapsody")
CrawlerbySuper("the_nutcracker_and_the_four_realms", directory = "train")
CrawlerbySuper("oceans_8", directory = "train")
CrawlerbySuper("superfly_2018", directory = "train")

myDTM <- TomatoDTM()

TTdivide <- TrainTestSize(myDTM, c(8,2))

mycontainer <- create_container(myDTM[[1]], myDTM[[2]],
                                       trainSize = TTdivide[[1]], TTdivide[[2]],
                                       virgin = FALSE)
mytestlabel <- myDTM[[2]][TTdivide[[2]]]
myclassifier <- train_models(mycontainer, algorithms = c("SVM", "BOOSTING"))
myresult <- classify_models(mycontainer, myclassifier)

table(myresult$SVM_LABEL, mytestlabel) 
table(myresult$SLDA_LABEL, mytestlabel)
table(myresult$BOOSTING_LABEL, mytestlabel)
?removeSparseTerms
myDTM
