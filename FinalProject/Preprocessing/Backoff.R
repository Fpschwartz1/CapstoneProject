suppressPackageStartupMessages(library("data.table"))

setwd("C:\\z\\_Pesquisa\\Coursera\\TheDataScienceSpecialization\\CapstoneProject\\MilestoneReportRubric")

load(file="..\\_CleanData\\1gram.RData")
load(file="..\\_CleanData\\2gram.RData")
load(file="..\\_CleanData\\3gram.RData")
load(file="..\\_CleanData\\4gram.RData")
load(file="..\\_CleanData\\5gram.RData")

#####################
# beginning of step 3

split_on_space <- function (x) {
  result <- unlist (strsplit(x, split = "[ ]+"))
  result [nchar (result) > 0]
}


alphagram <- function(Ngram, N_1gram, tcontext){
  splcon <- split_on_space(tcontext)
  
  # Beta: the total discounted probability mass for all N-grams starting with that context
  b1 <- 1 - sum(Ngram[Ngram$context == tcontext, ps])
      
  # wm which followed tcontext
  wm <- Ngram[Ngram$context == tcontext, word]
    
  b2 <- 1 - sum(N_1gram[N_1gram$context == splcon[-1] & N_1gram$word %in% wm, ps])
  #b2 <- 1 - sum(N_1gram[N_1gram$context == splcon[-1], pstar])

  return(b1/b2)
}




Pkatz2 <- function(bigram){
  pkatz <- bkBigram[bkBigram$ngram == bigram, ps]
  pkatz <- sum(pkatz)
  if(pkatz > 0) return(pkatz)
  
  print("alfa")
  
  spl <- split_on_space(bigram)
  alpha <- alphagram(bkBigram, bkUnigram, spl[1])
  pkatz <- bkUnigram[bkUnigram$ngram == spl[2], pstar]
  pkatz <- sum(pkatz)
  pkatz <- alpha * pkatz
  return(pkatz)
}

Pkatz2("going home")

Pkatz3 <- function(trigram){
  pkatz <- bkTrigram[bkTrigram$ngram == trigram, ps]
  pkatz <- sum(pkatz)
  if(pkatz > 0) return(pkatz)
  
  print("alfa")
  
  spl <- split_on_space(trigram)
  alpha <- alphagram(bkTrigram, bkBigram, paste(spl[1],spl[2]))
  pkatz <- Pkatz2(paste(spl[2],spl[3]))
  pkatz <- alpha * pkatz
  return(pkatz)
}

Pkatz3("going to be")

Pkatz4 <- function(tetgram){
  pkatz <- bkTetgram[bkTetgram$ngram == tetgram, ps]
  pkatz <- sum(pkatz)
  if(pkatz > 0) return(pkatz)
  
  print("alfa")
  
  spl <- split_on_space(tetgram)
  alpha <- alphagram(bkTetgram, bkTrigram, paste(spl[1],spl[2],spl[3]))
  pkatz <- Pkatz3(paste(spl[2],spl[3],spl[4]))
  pkatz <- alpha * pkatz
  return(pkatz)
}

Pkatz4("going to be sky")

Pkatz5 <- function(pengram){
  pkatz <- bkPengram[bkPengram$ngram == pengram, ps]
  pkatz <- sum(pkatz)
  if(pkatz > 0) return(pkatz)
  
  print("alfa")

  spl <- split_on_space(pengram)
  alpha <- alphagram(bkPengram, bkTetgram, paste(spl[1],spl[2],spl[3],spl[4]))
  pkatz <- Pkatz4(paste(spl[2],spl[3],spl[4],spl[5]))
  pkatz <- alpha * pkatz
  return(pkatz)
}

Pkatz5(clean("jury to settle the account"))


Pkatz3(clean("and I'd die"))


#select the n-grams found more than once in the text
bkUnigram<-subset(bkUnigram,r>1)
bkBigram <-subset(bkBigram,r>1)
bkTrigram<-subset(bkTrigram,r>1)
bkTetgram<-subset(bkTetgram,r>1)



pred<-function(frase){
  prev <- unlist (strsplit (frase, split = " ", fixed = TRUE))
  len<-length(prev)
  fra2 <- paste(tail(prev, 1), collapse = " ")
  fra3 <- paste(tail(prev, 2), collapse = " ")
  fra4 <- paste(tail(prev, 3), collapse = " ")
  fra5 <- paste(tail(prev, 4), collapse = " ")
  
  predict<-NULL
  try(pred5 <- Pengram[context == fra5, .SD [which.max(p), word]])
  try(pred4 <- Tetgram[context == fra4, .SD [which.max(p), word]])
  try(pred3 <- Trigram[context == fra3, .SD [which.max(p), word]])
  try(pred2 <- Bigram[context == fra2, .SD [which.max(p), word]])
  if(length(head(pred5))==0){  
    if(length(head(pred4))==0){  
      if(length(head(pred3))==0){    
        if(length(head(pred2))!=0){predict<-pred2
        }else{
          predict<-"Next word, not predicted, out of vocabulary sequence"
        }
      }else{predict<-pred3}
    }else{predict<-pred4}
  }else{predict<-pred5}
  return(predict)
}

clean<-function(text){
  #load the required packages,rm,RWeka y data.table.
  suppressPackageStartupMessages(library("tm"))
  suppressPackageStartupMessages(library("stringi"))
  suppressPackageStartupMessages(library("RWeka"))
  #suppressPackageStartupMessages(library("qdap"))
  mydata.corpus<-Corpus(VectorSource(text))
  mydata.corpus<-tm_map(mydata.corpus,content_transformer(function(x) iconv(x, to='ASCII', sub='')))
  # Change text to lowercase
  mydata.corpus<-tm_map(mydata.corpus,content_transformer(tolower))
  # Eliminate numbers
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(removeNumbers))
  # Remove punctuation marks
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(removePunctuation))
  
  #bad <- rbind(read.table("profane_words.csv", quote="\""),data.frame(V1=cbind(stopwords("english"))))
  #bad <- data.frame(V1=cbind(stopwords("english")))
  #mydata.corpus <- tm_map(mydata.corpus, removeWords, bad) 
  
  #spaces are removed extra blank
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(stripWhitespace))
  mydata.corpus <- tm_map(mydata.corpus, PlainTextDocument)
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(function(x) stri_trans_tolower(x)))
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(function(x) stri_trans_general(x, "en_US")))
  text2<-unlist(mydata.corpus[[1]]$content)
  return(text2)
}

frase <- clean("I'd give anything to see arctic monkeys this")
prediction <- pred(frase) 
prediction

#cat("Next Word Prediction:", prediction)