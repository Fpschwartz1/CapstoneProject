suppressPackageStartupMessages(library("data.table"))

setwd("C:\\z\\_Pesquisa\\Coursera\\TheDataScienceSpecialization\\CapstoneProject\\MilestoneReportRubric")

load(file="..\\_CleanData\\1gram.RData")
load(file="..\\_CleanData\\2gram.RData")
load(file="..\\_CleanData\\3gram.RData")
load(file="..\\_CleanData\\4gram.RData")
load(file="..\\_CleanData\\5gram.RData")

#####################
# beginning of step 3

# reducing table sizes
Unigram$rstar = NULL; Unigram$phrase = NULL; # Unigram$r = NULL

Bigram$prediction = NULL; Bigram$token1 = NULL; Bigram$rstar = NULL
Bigram$rcont = NULL; Bigram$phrase = NULL; Bigram$context = NULL; 
#Bigram$r = NULL
setcolorder(Bigram, c("tk1","pred","r","pstar","ps"))
Bigram <- Bigram[!is.na(pred),]
Bigram <- Bigram[!is.na(ps),]

Trigram$prediction = NULL; Trigram$token2 = NULL; Trigram$token1 = NULL; 
Trigram$rstar = NULL; Trigram$rcont = NULL; Trigram$phrase = NULL; Trigram$context = NULL
# Trigram$r = NULL
setcolorder(Trigram, c("tk1","tk2","pred","r","pstar","ps"))
Trigram <- Trigram[!is.na(pred),]
Trigram <- Trigram[!is.na(ps),]

Tetgram$prediction = NULL; Tetgram$token3 = NULL; Tetgram$token2 = NULL; Tetgram$token1 = NULL; 
Tetgram$rstar = NULL; Tetgram$rcont = NULL; Tetgram$phrase = NULL; Tetgram$context = NULL
# Tetgram$r = NULL
setcolorder(Tetgram, c("tk1","tk2","tk3","pred","r","pstar","ps"))
Tetgram <- Tetgram[!is.na(pred),]
Tetgram <- Tetgram[!is.na(ps),]

Pengram$prediction = NULL; Pengram$token4 = NULL; Pengram$token3 = NULL; Pengram$token2 = NULL; 
Pengram$token1 = NULL; Pengram$rstar = NULL; Pengram$rcont = NULL; Pengram$phrase = NULL
Pengram$context = NULL; 
# Pengram$r = NULL
setcolorder(Pengram, c("tk1","tk2","tk3","tk4","pred","r","pstar","ps"))
Pengram <- Pengram[!is.na(pred),]
Pengram <- Pengram[!is.na(ps),]

save(Unigram, file="..\\_CleanData\\1gram.RData")
save(Bigram,  file="..\\_CleanData\\2gram.RData")
save(Trigram, file="..\\_CleanData\\3gram.RData")
save(Tetgram, file="..\\_CleanData\\4gram.RData")
save(Pengram, file="..\\_CleanData\\5gram.RData")

# reducing size
Bigram <-subset(Bigram,r>1)
Trigram<-subset(Trigram,r>1)
Tetgram<-subset(Tetgram,r>1)
Pengram<-subset(Pengram,r>1)

# our data saved in files .rds
saveRDS(Pengram, "..\\_CleanData\\Pengram.rds")
saveRDS(Tetgram, "..\\_CleanData\\Tetgram.rds")
saveRDS(Trigram, "..\\_CleanData\\Trigram.rds")
saveRDS(Bigram,  "..\\_CleanData\\Bigram.rds")
saveRDS(Unigram, "..\\_CleanData\\Unigram.rds")

# testing prediction
Unigram <- readRDS("..\\_CleanData\\Unigram.rds")
Bigram  <- readRDS("..\\_CleanData\\Bigram.rds")
Trigram <- readRDS("..\\_CleanData\\Trigram.rds")
Tetgram <- readRDS("..\\_CleanData\\Tetgram.rds")
Pengram <- readRDS("..\\_CleanData\\Pengram.rds")

split_on_space <- function (x) {
  result <- unlist (strsplit(x, split = "[ ]+"))
  result [nchar (result) > 0]
}

# find the word index in the Unigram table of each word in the phrase
wordindex <- function(phrase){
  phrase <- tidytext(phrase)
  phrase <- split_on_space(phrase)
  as.vector(unlist(sapply(phrase, function(x) Unigram[Unigram$token1 == x, id])))
}

predict <- function(phrase){

  vindices <- wordindex(phrase)
  
  ipred <- NULL
  n <- length(vindices)

  if(n>4) vindices <- vindices[(4-n):-1]
  
  if(n == 4) {
    ipred <- Pengram[tk1 == vindices[1] & tk2 == vindices[2] &
                     tk3 == vindices[3] & tk4 == vindices[4],  
                     .SD [which.max(ps), pred]]
    if(sum(ipred,0) == 0) {n<-3; vindices <- vindices[-1]}
  }

  if(n == 3) {
    ipred <- Tetgram[tk1 == vindices[1] & tk2 == vindices[2] &
                       tk3 == vindices[3],  
                     .SD [which.max(ps), pred]]
    if(sum(ipred,0) == 0) {n<-2; vindices <- vindices[-1]}
  }
  
  if(n == 2) {
    ipred <- Trigram[tk1 == vindices[1] & tk2 == vindices[2],
                     .SD [which.max(ps), pred]]
    if(sum(ipred,0) == 0) {n<-1; vindices <- vindices[-1]}
  }
  
  if(n == 1) ipred <- Bigram[tk1 == vindices[1], .SD [which.max(ps), pred]]
  
  if(sum(ipred,0) == 0) return(NULL)
  else Unigram[id == ipred, token1]
}


split_on_space <- function (x) {
  result <- unlist (strsplit(x, split = "[ ]+"))
  result [nchar (result) > 0]
}

tidytext <- function(text){
  #load the required packages,rm,RWeka y data.table.
  suppressPackageStartupMessages(library("tm"))
  suppressPackageStartupMessages(library("stringi"))
  suppressPackageStartupMessages(library("RWeka"))

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
  text<-unlist(mydata.corpus[[1]]$content)
  return(text)
}


# accuracy
load(file="..\\_CleanData\\5gramTest1.RData")

t<-Pentest[r>1, ]
t[, nword := split_on_space(phrase)[5], by = phrase]
t2 <- sapply(t$phrase, predict)
t3 <- sapply(t2, function(x) if(is.null(x)) {return("--")} else {return(x)}     )
t3 <- as.vector(unlist(t3))
t[, predi := t3]
t[, test  := predi == nword]
sum(t[, test])/nrow(t)
test1 <- t
save(test1, file="..\\_CleanData\\Test1.RData")


t<-Pentest[as.logical(rbinom(length(Pentest$phrase),1,0.02)), ]


# profanity
profanity <- read.csv("profane_words.csv")
names(profanity) <- "word"
profanity <- rbind(data.frame(word="4r5e"),profanity)
profanity$word <- sapply(profanity$word,tidytext)
saveRDS(profanity, "..\\_CleanData\\profanity.rds")








