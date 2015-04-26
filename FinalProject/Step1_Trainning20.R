
# Packages used
suppressPackageStartupMessages(library("tm"))
suppressPackageStartupMessages(library("RWeka"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("data.table"))

# auxiliary variables
setwd("C:\\z\\_Pesquisa\\Coursera\\TheDataScienceSpecialization\\CapstoneProject\\MilestoneReportRubric")
path     <- "..\\__final\\en_US\\"
fileName <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
dataSet  <- c("blogs", "news", "twitter")

# reading files and assigning them to variables whose names are in dataSet
for(i in 1:3){
  f <- file(paste0(path, fileName[i]), "rb")
  assign(dataSet[i], readLines(f, encoding="utf-8", skipNul=TRUE))
  close(f)
}

# counting lines of original files
totalLines1 <- c(length(blogs), length(news), length(twitter))

# SAMPLING FILE LINES - sampled data is 20% of total ammount 
#set.seed(56723)
set.seed(567232)
sample <- sapply(totalLines1, function(n) rbinom(n, 1, 0.20))

# creating three new variables with the sampled lines
for (i in 1:3){
  n <- eval(parse(text=dataSet[i]))[as.logical(sample[[i]])]
  assign(paste0(dataSet[i], "_subset"), n)
  
  write.table(eval(parse(text=paste0(dataSet[i], "_subset"))), 
              paste0("..\\_SampledData\\", dataSet[i], "_subset.txt"), row.names = FALSE)
}

# releasing memory
rm(blogs, news, twitter, blogs_subset, news_subset, twitter_subset, sample, f, i, n)


# CREATING THE CORPUS WITH SUBSET FILES
mydata.corpus <- Corpus(DirSource("..\\_SampledData\\",encoding = "utf-8"), readerControl = list(language="en_US"))

# PREPROCESSING
#load the required packages,rm,RWeka y data.table.
suppressPackageStartupMessages(library("stringi"))
mydata.corpus <-tm_map(mydata.corpus,content_transformer(function(x) iconv(x, to='ASCII', sub='')))
# Change text to lowercase
mydata.corpus <-tm_map(mydata.corpus,content_transformer(tolower))
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
# Saving clean data
save(mydata.corpus, file="..\\_CleanData\\SClean20.RData")

# releasing memory
rm(list = ls())

## Tokenization
setwd("C:\\z\\_Pesquisa\\Coursera\\TheDataScienceSpecialization\\CapstoneProject\\MilestoneReportRubric")
load(file="..\\_CleanData\\SClean20.RData")


#perform the calculation of each n-gram of 1-5 grams

##################################################################
# Unigram
# load(file="..\\_CleanData\\2gram.RData")
n <- 1
token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))

Unigram <- c()

# 1
i <- 1
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Unigram <- rbind(Unigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Unigram <- rbind(Unigram, token.aux)
rm(token.aux)

# 2
i <- 2
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Unigram <- rbind(Unigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Unigram <- rbind(Unigram, token.aux)
rm(token.aux)

# 3
i <- 3
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Unigram <- rbind(Unigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Unigram <- rbind(Unigram, token.aux)
rm(token.aux)

# remove duplicate ngrams and count the number of each
Unigram <- Unigram[, list (r = .N), by = phrase]
save(Unigram, file="..\\_CleanData\\1gram.RData")  
##################################################################

##################################################################
# Bigram
# load(file="..\\_CleanData\\2gram.RData")
n <- 2
token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))

Bigram <- c()

# 1
i <- 1
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Bigram <- rbind(Bigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Bigram <- rbind(Bigram, token.aux)
rm(token.aux)

# 2
i <- 2
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Bigram <- rbind(Bigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Bigram <- rbind(Bigram, token.aux)
rm(token.aux)

# 3
i <- 3
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Bigram <- rbind(Bigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Bigram <- rbind(Bigram, token.aux)
rm(token.aux)

# remove duplicate ngrams and count the number of each
Bigram <- Bigram[, list (r = .N), by = phrase]
save(Bigram, file="..\\_CleanData\\2gram.RData")  
##################################################################

##################################################################
# Trigram
#load(file="..\\_CleanData\\3gram.RData")
n <- 3
token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))

Trigram <- c()

# 1
i <- 1
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Trigram <- rbind(Trigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Trigram <- rbind(Trigram, token.aux)
rm(token.aux)

# 2
i <- 2
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Trigram <- rbind(Trigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Trigram <- rbind(Trigram, token.aux)
rm(token.aux)

# 3
i <- 3
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Trigram <- rbind(Trigram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Trigram <- rbind(Trigram, token.aux)
rm(token.aux)

# remove duplicate ngrams and count the number of each
Trigram <- Trigram[, list (r = .N), by = phrase]
save(Trigram, file="..\\_CleanData\\3gram.RData")  
##################################################################

##################################################################
# Tetgram
#load(file="..\\_CleanData\\4gram.RData")
n <- 4
token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))

Tetgram <- c()

# 1
i <- 1
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Tetgram <- rbind(Tetgram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Tetgram <- rbind(Tetgram, token.aux)
rm(token.aux)

# 2
i <- 2
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Tetgram <- rbind(Tetgram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Tetgram <- rbind(Tetgram, token.aux)
rm(token.aux)

# 3
i <- 3
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Tetgram <- rbind(Tetgram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Tetgram <- rbind(Tetgram, token.aux)
rm(token.aux)

# remove duplicate ngrams and count the number of each
Tetgram <- Tetgram[, list (r = .N), by = phrase]
save(Tetgram, file="..\\_CleanData\\4gram.RData")  
##################################################################

##################################################################
# Pengram
#load(file="..\\_CleanData\\5gram.RData")
n <- 5
token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  
Pengram <- c()

# 1
i <- 1
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Pengram <- rbind(Pengram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Pengram <- rbind(Pengram, token.aux)
rm(token.aux)

# 2
i <- 2
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Pengram <- rbind(Pengram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Pengram <- rbind(Pengram, token.aux)
rm(token.aux)

# 3
i <- 3
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Pengram <- rbind(Pengram, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Pengram <- rbind(Pengram, token.aux)
rm(token.aux)
  
# remove duplicate ngrams and count the number of each
Pengram <- Pengram[, list (r = .N), by = phrase]
save(Pengram, file="..\\_CleanData\\5gram.RData")  
##################################################################

