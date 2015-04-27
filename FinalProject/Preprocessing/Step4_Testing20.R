# Building trainning dataset

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
set.seed(567232)
sample <- sapply(totalLines1, function(n) rbinom(n, 1, 0.20))

# creating three new variables with the sampled lines
for (i in 1:3){
  sample[[i]] <- (sample[[i]] - 1) * -1
  n <- eval(parse(text=dataSet[i]))[as.logical(sample[[i]])]
  assign(dataSet[i], n)
}


# counting lines of subset files
totalLines1 <- c(length(blogs), length(news), length(twitter))
# sampling for testing 
set.seed(672322)
sample <- sapply(totalLines1, function(n) rbinom(n, 1, 0.02))

# creating three new variables with the sampled lines
for (i in 1:3){
  n <- eval(parse(text=dataSet[i]))[as.logical(sample[[i]])]
  assign(paste0(dataSet[i], "_subset"), n)

  write.table(eval(parse(text=paste0(dataSet[i], "_subset"))), 
              paste0("..\\_SampledData\\", dataSet[i], "_subset.txt"), row.names = FALSE)
}

# releasing memory
rm(list = ls())

# CREATING THE CORPUS WITH SUBSET FILES
mydata.corpus <- Corpus(DirSource("..\\_SampledData\\",encoding = "utf-8"), readerControl = list(language="en_US"))

save(mydata.corpus, file="..\\_CleanData\\Test1.RData")

# load(file="..\\_CleanData\\Test1.RData")


##################################################################
# Pengram
#load(file="..\\_CleanData\\5gram.RData")
n <- 5
token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  
Pentest <- c()

# 1
i <- 1
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Pentest <- rbind(Pentest, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Pentest <- rbind(Pentest, token.aux)
rm(token.aux)

# 2
i <- 2
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Pentest <- rbind(Pentest, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Pentest <- rbind(Pentest, token.aux)
rm(token.aux)

# 3
i <- 3
l <- length(mydata.corpus[[i]]$content)
l1 <- round(l/2)
token.aux <- token(mydata.corpus[[i]]$content[1:l1])
token.aux <- data.table(phrase = token.aux)
Pentest <- rbind(Pentest, token.aux)
rm(token.aux)
#
token.aux <- token(mydata.corpus[[i]]$content[(l1+1):l])
token.aux <- data.table(phrase = token.aux)
Pentest <- rbind(Pentest, token.aux)
rm(token.aux)
  
# remove duplicate ngrams and count the number of each
Pentest <- Pentest[, list (r = .N), by = phrase]
save(Pentest, file="..\\_CleanData\\5gramTest1.RData")  
##################################################################

