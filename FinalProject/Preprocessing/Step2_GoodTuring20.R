# estimating probabilities

suppressPackageStartupMessages(library("data.table"))

setwd("C:\\z\\_Pesquisa\\Coursera\\TheDataScienceSpecialization\\CapstoneProject\\MilestoneReportRubric")

load(file="..\\_CleanData\\1gram.RData")
load(file="..\\_CleanData\\2gram.RData")
load(file="..\\_CleanData\\3gram.RData")
load(file="..\\_CleanData\\4gram.RData")
load(file="..\\_CleanData\\5gram.RData")

# begining of step 2

# Simple Good-Turing
SGT <- function(ngram){

  rgram <- setorder(ngram[, list(n = .N), by = r], r)
  
  N <- sum(rgram$r * rgram$n) # the number of individuals in the sample
  Po <- rgram$n[1] / N # estimate of the total probability of all unseen species
  rgram[, i := c(0, rgram$r[1:length(rgram$r)-1])] # immediately previous row
  rgram[, k := c(rgram$r[2:nrow(rgram)], 2*rgram$r[nrow(rgram)] - rgram$r[nrow(rgram)-1])] # immediately following row
  rgram[, Z := 2*n / (k-i)]
  rgram[, logr := log10(r)]
  rgram[, logZ := log10(Z)]
  fit <- lm(logZ ~ logr, data = rgram)
  Sr <- function(r) coef(fit)[1] + coef(fit)[2] * log10(r)
  rgram[, Sr := Sr(r)]
  rgram[, cond1 := (r+1 == k)] # cond1 
  
  rgram[, n1  := c(rgram$n[2:nrow(rgram)], 0)]
  rgram[, Sr1 := c(rgram$Sr[2:nrow(rgram)], 0)]
  
  rgram[, x := (r+1) * n1 / n]
  rgram[, y := (r+1) * Sr1 / Sr]
  rgram[, Absxy := abs(x-y)]
  rgram[, threshold := 1.96 * sqrt( (r+1)^2 * (n1/n^2) * (1 + n1/n) ) ]
  rgram[, cond2 := Absxy > threshold] # cond2
  
  cond <- function(r, x, y, cond1, cond2){
    if(!cond1) return(r)
    else{
      if(cond2) 
        return(x)
      else
        return(y)
    }
  }
  
  rstar <- NULL
  for(i in 1:nrow(rgram)){
    rstar <- c(rstar, cond(rgram$r[i], rgram$x[i], rgram$y[i], rgram$cond1[i], rgram$cond2[i]))  
  }
  
  rgram[, rstar := rstar]
  N1 <- sum(rgram$n * rgram$rstar)
  
  rgram[, pstar := (1-Po) * rstar/N1] # p is the SGT estimate for the population frequency of a species whose frequency in the sample is r

  rgram[, i := NULL]; rgram[, k := NULL]; rgram[, Sr := NULL]; rgram[, cond1 := NULL]
  rgram[, n1 := NULL]; rgram[, Sr1 := NULL]; rgram[, x := NULL]; rgram[, y := NULL]
  rgram[, Absxy := NULL]; rgram[, threshold := NULL]; rgram[, cond2 := NULL]
  rgram[, Z := NULL]; rgram[, logr := NULL]; rgram[, logZ := NULL]; rgram[, n := NULL]
  
  rgram
}

rUnigram <- SGT(Unigram)
rBigram  <- SGT(Bigram)
rTrigram <- SGT(Trigram)
rTetgram <- SGT(Tetgram)
rPengram <- SGT(Pengram)

Unigram <- merge(Unigram, rUnigram, by = "r", all = TRUE)
Bigram  <- merge(Bigram, rBigram, by = "r", all = TRUE)
Trigram <- merge(Trigram, rTrigram, by = "r", all = TRUE)
Tetgram <- merge(Tetgram, rTetgram, by = "r", all = TRUE)
Pengram <- merge(Pengram, rPengram, by = "r", all = TRUE)

# split on space
split_on_space <- function (x) {
  result <- unlist (strsplit (x, split = "[ ]+"))
  result [nchar (result) > 0]
}

# creating context and tokens
Pengram[, `:=` (
                context = paste(split_on_space(phrase)[1],
                                split_on_space(phrase)[2],
                                split_on_space(phrase)[3],
                                split_on_space(phrase)[4]),
                token1 = split_on_space(phrase)[1],
                token2 = split_on_space(phrase)[2],
                token3 = split_on_space(phrase)[3],
                token4 = split_on_space(phrase)[4],
                prediction = split_on_space(phrase)[5]),
                by = phrase]
save(Pengram, file="..\\_CleanData\\5gram.RData")  

Tetgram[, `:=` (
  context = paste(split_on_space(phrase)[1],
                  split_on_space(phrase)[2],
                  split_on_space(phrase)[3]),
  token1 = split_on_space(phrase)[1],
  token2 = split_on_space(phrase)[2],
  token3 = split_on_space(phrase)[3],
  prediction = split_on_space(phrase)[4]),
  by = phrase]
save(Tetgram, file="..\\_CleanData\\4gram.RData")  

Trigram[, `:=` (
  context = paste(split_on_space(phrase)[1],
                  split_on_space(phrase)[2]),
  token1 = split_on_space(phrase)[1],
  token2 = split_on_space(phrase)[2],
  prediction = split_on_space(phrase)[3]),
  by = phrase]
save(Trigram, file="..\\_CleanData\\3gram.RData")  

Bigram[, `:=` (
  context = split_on_space(phrase)[1],
  token1 = split_on_space(phrase)[1],
  prediction = split_on_space(phrase)[2]),
  by = phrase]
save(Bigram, file="..\\_CleanData\\2gram.RData")  

Unigram[, token1 := phrase]
Unigram[, id := 1:nrow(Unigram)]
setcolorder(Unigram, c("id", "token1", "r", "rstar", "pstar", "phrase"))
save(Unigram, file="..\\_CleanData\\1gram.RData")  

# inserting rcont and ps
load(file="..\\_CleanData\\1gram.RData")
load(file="..\\_CleanData\\2gram.RData")
aux <- Unigram[, r, phrase]; 
setnames(aux,c("phrase","r"),c("context","rcont"))
Bigram <- merge(Bigram, aux, by = "context", all.x = TRUE)
Bigram[, ps := rstar / rcont]
setcolorder(Bigram, c("token1", "prediction", "r", "rstar", "pstar", "rcont", "ps", "phrase", "context"))
save(Bigram, file="..\\_CleanData\\2gram.RData")

load(file="..\\_CleanData\\3gram.RData")
aux <- Bigram[, r, phrase]; 
setnames(aux,c("phrase","r"),c("context","rcont"))
Trigram <- merge(Trigram, aux, by = "context", all.x = TRUE)
Trigram[, ps := rstar / rcont]
setcolorder(Trigram, c("token1", "token2", "prediction", "r", "rstar", "pstar", "rcont", "ps", "phrase", "context"))
save(Trigram, file="..\\_CleanData\\3gram.RData")

load(file="..\\_CleanData\\4gram.RData")
aux <- Trigram[, r, phrase]; 
setnames(aux,c("phrase","r"),c("context","rcont"))
Tetgram <- merge(Tetgram, aux, by = "context", all.x = TRUE)
Tetgram[, ps := rstar / rcont]
setcolorder(Tetgram, c("token1", "token2", "token3", "prediction", "r", "rstar", "pstar", "rcont", "ps", "phrase", "context"))
save(Tetgram, file="..\\_CleanData\\4gram.RData")

load(file="..\\_CleanData\\5gram.RData")
aux <- Tetgram[, r, phrase]; 
setnames(aux,c("phrase","r"),c("context","rcont"))
Pengram <- merge(Pengram, aux, by = "context", all.x = TRUE)
Pengram[, ps := rstar / rcont]
setcolorder(Pengram, c("token1", "token2", "token3", "token4", "prediction", "r", "rstar", "pstar", "rcont", "ps", "phrase", "context"))
save(Pengram, file="..\\_CleanData\\5gram.RData")

rm(Unigram,Bigram,Trigram,Tetgram,Pengram,rUnigram,rBigram,rTrigram,rTetgram,rPengram,aux)

# including Unigram index

load(file="..\\_CleanData\\1gram.RData")
load(file="..\\_CleanData\\2gram.RData")
load(file="..\\_CleanData\\3gram.RData")
load(file="..\\_CleanData\\4gram.RData")
load(file="..\\_CleanData\\5gram.RData")

setkey(Unigram, id)

aux <- Unigram[, id, token1]; 

# Bigram
Bigram <- merge(Bigram, aux, by = "token1", all.x = TRUE)
setnames(Bigram,colnames(Bigram),c(colnames(Bigram)[1:(length(colnames(Bigram))-1)],"tk1"))
setnames(aux,c("token1","id"),c("prediction","id"))
Bigram <- merge(Bigram, aux, by = "prediction", all.x = TRUE)
setnames(Bigram,colnames(Bigram),c(colnames(Bigram)[1:(length(colnames(Bigram))-1)],"pred"))
save(Bigram, file="..\\_CleanData\\2gram.RData")

# Trigram
setnames(aux,c("prediction","id"),c("token1","id"))
Trigram <- merge(Trigram, aux, by = "token1", all.x = TRUE)
setnames(Trigram,colnames(Trigram),c(colnames(Trigram)[1:(length(colnames(Trigram))-1)],"tk1"))
setnames(aux,c("token1","id"),c("token2","id"))
Trigram <- merge(Trigram, aux, by = "token2", all.x = TRUE)
setnames(Trigram,colnames(Trigram),c(colnames(Trigram)[1:(length(colnames(Trigram))-1)],"tk2"))
setnames(aux,c("token2","id"),c("prediction","id"))
Trigram <- merge(Trigram, aux, by = "prediction", all.x = TRUE)
setnames(Trigram,colnames(Trigram),c(colnames(Trigram)[1:(length(colnames(Trigram))-1)],"pred"))
save(Trigram, file="..\\_CleanData\\3gram.RData")

# Tetgram
setnames(aux,c("prediction","id"),c("token1","id"))
Tetgram <- merge(Tetgram, aux, by = "token1", all.x = TRUE)
setnames(Tetgram,colnames(Tetgram),c(colnames(Tetgram)[1:(length(colnames(Tetgram))-1)],"tk1"))
setnames(aux,c("token1","id"),c("token2","id"))
Tetgram <- merge(Tetgram, aux, by = "token2", all.x = TRUE)
setnames(Tetgram,colnames(Tetgram),c(colnames(Tetgram)[1:(length(colnames(Tetgram))-1)],"tk2"))
setnames(aux,c("token2","id"),c("token3","id"))
Tetgram <- merge(Tetgram, aux, by = "token3", all.x = TRUE)
setnames(Tetgram,colnames(Tetgram),c(colnames(Tetgram)[1:(length(colnames(Tetgram))-1)],"tk3"))
setnames(aux,c("token3","id"),c("prediction","id"))
Tetgram <- merge(Tetgram, aux, by = "prediction", all.x = TRUE)
setnames(Tetgram,colnames(Tetgram),c(colnames(Tetgram)[1:(length(colnames(Tetgram))-1)],"pred"))
save(Tetgram, file="..\\_CleanData\\4gram.RData")

# Pengram
setnames(aux,c("prediction","id"),c("token1","id"))
Pengram <- merge(Pengram, aux, by = "token1", all.x = TRUE)
setnames(Pengram,colnames(Pengram),c(colnames(Pengram)[1:(length(colnames(Pengram))-1)],"tk1"))
setnames(aux,c("token1","id"),c("token2","id"))
Pengram <- merge(Pengram, aux, by = "token2", all.x = TRUE)
setnames(Pengram,colnames(Pengram),c(colnames(Pengram)[1:(length(colnames(Pengram))-1)],"tk2"))
setnames(aux,c("token2","id"),c("token3","id"))
Pengram <- merge(Pengram, aux, by = "token3", all.x = TRUE)
setnames(Pengram,colnames(Pengram),c(colnames(Pengram)[1:(length(colnames(Pengram))-1)],"tk3"))
setnames(aux,c("token3","id"),c("token4","id"))
Pengram <- merge(Pengram, aux, by = "token4", all.x = TRUE)
setnames(Pengram,colnames(Pengram),c(colnames(Pengram)[1:(length(colnames(Pengram))-1)],"tk4"))
setnames(aux,c("token4","id"),c("prediction","id"))
Pengram <- merge(Pengram, aux, by = "prediction", all.x = TRUE)
setnames(Pengram,colnames(Pengram),c(colnames(Pengram)[1:(length(colnames(Pengram))-1)],"pred"))
save(Pengram, file="..\\_CleanData\\5gram.RData")

# end of step 2
