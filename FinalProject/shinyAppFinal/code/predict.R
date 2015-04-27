
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
  if(is.null(vindices)) return(NULL)
  
  ipred <- NULL
  n <- length(vindices)
  
  if(n>4) {vindices <- vindices[(4-n):-1]; n <- 4 }
  
  if(n == 4) {
    ipred <- Pengram[tk1 == vindices[1] & tk2 == vindices[2] &
                       tk3 == vindices[3] & tk4 == vindices[4], 
                      pred,ps][order(-ps)]
    if(sum(ipred[,pred],0) == 0) {n<-3; vindices <- vindices[-1]}
  }
  
  if(n == 3) {
    ipred <- Tetgram[tk1 == vindices[1] & tk2 == vindices[2] &
                       tk3 == vindices[3],  
                      pred,ps][order(-ps)]
    if(sum(ipred[,pred],0) == 0) {n<-2; vindices <- vindices[-1]}
  }
  
  if(n == 2) {
    ipred <- Trigram[tk1 == vindices[1] & tk2 == vindices[2],
                      pred,ps][order(-ps)]
    if(sum(ipred[,pred],0) == 0) {n<-1; vindices <- vindices[-1]}
  }
  
  if(n == 1) ipred <- Bigram[tk1 == vindices[1],  pred,ps][order(-ps)]
  
  if(sum(ipred[,pred],0) == 0) return("can't predict")
  else head(Unigram[id == ipred[,pred], token1],20)
}

verifyprof <- function(word){
  word <- as.vector(sapply(word, function(x) { 
                    if(sum(profanity$word == x)) return("#@#%#")
                    else return(x)
    }))
  word
}
