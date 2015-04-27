tidytext <- function(text){
  #load the required packages,rm,RWeka y data.table.
  #suppressPackageStartupMessages(library("tm"))
  #suppressPackageStartupMessages(library("stringi"))
  #suppressPackageStartupMessages(library("RWeka"))

  mydata.corpus<-Corpus(VectorSource(text))
  mydata.corpus<-tm_map(mydata.corpus,content_transformer(function(x) iconv(x, to='ASCII', sub='')))
  # Change text to lowercase
  mydata.corpus<-tm_map(mydata.corpus,content_transformer(tolower))
  # Eliminate numbers
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(removeNumbers))
  # Remove punctuation marks
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(removePunctuation))
  
  #spaces are removed extra blank
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(stripWhitespace))
  mydata.corpus <- tm_map(mydata.corpus, PlainTextDocument)
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(function(x) stri_trans_tolower(x)))
  mydata.corpus <- tm_map(mydata.corpus, content_transformer(function(x) stri_trans_general(x, "en_US")))
  
  text <- unlist(mydata.corpus[[1]]$content)
  return(text)
}
