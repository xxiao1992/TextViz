require('wordcloud')
require('biclust')
require('cluster')
require('igraph') 
require('dplyr')
require('scales')
require('SnowballC')
require('RColorBrewer')
require('ggplot2')
require('tm')
require('Rgraphviz')
require('fpc')

setwd("/Users/vishaljuneja/Desktop/EDAV/Project5/textViz/")

mypwd = "/Users/vishaljuneja/Desktop/EDAV/Project5/textViz/data"
docs = Corpus(DirSource(mypwd))

#reading the file and creating a temporary corpus
text <- readLines("State of the Union Addresses 1970-2016.txt")
docs <- Corpus(VectorSource(paste(text,collapse="\n")))


# Creating the corpus of the documents
docs <- Corpus(VectorSource(
  strsplit(as.character(docs[[1]]),  ## coerce to character
           "***",   
           fixed=TRUE)[[1]]))  


#Adding author and date as meta information for each document
docs_backs <- docs

meta_data <- strsplit(docs[[1]]$content,"\n ",fixed = TRUE)[[1]]
meta_data <- list[grepl("[Union].*",meta_data,perl=TRUE)]
meta_data <- sub("\n","",meta_data,perl = TRUE)
meta_data <- trimws(meta_data)
meta_data <- sub(", State of the Union Address, ","--",meta_data,perl=TRUE)
meta_data <- strsplit(meta_data,"--",fixed=TRUE)
docs[[1]] <- NULL
meta_data[[1]] <- NULL

for(j in seq(docs))   
{   
  meta(docs[[j]],"author") <- meta_data[[j]][1]
  meta(docs[[j]],"Date") <-  meta_data[[j]][2]
  meta(docs[[j]],"description") <- "State of the Union Address"
}
docs[[1]]$meta

# Processing documents - Cleaning
docs = tm_map(docs, function(x){
  x$content <- trimws(x$content)
  x
})
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stemDocument)
docs = tm_map(docs, stripWhitespace)
#docs = tm_map(docs, PlainTextDocument)


docs[[1]]$meta

# Building term matrix 

doc_target = docs[180:224]

doc_target[[1]]$content

#names(doc_target) = paste("year", 1970:2016, sep = " ") 

dtm = DocumentTermMatrix(doc_target)
dtm

inspect(dtm[1:5,1:20])

write.csv(as.matrix(dtm),file='dtm.csv')

mfreq = colSums(as.matrix(dtm))

p1 = ggplot(data.frame(word=names(mfreq),freq=mfreq),aes(word,freq))+ 
  geom_bar(stat='identity') + 
  theme(axis.text.x=element_text(angle=45,hjust=0.5))
p1

p1 = ggplot(subset(data.frame(word=names(mfreq),freq=mfreq),freq>800),aes(word,freq))+ 
  geom_bar(stat='identity') + 
  theme(axis.text.x=element_text(size=12,color='red',fac='bold.italic',angle=45,hjust=0.5))
p1

mord = order(mfreq, decreasing=TRUE) #increasing order as default
mfreq[head(mord,20)]

#wordcloud
wordcloud(names(mfreq),mfreq, min.freq=70)
