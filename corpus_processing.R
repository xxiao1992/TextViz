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

setwd("C:/Users/oweni/OneDrive/Documents/GitHub/textViz")

mypwd = "C:/Users/oweni/OneDrive/Documents/GitHub/textViz/data"
docs = Corpus(DirSource(mypwd))

class(docs)
class(docs[[1]])
inspect(docs[1])

# split the documents into individual lists
docs = Corpus(VectorSource(strsplit(paste(docs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))

docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stemDocument)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, PlainTextDocument)

doc_target = docs[180:225]

names(doc_target) = paste("year", 1970:2016, sep = " ") 

dtm = DocumentTermMatrix(doc_target)
dtm

inspect(dtm[1:5,1:20])

write.csv(as.matrix(dtm),file='dtm.csv')

### if read from a local file

# library(qdap)
# x = read.csv('dtm.csv', header = T)
# row.names(x) = x[,1]
# x = x[,-1]
# dtm = as.dtm(as.wfm(x))

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

wc_words = mfreq[order(mfreq, decreasing=TRUE)][1:1000]


set.seed(-1)
wordcloud(names(wc_words),wc_words,
          random.color=FALSE, 
          # colors chosen randomly or based on the frequency
          random.order=FALSE) # plot in an dreasing frequency

inspect(removeSparseTerms(dtm, 0.6))

dtmc = removeSparseTerms(dtm,sparse=0.6) 


mfreq = colSums(as.matrix(dtmc))
set.seed(429)
wordcloud(names(mfreq),mfreq,
          min.freq=5, # plot words apprear 10+ times
          scale=c(4,0.5), # make it bigger with argument "scale"
          colors=brewer.pal(8, "Dark2"), # use color palettes
          random.color=FALSE, 
          # colors chosen randomly or based on the frequency
          random.order=FALSE) # plot in an dreasing frequency