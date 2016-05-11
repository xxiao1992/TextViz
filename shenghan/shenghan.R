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
#require('Rgraphviz')
require('fpc')

setwd("D:/Google Drive/Courses_G/1B_Exploratory Data Analysis and Visualization/project4")

mypwd = "D:/Google Drive/Courses_G/1B_Exploratory Data Analysis and Visualization/project4/data"
docs = Corpus(DirSource(mypwd))


# split the documents into individual lists
docs = Corpus(VectorSource(strsplit(paste(docs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))

docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stemDocument)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, PlainTextDocument)


subset_docs = docs[180:224]
dtm = DocumentTermMatrix(subset_docs)
subset_docs[[1]]$content


df_dtm <-data.frame(as.matrix(dtm))

df_party <- read.csv("party.csv")
party <- df_party[,2]
row.names(df_dtm) <- df_party[,4]


df_dtm_label <- data.frame(cbind(df_dtm,party))
df_dtm_label$party
library(randomForest)
fit.rf = randomForest(party~., data=df_dtm_label, ntree = 10)

#plot 1
plot(importance(fit.rf))

#################################################################
#library(devtools)
#install_github("ggbiplot", "vqv")

#library(ggbiplot)
df_tdm <- t(df_dtm)
pca_df <- prcomp(t(df_dtm),center = TRUE, scale = TRUE)

plot(pca_df)

require(ggplot2)

###PLOT 2
biplot(pca_df)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = 0.3*cos(theta), y = 0.3*sin(theta))


loadings <- data.frame(pca_df$rotation,.names = df_party[,4])
p <- ggplot(circle,aes(x,y)) + geom_path() +
  geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

p

