library(stringr)
library(plyr)
library(dplyr)
library(magrittr)
library(mgcv)
library(tm)
#library(proxy)
library(ggdendro)
library(ggplot2)

sous <- readLines(file("State of the Union Addresses 1970-2016.txt"))
pres = sous[grep("^[//*]{3}$",sous)+3]
pres = pres[grep("[[:alpha:]]$",pres)]

strt1 = grep("^[//*]{3}$",sous)+5
strt = strt1[1:214]
end1 = grep("^[//*]{3}$",sous)-2
end = end1[-c(1,216)]
startend = data.frame ("start"=strt1, "end" =end1)

comb=list()
for (i in 1:length(startend[,1])){
  comb[i] <- paste(sous[startend$start[i]:startend$end[i]], sep=" ", collapse=" " )
}


s=list()
for (i in 1:length(comb)){
  test = sapply(comb[[i]], strsplit , split = "\\. |\\? ")
  s[i]= test
}


replace=list()
for (g in 1:length(s)){
  replace[[g]]=unlist(lapply(s[[g]],gsub, pattern = "[[:digit:]]|[\\']|\\([Aa]pplause\\.*\\)", replace = ""))
}

regexpr("\\([Aa]pplause\\.*\\)",s)
regexpr("\\([Aa]pplause\\.*\\)",replace)
#Test Case


punct=list()
for (i in 1:length(lower)){
  punct[[i]] = unlist(lapply(lower[[i]], strsplit , "[[:punct:]]|[[:blank:]]"))
}

full=list()
for (i in 1:length(punct)){
  full[[i]] = punct[[i]][nchar(punct[[i]])>0]
}

library(SnowballC)

steml = list()
for (i in 1:length(full)){
  steml[[i]] = unlist(lapply(full[[i]], wordStem))
}

bagwords = sort(unique(unlist(steml)))
bagwords = bagwords[nchar(bagwords)!=0]

wordv = matrix(seq(1,(length(bagwords)*length(steml)),by=1),
               nrow=length(bagwords), ncol=length(steml),
               dimnames = list(bagwords,1:length(steml)))


for(j in 1:length(steml)){
  for (i in 1:length(bagwords)){
    wordv[i,j] = length(steml[[j]][steml[[j]]==bagwords[i]])
  }
}

computeSJDistance =
  function(tf, df, terms, logdf = TRUE, verbose = TRUE)
  {
    
    numTerms = nrow(tf)
    numDocs = ncol(tf)
    
    tfn =  t(tf)/colSums(tf)
    if (logdf) tfidfs = t(tfn) * (log(numDocs) - log(df))
    else  tfidfs = numDocs * ( t(tfn) / df)
    
    D.SJ = matrix(0, numDocs, numDocs)
    for(i in 1:(numDocs -1)) {
      for(j in (i+1):numDocs) { 
        D.SJ[i,j] = D.SJ[j,i] = DJ(tfidfs[, i], tfidfs[, j])
      }
      if(verbose)
        print(i)  
    }
    return(D.SJ)
  }

DB = function(p, q)
{
  tmp = !(p == 0 | q == 0)
  p = p[tmp]
  q = q[tmp]
  
  sum(- p*log(q) + p*log(p) )
}

DJ = function(p, q)
{
  T = 0.5 * (p + q)  
  0.5 * DB(p, T) + 0.5 * DB(q, T)
}  

simMatrix = computeSJDistance(tf = wordv, 
                              df = dfq, terms = bagOfWords, 
                              logdf = FALSE)
rownames(simMatrix) = paste(pres,year, sep = "-")
colnames(simMatrix) = paste(pres,year, sep = "-")


#Multidimensional Scaling 
mds =cmdscale(simMatrix)
plot(mds, type = "n", xlab = "", ylab = "", main="MDS on Documents")
par(cex=0.5)
text(mds, rownames(simMatrix))

# write-up
'''
From the plot of the MDS, we can see that most speeches from presidents are similar. It makes sense since they use many common words and topics.
Notice that Warren Harding and Chester A. Arthur's speeches are most dissimliar than others' speeches (they are far from the dense cluster on the graph).
Although the graph is dense on that cluster and hard to tell who are in that cluster, we can infer the names of presidents in that cluster by excluding the presidents outside the cluster.
For example, Bush and Obama should be in that cluster and their speeches are more similar compared with presidents such as James Monroe or Grover Cleveland.
'''
