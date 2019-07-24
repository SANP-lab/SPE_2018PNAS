#for text analysis SPE
#xinyuanyan
#20170710
#SANP


library(tm) # Framework for text mining.
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(ggraph)
library(igraph)
library(scales) # Include commas in numbers.
library(Rgraphviz) # Correlation plots.
library(SnowballC)
#read text
setwd("G:/Yan.X Projects/1_Social_Placebo_Effect/3_EXP/description_data")
#ted <- readLines("owen_descrip_CLEAN0903.txt",encoding = "ANSI")
#ted <- readLines("owen_descrip_NOCLEAN0903.txt",encoding = "ANSI")
ted <- readLines("owen_descrip_CLEAN0903_increase_enhance.txt",encoding = "ANSI")

doc.vec<-VectorSource(ted)
doc.corpus<-Corpus(doc.vec)
#clean the text
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))#lower case
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

doc.corpus <- tm_map(doc.corpus, toSpace, "-")
doc.corpus <- tm_map(doc.corpus, toSpace, ":")
doc.corpus <- tm_map(doc.corpus, toSpace, "'")

doc.corpus <- tm_map(doc.corpus, toSpace, " -")
#  ------------------------------------------------------------------------
td.mat <- as.matrix(TermDocumentMatrix(doc.corpus))
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix
#  ------------------------------------------------------------------------

TDM<-DocumentTermMatrix(doc.corpus, control=list(wordLengths=c(5, 20)))

freq <- colSums(as.matrix(TDM))
length(freq)

ord <- order(freq,decreasing = T)
freq[head(ord,10)]

dtm <- TDM
# Recreate edges, using code from plot.DocumentTermMatrix
m <- dtm
corThreshold <- 0.05
m <- as.matrix(m[, freq.terms])
c <- cor(m)
c[c < corThreshold] <- 0
c[is.na(c)] <- 0
diag(c) <- 0
ig <- graph.adjacency(c, mode="undirected", weighted=TRUE)
g1 <- as_graphnel(ig)

# Make edge labels
ew <- as.character(unlist(edgeWeights(g1)))
ew <- ew[setdiff(seq(along=ew), Rgraphviz::removedEdges(g1))]
names(ew) <- edgeNames(g1)
eAttrs <- list()
elabs <- paste("        ", round(as.numeric(ew), 2)) # so it doesn't print on top of the edge
names(elabs) <- names(ew)
eAttrs$label <- elabs
fontsizes <- rep(7, length(elabs))
names(fontsizes) <- names(ew)
eAttrs$fontsize <- fontsizes

plot(dtm, term=freq.terms, corThreshold=0.3, weighting=T, 
     edgeAttrs=eAttrs)

findAssocs(dtm, "oxytocin", 0.2)


plot(TDM,
     terms=findFreqTerms(TDM, lowfreq=34),
     corThreshold=0.1)




