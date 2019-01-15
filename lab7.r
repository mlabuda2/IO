#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
#          "cluster", "igraph", "fpc")
#install.packages(Needed, dependencies = TRUE)
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

cname <- file.path("~","IO","articles")   
cname  
dir(cname)
#install.packages("tm") 
#wymaga libxml2-dev
library(tm)
docs <- VCorpus(DirSource(cname))   
summary(docs) 
inspect(docs[1])
docs <- tm_map(docs,removePunctuation)   

for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
}

docs <- tm_map(docs, removeNumbers)   

docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)

stopwords("english")
docs[["character(0)"]][["content"]]

#removing words 
docs <- tm_map(docs, removeWords, c("s", "l", "h", "th", "-", "—", "–"))  
docs <- tm_map(docs, PlainTextDocument)

for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, stripWhitespace)

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)
freq <- colSums(as.matrix(dtm))
ord <- order(freq)   
m <- as.matrix(dtm)   
write.csv(m, file="DocumentTermMatrix.csv")   
dtms <- removeSparseTerms(dtm, 0.2)

freq <- colSums(as.matrix(dtm))
head(table(freq), 20)
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

findFreqTerms(dtm, lowfreq=50)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 

library(ggplot2) 
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

findAssocs(dtm, c("country" , "american"), corlimit=0.85)
findAssocs(dtms, "think", corlimit=0.70)

# install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)
wordcloud(names(freq), freq, max.words=100) 
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  

dtmss <- removeSparseTerms(dtm, 0.15)
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete")   
plot(fit, hang=-1)  

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)    
rect.hclust(fit, k=6, border="red")

library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

library(slam)
cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))

