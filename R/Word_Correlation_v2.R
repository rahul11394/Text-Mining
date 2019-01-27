file_loc <- "C:/Users/RAHCHAU/Desktop/Rahul/IBT/Sentiment Analysis/December 2018/Freightliner/HH-HV/Electrical System/Electrical - Filtered Comments For R.csv"

x <- read.csv(file_loc,header = TRUE)
require(tm)
corp <- Corpus(DataframeSource(x))

corp

docs <- VCorpus(DataframeSource(x))
summary(docs)

summary(corp)

inspect(docs[1])

writeLines(as.character(docs))

#docs <- tm_map(docs,removePunctuation)
#docs <- tm_map(docs,removeNumbers)
#writeLines(as.character(docs))

docs <- tm_map(docs,tolower)
#docs <- tm_maps(docs,PlainTextDocument)
#writeLines(as.character(docs))
#docs <- tm_map(docs,removeWords, stopwords("english"))
#docs <- tm_map(docs,removeWords,c("ben", "troy", "stated","was", "not", "had", "sean","said","giving","give","go","back","let","also","overall","huge", "stub", "and", "the", "he", "has", "to"))
#writeLines(as.character(docs))

docs <- tm_map(docs,stripWhitespace)

writeLines(as.character(docs))

docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)
dtm

tdm <- TermDocumentMatrix(corp)
inspect(tdm)

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
ord

#Word Frequency

#freq <- colSums(as.matrix(dtm))
#head(table(freq),20)

#freq <- colSums(as.matrix(dtm))
#freq

freq <- sort(colSums(as.matrix(dtm)),decreasing = TRUE)
head(freq,20)

wf <- data.frame(word=names(freq), freq=freq)
head(wf)
wf

#Plot Word Frequencies

library(ggplot2)

p <- ggplot(subset(wf,freq>5), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Term Correlations

findAssocs(dtm,"electrical",corlimit = 0.15)
result <- findAssocs(dtm,"electrical",corlimit = 0.15)
result

getwd()

write.csv(result, file = "Electrical - FTL - Words Correlation.csv")

#Word Cloud

library(RColorBrewer)
#install.packages("wordcloud")
library(wordcloud)


set.seed(142)
wordcloud(names(freq),freq,min.freq=10)

wordcloud(names(freq),freq, min.freq = 5, scale = c(5,.1), colors = brewer.pal(6, "Dark2"))

wordcloud(names(freq), freq, min.freq = 5, scale = c(5,.1), colors())



