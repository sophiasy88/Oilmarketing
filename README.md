# Oilmarketing, following is the R code to input news text and implement text mining process and get useful information in Oil and gas marketing,

1. The first things we need to do is load useful text mining packages and read the news,calls, analysis files we need into R 
##loading tidy text packages
library(dplyr)
library(tm)
library(qdapTools)
library(ggplot2)
library(qdap)
library(wordcloud)

##read news, callings  file
filename <- file.path("C:/Users/yshi20/Desktop/Finding jobs","APCtrending")   

##check file, make sure the file is inputted correctly. 
dir(filename)
docs <- VCorpus(DirSource(filename))   
mysummary <- summary(docs)

writeLines(as.character(docs[1]))

2. Now, we begin the text mining processing. The first things in text mining is Text Pre-pocessing which including removing stopwords, punctuation, converting to lowercase...

#Text Preprocessing, removing  capitalization, stop words, punctuation, and others.
docs <- tm_map(docs,removePunctuation) 
for (j in seq(docs)) {
    docs[[j]] <- gsub("/", " ", docs[[j]])
    docs[[j]] <- gsub("@", " ", docs[[j]])
    docs[[j]] <- gsub("\\|", " ", docs[[j]])
    docs[[j]] <- gsub("\u2028", " ", docs[[j]])  
}

#Converting to lowercase:
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

#Removing “stopwords” (common words) .
docs <- tm_map(docs, removeWords, stopwords("english"))  
docs <- tm_map(docs, PlainTextDocument)
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("will", "us", "today", "good morning", "thank", "may"))

3. Steming the words, removing the end of words and only keep the root of words.
#despite whether or not it may have a variety of possible endings in the original text.

docs_st <- tm_map(docs, stemDocument)
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) 
#Also remove white space
docs <- tm_map(docs, stripWhitespace)
#Finish propocess
docs <- tm_map(docs, PlainTextDocument)

4. Create term frequency matrix to count the frequency of words, and sort them from most to least order
#create a document term matrix
dtm <- DocumentTermMatrix(docs) 
#Word Frequency check out some of the most and least frequently occurring words
freq <- colSums(as.matrix(dtm)) 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
#Return the first 30 high frequency words to get some useful information in these news.
head(freq, 30)

5. Visurlization, resurn histograms, wordclouds to make the text mining results visurable. 
#Plot Word Frequencies and word cloud
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)
Wordfreqplot <- ggplot(subset(wf, freq>10), aes(x = word, y = freq)) +
          geom_bar(stat = "identity") + 
          theme(axis.text.x=element_text(angle=45, hjust=1))

print(Wordfreqplot + ggtitle("APC analysis word frequency"))
set.seed(142)   
wordcloud(names(freq), freq, min.freq=5, colors=brewer.pal(8, "Dark2"))  

