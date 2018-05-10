# Install
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("tm", "SnowballC", "wordcloud", "RColorBrewer")
check.packages(packages)
rm(check.packages,packages)
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Read the text file
for(i in 1:4){
    filePath <- paste0("rawdata/budgetspeech",2013+i,".txt")
    text <- readLines(filePath)
    # Load the data as a corpus
    docs <- Corpus(VectorSource(text))
    # inspect(docs)
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("will", "crore", "propos", "year", "lakh", "also", "india", "propose","now","crores",
                                        "lakhs","years","provide","provided","new","tax","government","percent","sum","set",
                                        "sector","purpose","madam","speaker","budget","made","areas","give","well","will","crore",
                                        "proposed","per","like","input","address","two","taken","take","income","last",
                                        "various","focus","can","use","get","due","create","must","used","called","start",
                                        "intend","within","allow","view","start","back","towards","several",
                                        "may","certain","make","without","enable","including","one","need","encourage",
                                        "therefore","cases","case","next","number","act","reduce","")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    # head(d, 10)
    
    # Creating the wordcloud below:
    # 0. Choose a file name to save to
    flname = paste0("output/IndianBudget_common_words_",2013+i,".jpg")
    # 1. Open jpeg file
    jpeg(flname, width = 512, height = 512)
    # 2. Create the plot
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=50, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
    # 3. Close the file
    dev.off()
    
    rm(d,m,docs,dtm,filePath,text,v,toSpace,flname)
}
rm(i)