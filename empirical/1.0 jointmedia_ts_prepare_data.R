setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(readtext)
require(stringr)
require(tm)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/clean_text/"
raw_dir <- "data/raw_text/"


# Import the clean Federal Reserve minutes
clean_filename = paste0(clean_dir, "fedminutes_all.csv")
fedminutes.df <- read.csv(clean_filename, stringsAsFactors = FALSE)

# Import the clean Federal Reserve minutes
clean_filename = paste0(clean_dir, "fedspeeches_all.csv")
fedspeeches.df <- read.csv(clean_filename, stringsAsFactors = FALSE)

# Import the clean NYT articles
clean_filename = "~/Documents/DPhil/Clean_Data/New_York_Times/econ_news/nyt_articles_matched.csv"
nyt.df <- read.csv(clean_filename, stringsAsFactors = FALSE)



############################# Some basic cleaning ############################# 


# The removePunctuation function seems to be unreliable, so use gsub to remove some problems first
fedminutes.df$paragraph <- gsub("-|/'|/'|’|‘|“|”|€|,|:", " ", fedminutes.df$paragraph)
fedspeeches.df$paragraph <- gsub("-|/'|/'|’|‘|“|”|€|,|:", " ", fedspeeches.df$paragraph)
nyt.df$main_text <- gsub("-|/'|/'|’|‘|“|”|€|,|:", " ", nyt.df$main_text)

# Squish and update nchar
fedminutes.df$paragraph <- str_squish(fedminutes.df$paragraph)
fedminutes.df$nchar <- nchar(fedminutes.df$paragraph)
fedspeeches.df$paragraph <- str_squish(fedspeeches.df$paragraph)
fedspeeches.df$nchar <- nchar(fedspeeches.df$paragraph)
nyt.df$main_text <- str_squish(nyt.df$main_text)
nyt.df$nchar <- nchar(nyt.df$main_text)

fedminutes.corpus <- Corpus(VectorSource(unlist(fedminutes.df[, "paragraph"])))
fedspeeches.corpus <- Corpus(VectorSource(unlist(fedspeeches.df[, "paragraph"])))
nyt.corpus <- Corpus(VectorSource(unlist(nyt.df[, "main_text"])))

### Preliminary cleaning
# stripWhitespace
fedminutes.corpus <- tm_map(fedminutes.corpus, stripWhitespace)
fedspeeches.corpus <- tm_map(fedspeeches.corpus, stripWhitespace)
nyt.corpus <- tm_map(nyt.corpus, stripWhitespace)
# Remove numbers
fedminutes.corpus <- tm_map(fedminutes.corpus, removeNumbers)
fedspeeches.corpus <- tm_map(fedspeeches.corpus, removeNumbers)
nyt.corpus <- tm_map(nyt.corpus, removeNumbers)
# Remove punctuation
fedminutes.corpus <- tm_map(fedminutes.corpus, removePunctuation)
fedspeeches.corpus <- tm_map(fedspeeches.corpus, removePunctuation)
nyt.corpus <- tm_map(nyt.corpus, removePunctuation)
# Lower case
fedminutes.corpus <- tm_map(fedminutes.corpus, content_transformer(tolower))
fedspeeches.corpus <- tm_map(fedspeeches.corpus, content_transformer(tolower))
nyt.corpus <- tm_map(nyt.corpus, content_transformer(tolower))
# Remove stopwords
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, stopwords("english"))
fedspeeches.corpus <- tm_map(fedspeeches.corpus, removeWords, stopwords("english"))
nyt.corpus <- tm_map(nyt.corpus, removeWords, stopwords("english"))
# Stemming
fedminutes.corpus <-  tm_map(fedminutes.corpus, stemDocument)
fedspeeches.corpus <-  tm_map(fedspeeches.corpus, stemDocument)
nyt.corpus <-  tm_map(nyt.corpus, stemDocument)
# stripWhitespace again
fedminutes.corpus <- tm_map(fedminutes.corpus, stripWhitespace)
fedspeeches.corpus <- tm_map(fedspeeches.corpus, stripWhitespace)
nyt.corpus <- tm_map(nyt.corpus, stripWhitespace)


# Remove months and seasons as they might introduce artificial co-movement
months <- c("januari", "februari", "march", "april", "may", "june", "july", "juli", "julyaugust", "august",
            "septemb", "octob", "novemb", "decemb")
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, months)
fedspeeches.corpus <- tm_map(fedspeeches.corpus, removeWords, months)
nyt.corpus <- tm_map(nyt.corpus, removeWords, months)

seasons <- c("summer", "autumn", "spring", "winter")
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, seasons)
fedspeeches.corpus <- tm_map(fedspeeches.corpus, removeWords, seasons)
nyt.corpus <- tm_map(nyt.corpus, removeWords, seasons)

# Add cleaned text as new column
fedminutes.df$paragraph_clean <- sapply(fedminutes.corpus, as.character)
fedminutes.df$paragraph_clean <- str_squish(fedminutes.df$paragraph_clean)
fedminutes.df[250, c("paragraph", "paragraph_clean")]
fedspeeches.df$paragraph_clean <- sapply(fedspeeches.corpus, as.character)
fedspeeches.df$paragraph_clean <- str_squish(fedspeeches.df$paragraph_clean)
fedspeeches.df[250, c("paragraph", "paragraph_clean")]
nyt.df$paragraph_clean <- sapply(nyt.corpus, as.character)
nyt.df$paragraph_clean <- str_squish(nyt.df$paragraph_clean)
nyt.df[250, c("main_text", "paragraph_clean")]


# Remove any remaining empty paragraphs
fedminutes.df$nchar_clean <- nchar(fedminutes.df$paragraph_clean)
fedspeeches.df$nchar_clean <- nchar(fedspeeches.df$paragraph_clean)
nyt.df$nchar_clean <- nchar(nyt.df$paragraph_clean)

fedminutes_clean <- fedminutes.df[which(fedminutes.df$nchar_clean > 0),]
fedspeeches_clean <- fedspeeches.df[which(fedspeeches.df$nchar_clean > 0),]
nyt_clean <- nyt.df[which(nyt.df$nchar_clean > 0),]

# Redo the corpora
fedminutes.corpus <- Corpus(VectorSource(unlist(fedminutes_clean[, "paragraph_clean"])))


# Then convert the corpus to a DTM in order to extract the complete vocab
fedminutes.dtm <- DocumentTermMatrix(fedminutes.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of fedminutes.dtm are", dim(fedminutes.dtm)[1], "documents and", 
            dim(fedminutes.dtm)[2], "words in vocab"))
fedminutes.vocab <- fedminutes.dtm$dimnames$Terms
fedminutes_clean$wordcount <- rowSums(as.matrix(fedminutes.dtm))

# Include only the terms that appear in the minutes
pb = txtProgressBar(min = 1, max = nrow(fedspeeches_clean), initial = 1) 
for (ii in 1:nrow(fedspeeches_clean)){
  para_temp <- fedspeeches_clean$paragraph_clean[ii]
  para_temp <- str_split(para_temp, " ")[[1]]
  para_temp <- para_temp[which(para_temp %in% fedminutes.vocab)]
  fedspeeches_clean$paragraph_clean[ii] <- paste(para_temp, collapse = " ")
  setTxtProgressBar(pb,ii)
}
fedspeeches_clean$wordcount <- str_count(fedspeeches_clean$paragraph_clean, " ") +1
summary(fedspeeches_clean$wordcount)
fedspeeches_clean <- fedspeeches_clean[which(fedspeeches_clean$wordcount >=4),]

pb = txtProgressBar(min = 1, max = nrow(nyt_clean), initial = 1) 
for (ii in 1:nrow(nyt_clean)){
  para_temp <- nyt_clean$paragraph_clean[ii]
  para_temp <- str_split(para_temp, " ")[[1]]
  para_temp <- para_temp[which(para_temp %in% fedminutes.vocab)]
  nyt_clean$paragraph_clean[ii] <- paste(para_temp, collapse = " ")
  setTxtProgressBar(pb,ii)
}
nyt_clean$wordcount <- str_count(nyt_clean$paragraph_clean, " ") +1
summary(nyt_clean$wordcount)
nyt_clean <- nyt_clean[which(nyt_clean$wordcount >=4),]





## Check that the DTMs match the minutes
fedspeeches.corpus <- Corpus(VectorSource(unlist(fedspeeches_clean[, "paragraph_clean"])))
nyt.corpus <- Corpus(VectorSource(unlist(nyt_clean[, "paragraph_clean"])))

fedspeeches.dtm <- DocumentTermMatrix(fedspeeches.corpus, control = 
                                        list(minWordLength = 3))#, dictionary = Terms(fedminutes.dtm)))
print(paste("Dimensions of fedspeeches.dtm are", dim(fedspeeches.dtm)[1], "documents and", 
            dim(fedspeeches.dtm)[2], "words in vocab"))
nyt.dtm <- DocumentTermMatrix(nyt.corpus, control = 
                                list(minWordLength = 3))#, dictionary = Terms(fedminutes.dtm)))
print(paste("Dimensions of nyt.dtm are", dim(nyt.dtm)[1], "documents and", 
            dim(nyt.dtm)[2], "words in vocab"))






### Export the prepped text data

names(fedminutes_clean)
fedminutes_export <- fedminutes_clean[,c("unique_id", "meeting_id", "quarter", "meet_date", 
                                         "pub_date", "paragraph_clean", "wordcount", "sentiment")]

names(fedspeeches_clean)
fedspeeches_export <- fedspeeches_clean[,c("unique_id", "speech_id", "quarter", "date", 
                                         "paragraph_clean", "wordcount", "sentiment")]
names(nyt_clean)
nyt_export <- nyt_clean[,c("unique_id", "quarter", "date", "subsequent_meeting", "recent_meeting", 
                           "subsequent_pub", "recent_pub", "subsequent_speech", "recent_speech",
                           "paragraph_clean", "wordcount", "sentiment")]
write.csv(nyt_export[1:round(nrow(nyt_export)/2),], paste0(clean_dir, "NYT_clean_1.csv"), fileEncoding = "utf-8", row.names = FALSE)
write.csv(nyt_export[round(nrow(nyt_export)/2)+11:nrow(nyt_export),], paste0(clean_dir, "NYT_clean_2.csv"), fileEncoding = "utf-8", row.names = FALSE)
# nyt_relevant <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)


############################# End ############################# 