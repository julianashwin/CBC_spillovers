setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(readtext)
require(stringr)
require(tm)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/clean_text/"
raw_dir <- "data/raw_text/"


# Read the clean Federal Reserve minutes to a file
clean_filename = paste0(clean_dir, "fedminutes_clean.csv")
fedminutes.df <- read.csv(clean_filename, stringsAsFactors = FALSE)


# Read the clean Federal Reserve minutes to a file
clean_filename = paste0(clean_dir, "fedspeeches_all.csv")
fedspeeches.df <- read.csv(clean_filename, stringsAsFactors = FALSE)








for (i in 1:nrow(meeting.df)){
  # Meeting detail
  meeting_id <- meeting.df$meeting_id[i]
  meet_date <- meeting.df$meet_date[i]
  pub_date <- meeting.df$pub_date[i]
  print(paste("Finding relevant articles for meeting", meeting.df$meeting_id[i]))
  
  pre_meet = meet_date - 6
  post_meet = meet_date + 6
  
  pre_pub = pub_date - 6
  post_pub = pub_date + 6
  
  
  ### Around the meeting
  # Identify those articles in the week leading up to the meeting
  premeet_articles <- nyt.df[which(nyt.df$date_num < meet_date & 
                                     nyt.df$date_num >= pre_meet), ]
  premeet_articles$subsequent_meeting <- meeting_id
  # Identify those articles in the week following the publication of the minutes
  postmeet_articles <- nyt.df[which(nyt.df$date_num > meet_date & 
                                      nyt.df$date_num <= post_meet), ]
  postmeet_articles$recent_meeting <- meeting_id
  
  
  ### Around the publication
  # Identify those articles in the week leading up to the meeting
  prepub_articles <- nyt.df[which(nyt.df$date_num < pub_date & 
                                    nyt.df$date_num >= pre_pub), ]
  prepub_articles$subsequent_pub <- meeting_id
  
  # Identify those articles in the week following the publication of the minutes
  postpub_articles <- nyt.df[which(nyt.df$date_num >= pub_date & 
                                     nyt.df$date_num <= post_pub), ]
  postpub_articles$recent_pub <- meeting_id
  
  
  nyt_premeeting <- rbind(nyt_premeeting, premeet_articles)
  nyt_postmeeting <- rbind(nyt_postmeeting, postmeet_articles)
  
  nyt_prepub <- rbind(nyt_prepub, prepub_articles)
  nyt_postpub <- rbind(nyt_postpub, postpub_articles)
}


# Merge the pre and post, recognising that some may appear in both
nyt_relevant <- merge(nyt_postmeeting, nyt_premeeting, 
                      by = c("unique_id", "date_num", "quarter", "headline", "main_text", "nchar"), 
                      all.x = TRUE, all.y = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_postpub, 
                      by = c("unique_id", "date_num", "quarter", "headline", "main_text", "nchar"), 
                      all.x = TRUE, all.y = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_prepub, 
                      by = c("unique_id", "date_num", "quarter", "headline", "main_text", "nchar"), 
                      all.x = TRUE, all.y = TRUE)




############################# Some basic cleaning ############################# 


### For the Fed minutes corpus
# The removePunctuation function seems to be unreliable, so use gsub to remove some problems first
fedminutes.df$paragraph <- gsub("-", " ", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("/'", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("’", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("‘", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("“", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("”", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("€", " ", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub(",", " ", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub(":", " ", fedminutes.df$paragraph)

fedminutes.corpus <- Corpus(VectorSource(unlist(fedminutes.df[, "paragraph"])))
# Preliminary cleaning
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, stripWhitespace)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, removeNumbers)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, removePunctuation)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, content_transformer(tolower))
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, stopwords("english"))
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <-  tm_map(fedminutes.corpus, stemDocument)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, stripWhitespace)
inspect(fedminutes.corpus[[250]])

# Remove months and seasons as they might introduce artificial co-movement
months <- c("januari", "februari", "march", "april", "may", "june", "july", "juli", "julyaugust", "august",
            "septemb", "octob", "novemb", "decemb")
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, months)
seasons <- c("summer", "autumn", "spring", "winter")
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, seasons)

fedminutes.df$paragraph_clean <- sapply(fedminutes.corpus, as.character)
fedminutes.df$paragraph_clean <- str_trim(fedminutes.df$paragraph_clean)
fedminutes.df[250, c("paragraph", "paragraph_clean")]


# Then convert the corpus to a DTM in order to extract the complete vocab
fedminutes.dtm <- DocumentTermMatrix(fedminutes.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of fedminutes.dtm are", dim(fedminutes.dtm)[1], "documents and", 
            dim(fedminutes.dtm)[2], "words in vocab"))
fedminutes.vocab <- fedminutes.dtm$dimnames$Terms

# Total wordcount
fedminutes.df$wordcount <- rowSums(as.matrix(fedminutes.dtm))
summary(fedminutes.df$wordcount)
sum(fedminutes.df$wordcount)
length(unique(fedminutes.df$meeting_id))

clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
write.csv(fedminutes.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)







### For the NYT articles corpus
nyt_relevant$main_text <- gsub("-", " ", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub("/'", "", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub("’", "", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub("‘", "", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub("“", "", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub("”", "", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub("€", " ", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub(",", " ", nyt_relevant$main_text)
nyt_relevant$main_text <- gsub(":", " ", nyt_relevant$main_text)

nyt.corpus <- Corpus(VectorSource(unlist(nyt_relevant[, "main_text"])))
# Preliminary cleaning
inspect(nyt.corpus[[250]])
nyt.corpus <- tm_map(nyt.corpus, stripWhitespace)
inspect(nyt.corpus[[250]])
nyt.corpus <- tm_map(nyt.corpus, removeNumbers)
inspect(nyt.corpus[[250]])
nyt.corpus <- tm_map(nyt.corpus, removePunctuation)
inspect(nyt.corpus[[250]])
nyt.corpus <- tm_map(nyt.corpus, content_transformer(tolower))
inspect(nyt.corpus[[250]])
nyt.corpus <- tm_map(nyt.corpus, removeWords, stopwords("english"))
inspect(nyt.corpus[[250]])
nyt.corpus <-  tm_map(nyt.corpus, stemDocument)
inspect(nyt.corpus[[250]])
nyt.corpus <- tm_map(nyt.corpus, stripWhitespace)
inspect(nyt.corpus[[250]])

# Remove months and seasons as they might introduce artificial co-movement
months <- c("januari", "februari", "march", "april", "may", "june", "july", "juli", "julyaugust", "august",
            "septemb", "octob", "novemb", "decemb")
nyt.corpus <- tm_map(nyt.corpus, removeWords, months)
seasons <- c("summer", "autumn", "spring", "winter")
nyt.corpus <- tm_map(nyt.corpus, removeWords, seasons)
website_words <- c("http", "www", "nytimes", "com", "wwwnytimescom", "wwwnytimes", "nytimescom", "url")


# Then convert the corpus to a DTM in order to extract the complete vocab
nyt.dtm <- DocumentTermMatrix(nyt.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of nyt.dtm are", dim(nyt.dtm)[1], "documents and", 
            dim(nyt.dtm)[2], "words in vocab"))

### Remove the words which are not common to the nyt and fed texts from the articles
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
nyt.vocab <- nyt.dtm$dimnames$Terms

justnyt.vocab <- outersect(nyt.vocab, fedminutes.vocab)



# Need to remove the vocab in stages as it is to large for the gsub function
N <- length(justnyt.vocab)
n <- N/1000
n <- round(n+1,0)
j <- 1
for (i in 1:n){
  print(j)
  if ((j+999) < N){
    nyt.corpus <- tm_map(nyt.corpus, removeWords, justnyt.vocab[j:(j+999)])
    inspect(nyt.corpus[[700]])
    print(j+999)
  } else if (j < N){
    nyt.corpus <- tm_map(nyt.corpus, removeWords, justnyt.vocab[j:N])
    inspect(nyt.corpus[[700]])
    print (N)
  }
  j <- j+1000
}
nyt.corpus <- tm_map(nyt.corpus, stripWhitespace)
inspect(nyt.corpus[[700]])

# Add the clean text to the data frame
nyt_relevant$text_clean <- sapply(nyt.corpus, as.character)
nyt_relevant$text_clean <- str_trim(nyt_relevant$text_clean)
nyt_relevant[700, c("main_text", "text_clean")]

table(as.numeric(nyt_relevant$text_clean == ""))
table(is.na(nyt_relevant$text_clean))

nyt_relevant <- nyt_relevant[,c("unique_id", "date_num", "headline", "recent_meeting",
                                "subsequent_meeting",  "main_text", "text_clean"  )]



# Then convert the corpus to a DTM in order to the final word counts
nyt.dtm <- DocumentTermMatrix(nyt.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of nyt.dtm are", dim(nyt.dtm)[1], "documents and", 
            dim(nyt.dtm)[2], "words in vocab"))


### Calculate the tf and df score for each term
term_freq <- col_sums(nyt.dtm) # Total number of times a term appears
doc_freq <- col_sums(nyt.dtm > 0) # Total number of documents it appears i
cor.test(term_freq, doc_freq)

# Which terms appear in three or fewer documents
rare_terms <- nyt.dtm[,doc_freq <= 3]$dimnames$Terms
print(rare_terms)

# Total wordcount
nyt_relevant$wordcount <- rowSums(as.matrix(nyt.dtm))
summary(nyt_relevant$wordcount)
sum(nyt_relevant$wordcount)
length(unique(nyt_relevant$unique_id))


clean_filename = paste(clean_dir, "CBC/NYT_relevant_clean.csv", sep = "/")
write.csv(nyt_relevant, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# nyt_relevant <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)


############################# End ############################# 