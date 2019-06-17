setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(readtext)
require(stringr)
require(tm)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"



############################# Import and clean Fed minutes ############################# 

import_filename <- paste0(raw_dir, "CBC/fedminutes_clean.txt")
fedminutes_all <- readtext(import_filename, encoding = "utf-8")
#fedminutes_all <- read.table(import_filename, sep="\t", header=TRUE, encoding = "utf-8")

# Create dataframe for the paragraphs
variables <- c( "year", "month", "document", "paragraph","seq")
fedminutes.df <- data.frame(matrix(NA,0,length(variables)))
colnames(fedminutes.df) <- variables

para.locs <- as.data.frame(str_locate_all(fedminutes_all, "[0-9]+\t"))
para.num <- length(para.locs[,1])

# Loop over the paragraphs
for (i in 1:(para.num-1)){
  
  # Create empty dataframe to fill
  temp.df <- data.frame(matrix(NA,0,length(variables)))
  colnames(temp.df) <- variables
  
  #print(i)
  # Pull out an individual article
  full.para <- str_sub(fedminutes_all, para.locs[i,1], (para.locs[(i+1),1]-1))
  
  temp <- as.data.frame(str_locate(full.para, "\t"))
  
  docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
  text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
  
  #temp <- as.data.frame(str_locate_all(text, "\t"))
  #assert_that(nrow(temp) == 1)
  
  #seq <- str_trim(str_sub(text, (temp[,"end"]+1)))
  #text <- str_trim(str_sub(text, 1, (temp[,"end"]-1)))
  
  temp.df[1, "document"] <- as.character(docid)
  temp.df[1, "paragraph"] <- text
  
  # Append to dataframe for that file
  fedminutes.df <- rbind(fedminutes.df, temp.df)
}
temp.df <- data.frame(matrix(NA,0,length(variables)))
colnames(temp.df) <- variables
full.para <- str_sub(fedminutes_all, para.locs[para.num,1])
temp <- as.data.frame(str_locate(full.para, "\t"))
docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
temp.df[1, "document"] <- as.character(docid)
temp.df[1, "paragraph"] <- text
fedminutes.df <- rbind(fedminutes.df, temp.df)


# Edit some paragraph names as meeting sometimes starts in a different month to when it finishes
fedminutes.df[which(fedminutes.df$document == "199807"), "document"] <- "199806"
fedminutes.df[which(fedminutes.df$document == "201208"), "document"] <- "201207"
fedminutes.df[which(fedminutes.df$document == "201305"), "document"] <- "201304"

# Extract date information
year <- as.array(str_sub(fedminutes.df$document, 1, 4))
month <- as.array(as.numeric(str_sub(fedminutes.df$document, 5,6)))

# Functions to clean the year and date terms from the fedminutes file
clean_month <- function(month_in){
  month_in <- as.numeric(month_in)
  if(month_in == 1){
    month_out <- "January"
  } else if(month_in == 2){
    month_out <- "February"
  } else if(month_in == 3){
    month_out <- "March"
  } else if(month_in == 4){
    month_out <- "April"
  } else if(month_in == 5){
    month_out <- "May"
  } else if(month_in == 6){
    month_out <- "June"
  } else if(month_in == 7){
    month_out <- "July"
  } else if(month_in == 8){
    month_out <- "August"
  } else if(month_in == 9){
    month_out <- "September"
  } else if(month_in == 10){
    month_out <- "October"
  } else if(month_in == 11){
    month_out <- "November"
  } else if(month_in == 12){
    month_out <- "December"
  }
  return(month_out)
}

month <- apply(month, 1, clean_month)

# Fill in the date values of dataframe
fedminutes.df$year <- as.numeric(year)
fedminutes.df$month <- month


# Import the meeting date data
meeting_dates <- read.csv(paste0(raw_dir, "CBC/Fed_meeting_dates.csv"),
                          stringsAsFactors = FALSE, encoding = "utf-8")

fedminutes.df <- merge(fedminutes.df, meeting_dates, by = c("year", "month"), all.x = TRUE)

fedminutes.df <- fedminutes.df[which(fedminutes.df$paragraph != "?"),]
#View(fedminutes.df[which(is.na(fedminutes.df$meet_date)),])

# Order by meeting date
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date, format = "%d/%m/%Y")
fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date, format = "%d/%m/%Y")
fedminutes.df <- fedminutes.df[order(fedminutes.df$meet_date),]

fedminutes.df <- fedminutes.df[which(fedminutes.df$meet_date >= "1993-01-01"),]
table(is.na(fedminutes.df$pub_date))
table(is.na(fedminutes.df$meet_date))


#View(fedminutes.df[which((str_detect(fedminutes.df$paragraph, "Votes for"))),])
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Votes against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Votes for"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Voting against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Voting for"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Absent and"))), "paragraph"] <- NA

# Remove all the deleted paragraphs from the dataframe
fedminutes.df <- fedminutes.df[which(!is.na(fedminutes.df$paragraph)),]


plot(table(fedminutes.df$meet_date))
summary(nchar(fedminutes.df$paragraph))
fedminutes.df$nchar <- nchar(fedminutes.df$paragraph)

# Add unique paragraph identifier
unique_id <- paste0("FEDp_", 1:nrow(fedminutes.df))
fedminutes.df$unique_id <- unique_id


# Add a unique meeting identifier
meeting.df <- unique(fedminutes.df[, c("year", "month", "pub_date", "meet_date")])
meeting.df <- meeting.df[order(meeting.df$meet_date),]
table(duplicated(meeting.df[,c("year", "month")]))
meeting.df$meeting_id <- paste0("FEDm_", 1:nrow(meeting.df))
fedminutes.df <- merge(fedminutes.df, meeting.df, by = c("year", "month", "pub_date", "meet_date"), all.x = TRUE)
fedminutes.df <- fedminutes.df[order(fedminutes.df$meet_date),]

fedminutes.df <- fedminutes.df[, c("unique_id", "meeting_id", "year", "month", "document", 
                                   "pub_date", "meet_date", "source", "paragraph")]

# Write the clean Federal Reserve minutes to a file
clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
write.csv(fedminutes.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)


meeting.df$difftime <- as.numeric(difftime(meeting.df$pub_date, meeting.df$meet_date, units = "days"))


plot(meeting.df$meet_date, meeting.df$difftime, type = "l")




############################# Import and filter the NYT data ############################# 

clean_filename = paste(clean_dir, "New_York_Times/econ_news/NYTarticles_short.csv", sep = "/")
nyt.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
colnames(nyt.df)
nyt.df <- nyt.df[,c("unique_id", "date_num", "headline", "main_text")]

nyt.df$date_num <- as.Date(nyt.df$date_num)
nyt.df <- nyt.df[which(nyt.df$date_num >= "1993-01-01"),]
nyt.df <- nyt.df[which(nyt.df$date_num < "2015-01-01"),]

# Some basic stats about the NYT data
nyt.df$nchar <- nchar(nyt.df$main_text)
summary(nyt.df$nchar)


nyt.df$date_num <- as.Date(nyt.df$date_num)
nyt.df$quarter <- floor_date(nyt.df$date_num, "quarter")

nyt.df$ind <- 1

paragraph_quarterly <- nyt.df %>%
  select(quarter, ind) %>%
  group_by(quarter) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)))
paragraph_quarterly$quarter <- as.Date(paragraph_quarterly$quarter)


ggplot(paragraph_quarterly) + 
  scale_color_manual("Corpus",
                     values = c("New York Times" = "dimgray", "Fed" = "blue3", "ECB" = "darkgoldenrod2")) +
  geom_line( aes(x = quarter, y = ind, color = "New York Times")) +
  ylim(0, 1500) +
  xlab('Quarter') +
  ylab("Number of \"economic news\" articles")
#ggtitle("Number of paragraphs over time")
ggsave(paste0(export_dir, "NYT_articles.png"))


# Identify those articles in the week before a meeting
nyt_premeeting <- nyt.df[0,]
nyt_premeeting$subsequent_meeting <- nyt.df[0,1]
# And in the week after the publication of the minutes
nyt_postmeeting <- nyt.df[0,]
nyt_postmeeting$recent_meeting <- nyt.df[0,1]

for (i in 1:nrow(meeting.df)){
  # Meeting detail
  meeting_id <- meeting.df$meeting_id[i]
  meet_date <- meeting.df$meet_date[i]
  pub_date <- meeting.df$pub_date[i]
  print(paste("Finding relevant articles for meeting", meeting.df$meeting_id[i]))
  
  pre_date = meet_date - 6
  post_date = pub_date + 6
  
  # Identify those articles in the week leading up to the meeting
  pre_articles <- nyt.df[which(nyt.df$date_num <= meet_date & 
                                 nyt.df$date_num >= pre_date), ]
  pre_articles$subsequent_meeting <- meeting_id
  
  # Identify those articles in the week following the publication of the minutes
  post_articles <- nyt.df[which(nyt.df$date_num >= pub_date & 
                                 nyt.df$date_num <= post_date), ]
  post_articles$recent_meeting <- meeting_id
  
  nyt_premeeting <- rbind(nyt_premeeting, pre_articles)
  nyt_postmeeting <- rbind(nyt_postmeeting, post_articles)
}

# Merge the pre and post, recognising that some may appear in both
nyt_relevant <- merge(nyt_postmeeting, nyt_premeeting, 
                      by = c("unique_id", "date_num", "headline", "main_text", "nchar"), 
                      all.x = TRUE, all.y = TRUE)

#View(nyt_relevant[which(!is.na(nyt_relevant$recent_meeting) & !is.na(nyt_relevant$subsequent_meeting)),])





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