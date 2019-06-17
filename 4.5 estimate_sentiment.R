####
# This file estimates the sentiment of the Federal Reserve and NYT articles
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(tm)
require(slam)
require(topicmodels)
require(tidyverse)
require(tidytext)
require(wordcloud)
require(lubridate)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


# Import the text data
clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

clean_filename = paste(clean_dir, "CBC/NYT_relevant_clean.csv", sep = "/")
nyt_relevant <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

fedminutes.df$quarter <- as.Date(fedminutes.df$meet_date)
fedminutes.df$quarter <- floor_date(fedminutes.df$quarter, unit = "quarter")
fedminutes.df$corpus <- "Fed"
fedminutes.df <- fedminutes.df[,c("quarter", "paragraph", "corpus", "unique_id")]

nyt_relevant$paragraph <- nyt_relevant$main_text
nyt_relevant$quarter <- as.Date(nyt_relevant$date_num)
nyt_relevant$quarter <- floor_date(nyt_relevant$quarter, unit = "quarter")
nyt_relevant$corpus <- "NYT"
nyt_relevant <- nyt_relevant[,c("quarter", "paragraph", "corpus", "unique_id")]



full_data <- rbind(fedminutes.df, nyt_relevant)

full_data$clean_text <- full_data$paragraph

# Remove some characters that can be problematic for tm package
full_data$clean_text <- gsub("-", " ", full_data$clean_text)
full_data$clean_text <- gsub("/'", "", full_data$clean_text)
full_data$clean_text <- gsub("’", "", full_data$clean_text)
full_data$clean_text <- gsub("‘", "", full_data$clean_text)
full_data$clean_text <- gsub("“", "", full_data$clean_text)
full_data$clean_text <- gsub("”", "", full_data$clean_text)
full_data$clean_text <- gsub("€", " ", full_data$clean_text)
full_data$clean_text <- gsub("£", " ", full_data$clean_text)
full_data$clean_text <- gsub(",", " ", full_data$clean_text)
full_data$clean_text <- gsub(":", " ", full_data$clean_text)


articles.corpus <- Corpus(VectorSource(unlist(full_data[, "clean_text"])))
# Preliminary cleaning
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, stripWhitespace)
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, removeNumbers)
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, removePunctuation)
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, content_transformer(tolower))
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, removeWords, stopwords("english"))
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, stripWhitespace)
inspect(articles.corpus[[700]])


### Convert to document-term-matrix
articles.dtm <- DocumentTermMatrix(articles.corpus, control = list(minWordLength = 3))

### Add back into dataframe
full_data$word_count <- row_sums(articles.dtm)
full_data$clean_text <- sapply(articles.corpus, as.character)
full_data$clean_text <- str_trim(full_data$clean_text)


text_tidy <- full_data %>%
  unnest_tokens(word, clean_text) %>% # Break the documents into individual words
  filter(!nchar(word) < 2) # Remove one or two character tokens

# Unnest to get a template to bind to
text_loughran <- text_tidy %>%
  inner_join(get_sentiments("loughran"))
text_nrc <- text_tidy %>%
  inner_join(get_sentiments("nrc"))


### Plot word clouds representing the sentiment scores
text_loughran_negative <- text_loughran[which(text_loughran$sentiment == "negative"),]
text_loughran_negative$ind <- 1
text_loughran_positive <- text_loughran[which(text_loughran$sentiment == "positive"),]
text_loughran_positive$ind <- 1

negative_words <- aggregate(text_loughran_negative$ind, by = list(text_loughran_negative$word), FUN = sum)
positive_words <- aggregate(text_loughran_positive$ind, by = list(text_loughran_positive$word), FUN = sum)


# Word cloud for negative sentiment
file_name <- paste0(export_dir,"negative_sentiment_words.png")
# Create a term distribution df
negative_words <- negative_words[order(-negative_words$x),]
negative_words_short <- negative_words[1:50,]

par(mar = rep(0.5, 4))
png(file_name)
wordcloud(words = negative_words_short$Group.1,
          freq = negative_words_short$x,
          max.words = 50,
          random.order = FALSE,
          rot.per = 0.35,
          colors= brewer.pal(3, "Dark2"),
          scale=c(4,.2))
dev.off()


# Word cloud for positive sentiment
file_name <- paste0(export_dir,"positive_sentiment_words.png")
# Create a term distribution df
positive_words <- positive_words[order(-positive_words$x),]
positive_words_short <- positive_words[1:50,]

par(mar = rep(0.5, 4))
png(file_name)
wordcloud(words = positive_words_short$Group.1,
          freq = positive_words_short$x,
          max.words = 50,
          random.order = FALSE,
          rot.per = 0.35,
          colors= brewer.pal(3, "Dark2"),
          scale=c(5,.2))
dev.off()








test.df <- merge(text_loughran, full_data, by = c("unique_id"), all.x = TRUE) 


fed_loughran <- text_loughran[which(text_loughran$corpus == "Fed"),]
nyt_loughran <- text_loughran[which(text_loughran$corpus == "NYT"),]

#### Check that the various categories have been picked up
loughran_plot <- text_loughran %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  #theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  #scale_y_continuous(limits = c(0, 8000)) +
  ggtitle("Loughran Sentiment") +
  coord_flip()
loughran_plot


# Break down Loughran sentiment by observation
fed_data_sentiment <- fed_loughran %>%
  count(sentiment, unique_id) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)
nyt_data_sentiment <- nyt_loughran %>%
  count(sentiment, unique_id) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

full_data_sentiment <- rbind(fed_data_sentiment, nyt_data_sentiment)
full_data_sentiment <- merge(full_data_sentiment, full_data[,c("quarter", "unique_id", "corpus")], by = "unique_id")


sentiment_quarterly <- full_data_sentiment %>%
  group_by(quarter, corpus) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

fed_sentiment_quarterly <- sentiment_quarterly[which(sentiment_quarterly$corpus == "Fed"),]
nyt_sentiment_quarterly <- sentiment_quarterly[which(sentiment_quarterly$corpus == "NYT"),]

fed_sentiment_quarterly$fed_sentiment <- fed_sentiment_quarterly$percent_positive 
nyt_sentiment_quarterly$nyt_sentiment <- nyt_sentiment_quarterly$percent_positive

fed_sentiment_quarterly <- select(fed_sentiment_quarterly, quarter, fed_sentiment)
nyt_sentiment_quarterly <- select(nyt_sentiment_quarterly, quarter, nyt_sentiment)


fed_sentiment_quarterly$fed_sentiment_std <- (fed_sentiment_quarterly$fed_sentiment - mean(fed_sentiment_quarterly$fed_sentiment))/sd(fed_sentiment_quarterly$fed_sentiment)
nyt_sentiment_quarterly$nyt_sentiment_std <- (nyt_sentiment_quarterly$nyt_sentiment - mean(nyt_sentiment_quarterly$nyt_sentiment))/sd(nyt_sentiment_quarterly$nyt_sentiment)

sentiment_quarterly <- merge(fed_sentiment_quarterly, nyt_sentiment_quarterly, by = "quarter", all.x = TRUE,
                             all.y = TRUE)

sentiment_quarterly$quarter <- as.Date(sentiment_quarterly$quarter)

cor.test(sentiment_quarterly$fed_sentiment, sentiment_quarterly$nyt_sentiment)



ggplot(sentiment_quarterly, aes(x = quarter)) + 
  geom_line(aes(y = fed_sentiment_std)) + 
  geom_line(aes( y = nyt_sentiment_std))

n = length(sentiment_quarterly) 
summary(lm(nyt_sentiment[-n] ~ fed_sentiment[-1], data = sentiment_quarterly))
summary(lm(nyt_sentiment[-n] ~ fed_sentiment[-1] + nyt_sentiment[-1], data = sentiment_quarterly))

summary(lm(fed_sentiment[-n] ~ nyt_sentiment[-1], data = sentiment_quarterly))
summary(lm(fed_sentiment[-n] ~ nyt_sentiment[-1] + fed_sentiment[-1], data = sentiment_quarterly))



clean_filename = paste(clean_dir, "CBC/fednyt_sentiment", sep = "/")
write.csv(sentiment_quarterly, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)



