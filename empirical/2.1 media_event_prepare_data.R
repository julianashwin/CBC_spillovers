####
# This file estimates topics on the Federal Reserve and NYT articles combined
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
require(plm)
require(lfe)
library(fixest)



standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}



### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/topic_data/overall/"
#export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


k <- 29

#FOMC minutes
minutes_key <- read.csv("data/clean_text/minutes_key.csv", stringsAsFactors = F)
import_filename = paste0(clean_dir,"meeting_topics_k",k,".csv")
minutes_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
minutes_df <- merge(minutes_key, minutes_df, by = "unique_id")
#minutes_df$sentiment_raw <- minutes_df$sentiment
minutes_df$sentiment <- standardise(minutes_df$sentiment)

# FOMC speeches
speeches_key <- read.csv("data/clean_text/speeches_key.csv", stringsAsFactors = F)
import_filename = paste0(clean_dir,"speech_topics_k",k,".csv")
speeches_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
speeches_df <- merge(speeches_key, speeches_df, by = "unique_id")
#speeches_df$sentiment_raw <- speeches_df$sentiment
speeches_df$sentiment <- standardise(speeches_df$sentiment)


# NYT articles
articles_key <- read.csv("data/clean_text/articles_key.csv", stringsAsFactors = F)
import_filename = paste0(clean_dir,"article_topics_k",k,".csv")
articles_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
articles_df <- merge(articles_key, articles_df, by = "unique_id")
#articles_df$sentiment_raw <- articles_df$sentiment
articles_df$sentiment <- standardise(articles_df$sentiment)


# Define topic-sentiment 
ggplot() + theme_bw() + 
  geom_density(data = minutes_df, aes(x = sentiment, color = "mins")) +
  geom_density(data = speeches_df, aes(x = sentiment, color = "speech")) +
  geom_density(data = articles_df, aes(x = sentiment, color = "news")) 
for (ii in 1:k){
  minutes_df[,paste0("T",ii,"_sent")] <- minutes_df[,paste0("T",ii)]*
    minutes_df$sentiment
  speeches_df[,paste0("T",ii,"_sent")] <- speeches_df[,paste0("T",ii)]*
    speeches_df$sentiment
  articles_df[,paste0("T",ii,"_sent")] <- articles_df[,paste0("T",ii)]*
    articles_df$sentiment
}
#minutes_df$sentiment_decomp <- rowSums(minutes_df[,which(str_detect(names(minutes_df),"_sent"))])
#table(round(minutes_df$sentiment_decomp - minutes_df$sentiment,10))

if (k == 29){
  # Plot sentiment decomp for minutes
  minutes_df$event <- minutes_df$meeting_id
  minutes_agg <- minutes_df %>% 
    group_by(event, meet_date, pub_date) %>% 
    summarise_if(is.numeric, mean)
  minutes_long <- melt(minutes_agg, id = c("event", "meet_date", "pub_date", "wordcount", "sentiment"),
                       variable.name = "topic", value.name = "mins_theta")
  minutes_long <- minutes_long[which(str_detect(minutes_long$topic, "_sent")),]
  minutes_long$topic <- as.factor(as.numeric(str_remove(
    str_remove(minutes_long$topic, "_sent"), "T")))
  minutes_long$date <- as.Date(minutes_long$meet_date)
  ggplot(minutes_long, aes(x = date)) + theme_bw() + 
    geom_bar(aes(y = mins_theta, fill = topic), positin = "stack", stat = "identity") +
    geom_line(aes(y = sentiment))
  # Plot sentiment decomp for speeches
  speeches_df$quarter <- floor_date(as.Date(speeches_df$date), unit = "quarters")
  speeches_agg <- speeches_df %>% 
    group_by(quarter) %>% 
    summarise_if(is.numeric, mean)
  speeches_long <- melt(speeches_agg, id = c("quarter", "wordcount", "sentiment"),
                       variable.name = "topic", value.name = "sp_theta")
  speeches_long <- speeches_long[which(str_detect(speeches_long$topic, "_sent")),]
  speeches_long$topic <- as.factor(as.numeric(str_remove(
    str_remove(speeches_long$topic, "_sent"), "T")))
  speeches_long$quarter <- as.Date(speeches_long$quarter)
  ggplot(speeches_long, aes(x = quarter)) + theme_bw() + 
    geom_bar(aes(y = sp_theta, fill = topic), positin = "stack", stat = "identity") +
    geom_line(aes(y = sentiment))
  
}


"
Split articles into windows around meetings and speeches
"
# Minutes
premeet_articles.df <- articles_df[which(!is.na(articles_df$subsequent_meeting)),]
postmeet_articles.df <- articles_df[which(!is.na(articles_df$recent_meeting)),]
prepub_articles.df <- articles_df[which(!is.na(articles_df$subsequent_pub)),]
postpub_articles.df <- articles_df[which(!is.na(articles_df$recent_pub)),]

# Bit more complicated for speeches as we need to account for multiple matches
# Pre speech coverage
prespeech_articles.df <- articles_df[which(!is.na(articles_df$subsequent_speech)),]
temp_df <- prespeech_articles.df[0,]
for (ii in which(str_detect(prespeech_articles.df$subsequent_speech, ","))){
  temp_row <- prespeech_articles.df[ii,]
  speeches <- str_squish(str_split(temp_row$subsequent_speech, ",")[[1]])
  temp_rows <- temp_row[rep(1, length(speeches)),]
  temp_rows$subsequent_speech <- speeches
  temp_rows$recent_speech <- NA
  temp_df <- rbind(temp_df, temp_rows)
}
prespeech_articles.df <- 
  prespeech_articles.df[which(!str_detect(prespeech_articles.df$subsequent_speech, ",")),]
prespeech_articles.df <- rbind(prespeech_articles.df, temp_df)
# Post speech coverage 
postspeech_articles.df <- articles_df[which(!is.na(articles_df$recent_speech)),]
temp_df <- postspeech_articles.df[0,]
for (ii in which(str_detect(postspeech_articles.df$recent_speech, ","))){
  temp_row <- postspeech_articles.df[ii,]
  speeches <- str_squish(str_split(temp_row$recent_speech, ",")[[1]])
  temp_rows <- temp_row[rep(1, length(speeches)),]
  temp_rows$recent_speech <- speeches
  temp_rows$subsequent_speech <- NA
  temp_df <- rbind(temp_df, temp_rows)
}
postspeech_articles.df <- 
  postspeech_articles.df[which(!str_detect(postspeech_articles.df$recent_speech, ",")),]
postspeech_articles.df <- rbind(postspeech_articles.df, temp_df)



## Edit each id to show whether this is in pre or post sample
# Meeting
premeet_articles.df$meeting <- paste0(premeet_articles.df$subsequent_meeting, "_premeet")
premeet_articles.df$article_id <- paste0(premeet_articles.df$unique_id, "_premeet")
table(is.na(premeet_articles.df$recent_meeting))
postmeet_articles.df$meeting <- paste0(postmeet_articles.df$recent_meeting, "_postmeet")
postmeet_articles.df$article_id <- paste0(postmeet_articles.df$unique_id, "_postmeet")
table(is.na(postmeet_articles.df$subsequent_meeting))
# Publication
prepub_articles.df$meeting <- paste0(prepub_articles.df$subsequent_pub, "_prepub")
prepub_articles.df$article_id <- paste0(prepub_articles.df$unique_id, "_prepub")
table(is.na(premeet_articles.df$recent_pub))
postpub_articles.df$meeting <- paste0(postpub_articles.df$recent_pub, "_postpub")
postpub_articles.df$article_id <- paste0(postpub_articles.df$unique_id, "_postpub")
table(is.na(postpub_articles.df$subsequent_pub))
# Speeches
prespeech_articles.df$speech <- paste0(prespeech_articles.df$subsequent_speech, "_prespeech")
prespeech_articles.df$article_id <- paste0(prespeech_articles.df$unique_id, "_prespeech")
table(is.na(prespeech_articles.df$subsequent_speech))
postspeech_articles.df$speech <- paste0(postspeech_articles.df$recent_speech, "_postspeech")
postspeech_articles.df$article_id <- paste0(postspeech_articles.df$unique_id, "_postspeech")
table(is.na(postspeech_articles.df$recent_speech))

# Group together the minutes articles
articles_mins_df <- rbind(premeet_articles.df, postmeet_articles.df, 
                           prepub_articles.df, postpub_articles.df)
articles_mins_df <- articles_mins_df %>% arrange(meeting, article_id)
table(articles_mins_df$meeting)[1:10]

# Group together the speeches articles
articles_speeches_df <- rbind(prespeech_articles.df, postspeech_articles.df)
articles_speeches_df <- articles_speeches_df %>% arrange(speech, article_id)
table(articles_speeches_df$speech)[1:10]

rm(premeet_articles.df, postmeet_articles.df, prepub_articles.df, postpub_articles.df,
   prespeech_articles.df, postspeech_articles.df, 
   minutes_agg, minutes_key, minutes_long,temp_df, temp_row, temp_rows,
   speeches_agg, minutes_key, speeches_long)



"
Aggregate and convert articles for minutes to panel structure
"
# Articles for minutes
articles_mins_df$type <- articles_mins_df$meeting
articles_mins_df$event <- articles_mins_df$meeting
articles_mins_df$type <- str_remove_all(articles_mins_df$type, "FEDm_[0-9]+_")
articles_mins_df$event <- str_remove_all(articles_mins_df$event, "_p[a-z]+")
articles_mins_agg <- articles_mins_df %>% 
  group_by(event, type) %>% 
  summarise_if(is.numeric, mean)
# Melt to long by topic
articles_mins_long <- melt(articles_mins_agg, id = c("event", "type", "wordcount", "sentiment"),
                           variable.name = "topic", value.name = "theta")
# The widen out to separate sentiment from topic
articles_mins_long$theta_sent <- ""
articles_mins_long$theta_sent[str_detect(articles_mins_long$topic, "_sent")] <- "_sent"
articles_mins_long$topic <- str_remove(articles_mins_long$topic, "_sent")
#table(articles_mins_long$topic)
articles_mins_long <- pivot_wider(articles_mins_long,
                                  id_cols = c(event, topic, type, sentiment), names_from = theta_sent, 
                                  names_glue = "{.value}{theta_sent}",values_from = c(theta))
# Then widen again get different types
articles_mins_long <- pivot_wider(articles_mins_long,
                                  id_cols = c(event, topic), names_from = type, 
                                  names_glue = "{.value}_{type}",
                                  values_from = c(theta, theta_sent, sentiment))


"
Aggregate and convert articles for speeches to panel structure
"
# Articles for speeches
articles_speeches_df$type <- articles_speeches_df$speech
articles_speeches_df$event <- articles_speeches_df$speech
articles_speeches_df$type <- str_remove_all(articles_speeches_df$type, "SPEECH_[0-9]+_")
articles_speeches_df$event <- str_remove_all(articles_speeches_df$event, "_p[a-z]+")
articles_speeches_agg <- articles_speeches_df %>% 
  group_by(event, type) %>% 
  summarise_if(is.numeric, mean)
# Melt to long by topic
articles_speeches_long <- melt(articles_speeches_agg, id = c("event", "type", "wordcount", "sentiment"),
                           variable.name = "topic", value.name = "theta")
# The widen out to separate sentiment from topic
articles_speeches_long$theta_sent <- ""
articles_speeches_long$theta_sent[str_detect(articles_speeches_long$topic, "_sent")] <- "_sent"
articles_speeches_long$topic <- str_remove(articles_speeches_long$topic, "_sent")
#table(articles_mins_long$topic)
articles_speeches_long <- pivot_wider(articles_speeches_long,
                                  id_cols = c(event, topic, type, sentiment), names_from = theta_sent, 
                                  names_glue = "{.value}{theta_sent}",values_from = c(theta))
# Then widen again get different types
articles_speeches_long <- pivot_wider(articles_speeches_long,
                                  id_cols = c(event, topic), names_from = type, 
                                  names_glue = "{.value}_{type}",
                                  values_from = c(theta, theta_sent, sentiment))



"
Aggregate the minutes and speeches then merge with media
"
## Minutes
minutes_df$event <- minutes_df$meeting_id
minutes_agg <- minutes_df %>% 
  group_by(event, meet_date, pub_date) %>% 
  summarise_if(is.numeric, mean)
# Convert to long by topic
minutes_long <- melt(minutes_agg, id = c("event", "meet_date", "pub_date", "wordcount", "sentiment"),
                     variable.name = "topic", value.name = "mins_theta")
# The widen out to separate sentiment from topic
minutes_long$theta_sent <- ""
minutes_long$theta_sent[str_detect(minutes_long$topic, "_sent")] <- "_sent"
minutes_long$topic <- str_remove(minutes_long$topic, "_sent")
minutes_long <- pivot_wider(minutes_long,
                            id_cols = c(event, meet_date, pub_date, topic, sentiment), 
                            names_from = theta_sent, names_glue = "{.value}{theta_sent}",
                            values_from = c(mins_theta))
minutes_panel <- merge(minutes_long, articles_mins_long, by = c("event", "topic"))
minutes_panel$news_meetwindow <- minutes_panel$theta_postmeet - minutes_panel$theta_premeet
minutes_panel$news_pubwindow <- minutes_panel$theta_postpub - minutes_panel$theta_prepub
minutes_panel$news_sent_meetwindow <- minutes_panel$theta_sent_postmeet - minutes_panel$theta_sent_premeet
minutes_panel$news_sent_pubwindow <- minutes_panel$theta_sent_postpub - minutes_panel$theta_sent_prepub

minutes_panel$overall_sent_pubwindow <- minutes_panel$sentiment_postpub - 
  minutes_panel$sentiment_prepub
model <- lm(overall_sent_pubwindow ~ sentiment + sentiment_prepub +
              sentiment_premeet + sentiment_postmeet, minutes_panel[which(minutes_panel$topic=="T1"),]) 
summary(model)

model <- feols(theta_postpub ~ mins_theta + theta_prepub| topic, minutes_panel) 
summary(model, vcv = "iid")
model <- felm(theta_postpub ~ mins_theta + theta_prepub|topic|0|meet_date, minutes_panel) 
summary(model)
summary(felm(theta_postpub ~ mins_theta + theta_prepub|topic|0|meet_date, data = minutes_panel))

summary(felm(theta_postpub ~ mins_theta + theta_prepub|topic|0|meet_date, data = minutes_panel))


summary(lm(news_pubwindow ~ mins_theta + theta_prepub + topic, data = minutes_panel))
summary(lm(news_meetwindow ~ mins_theta + theta_premeet + topic, data = minutes_panel))

summary(felm(news_sent_pubwindow ~ mins_theta + mins_theta_sent + theta_prepub + theta_sent_prepub
             |topic, data = minutes_panel))
summary(felm(news_sent_pubwindow ~ mins_theta + mins_theta_sent + theta_prepub + theta_sent_prepub
           |topic|0|meet_date, data = minutes_panel))
summary(lm(news_sent_meetwindow ~ mins_theta + theta_premeet + topic, data = minutes_panel))

## Speeches
speeches_df$event <- speeches_df$speech_id
speeches_agg <- speeches_df %>% 
  group_by(event, date) %>% 
  summarise_if(is.numeric, mean)
# Get the first speech id for each date
speeches_key <- speeches_key[order(speeches_key$date),]
speeches_key$speech_id_new <- speeches_key$speech_id
speeches_ids <- speeches_key[!duplicated(speeches_key[,c("date")]),
                             c("speech_id_new","date")]
# Get average across speeches 
speeches_agg1 <- merge(speeches_ids, speeches_agg, by = "date")
speeches_agg1 <- speeches_agg1 %>% 
  group_by(speech_id_new, date) %>% 
  summarise_if(is.numeric, mean)
speeches_agg1$event <- speeches_agg1$speech_id_new

# Convert to long by topic
speeches_agg1 <- subset(speeches_agg1, select=-c(speech_id_new))
speeches_long <- melt(speeches_agg1, id = c("event", "date", "wordcount", "sentiment"),
                     variable.name = "topic", value.name = "speech_theta")
# The widen out to separate sentiment from topic
speeches_long$theta_sent <- ""
speeches_long$theta_sent[str_detect(speeches_long$topic, "_sent")] <- "_sent"
speeches_long$topic <- str_remove(speeches_long$topic, "_sent")
speeches_long <- pivot_wider(speeches_long,
                            id_cols = c(event, date, date, topic), 
                            names_from = theta_sent, names_glue = "{.value}{theta_sent}",
                            values_from = c(speech_theta))
speeches_panel <- merge(speeches_long, articles_speeches_long, by = c("event", "topic"))
speeches_panel$news_speechwindow <- speeches_panel$theta_postspeech - speeches_panel$theta_prespeech
speeches_panel$news_sent_speechwindow <- speeches_panel$theta_sent_postspeech - 
  speeches_panel$theta_sent_prespeech

model <-
summary(lm(theta_sent_postspeech ~ speech_theta_sent + speech_theta + theta_sent_prespeech + topic, data = speeches_panel))
summary(lm(news_speechwindow ~ speech_theta + theta_prespeech + topic, data = speeches_panel))




"
Convert into panel data.frame
"
# Speeches
speeches_panel$date <- as.Date(speeches_panel$date)
speeches_panel <- speeches_panel[order(speeches_panel$date,
                                       speeches_panel$topic),]
speeches_panel$period <- factor(as.character(speeches_panel$date), ordered = TRUE,
                                levels = unique(as.character(speeches_panel$date)))
speeches_panel$period <- as.numeric(speeches_panel$period)
speeches_panel <- pdata.frame(speeches_panel, index = c("topic", "period"))

# Minutes
minutes_panel$meet_date <- as.Date(minutes_panel$meet_date)
minutes_panel <- minutes_panel[order(minutes_panel$meet_date,
                                     minutes_panel$topic),]
minutes_panel$period <- factor(as.character(minutes_panel$meet_date), ordered = TRUE,
                               levels = unique(as.character(minutes_panel$meet_date)))
minutes_panel$period <- as.numeric(minutes_panel$period)
minutes_panel <- pdata.frame(minutes_panel, index = c("topic", "period"))


"
Standardise
"
# Minutes
for (ii in unique(minutes_panel$topic)){
  obs <- which(minutes_panel$topic == ii)
  # Mins theta
  minutes_panel$mins_theta_std[obs] <- standardise(minutes_panel$mins_theta[obs])
  # Pre meeting articles
  minutes_panel$theta_premeet_std[obs] <- standardise(minutes_panel$theta_premeet[obs])
  # Post meeting articles
  minutes_panel$theta_postmeet_std[obs] <- standardise(minutes_panel$theta_postmeet[obs])
  # Pre meeting articles
  minutes_panel$theta_prepub_std[obs] <- standardise(minutes_panel$theta_prepub[obs])
  # Post meeting articles
  minutes_panel$theta_postpub_std[obs] <- standardise(minutes_panel$theta_postpub[obs])
  # Publication window
  minutes_panel$news_pubwindow_std[obs] <- standardise(minutes_panel$news_pubwindow[obs])
}
# Speeches
for (ii in unique(speeches_panel$topic)){
  obs <- which(speeches_panel$topic == ii)
  # Mins theta
  speeches_panel$speech_theta_std[obs] <- standardise(speeches_panel$speech_theta[obs])
  # Pre meeting articles
  speeches_panel$theta_prespeech_std[obs] <- standardise(speeches_panel$theta_prespeech[obs])
  # Post meeting articles
  speeches_panel$theta_postspeech_std[obs] <- standardise(speeches_panel$theta_postspeech[obs])
  # Publication window
  speeches_panel$news_speechwindow_std[obs] <- standardise(speeches_panel$news_speechwindow[obs])
}


"
Some regressions
"
model1 <- felm(news_pubwindow ~ mins_theta + theta_prepub + theta_premeet +
                plm::lag(news_pubwindow, 1:3) | topic + period, data = minutes_panel)
summary(model1)
model1 <- felm(news_pubwindow_std ~ mins_theta_std + theta_prepub_std + theta_premeet_std +
                 plm::lag(news_pubwindow_std, 1:3) | topic + period, data = minutes_panel)
summary(model1)



model1 <- felm(news_speechwindow_std ~ speech_theta_std + theta_prespeech_std +
                 plm::lag(news_speechwindow_std, 1:3) | topic + period, data = speeches_panel)
summary(model1)

model1 <- felm(news_speechwindow_std ~ speech_theta_std + theta_prespeech_std 
                | topic + period | 0 | topic + period, speeches_panel)
summary(model1)
model1 <- feols(news_speechwindow ~ speech_theta 
                | topic + period, speeches_panel)
model1 <- feols(news_speechwindow_std ~ speech_theta_std + theta_prespeech_std 
                | topic + period, speeches_panel)
summary(model1, vcov = "twoway")



model1 <- felm(mins_theta ~ theta_premeet + 
                 plm::lag(mins_theta,1)| topic + period , 
               data = minutes_panel)
summary(model1)


model1 <- felm(news_speechwindow ~ speech_theta + theta_prespeech +
                 plm::lag(news_speechwindow,1)| topic + period , 
               data = speeches_panel)
summary(model1)




############################# End ############################# 