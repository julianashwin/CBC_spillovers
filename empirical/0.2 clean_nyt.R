setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(readtext)
require(stringr)
require(tm)
require(lubridate)
require(tidyverse)
require(SentimentAnalysis)



################# Import the NYT data #################

clean_dir <- "data/clean_text/"
# Import all NYT data
import_filename = paste0("~/Documents/DPhil/Clean_Data/New_York_Times/econ_news/NYTarticles_short.csv")
nyt.df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
colnames(nyt.df)
nyt.df <- nyt.df[,c("unique_id", "date_num", "headline", "main_text")]
# Sort by date
nyt.df$date_num <- as.Date(nyt.df$date_num)
nyt.df <- nyt.df[which(nyt.df$date_num >= "1993-01-01"),]
nyt.df <- nyt.df[which(nyt.df$date_num <= "2018-01-31"),]
nyt.df <- nyt.df[order(nyt.df$date_num),]
rownames(nyt.df) <- NULL
# Some extra variables
nyt.df$nchar <- nchar(nyt.df$main_text)
summary(nyt.df$nchar)
nyt.df$date_num <- as.Date(nyt.df$date_num)
nyt.df$quarter <- floor_date(nyt.df$date_num, "quarter")
nyt.df$ind <- 1
# Add a sentiment score for each article
nyt.df$sentiment <- NA
pb = txtProgressBar(min = 1, max = nrow(nyt.df), initial = 1) 
for (ii in 1:nrow(nyt.df)){
  para <- nyt.df$main_text[ii]
  sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
  nyt.df$sentiment[ii] <- sentiment[1,1]
  setTxtProgressBar(pb,ii)
}
nyt.df$sentiment[which(is.na(nyt.df$sentiment))] <- 0




# PLot articles per quarter
nyt_qly <- aggregate(nyt.df[,c("ind", "sentiment")], by = list(quarter = nyt.df$quarter),
                     FUN = mean)
nyt_qly$quarter <- as.Date(nyt_qly$quarter)
ggplot(nyt_qly) + 
  geom_line( aes(x = quarter, y = sentiment)) +
  xlab('Quarter') +
  ylab("NYT sentiment")
#ggsave(paste0(export_dir, "NYT_articles.png"))
rm(nyt_qly)



################# Filter out only the relevant articles #################

# Identify those articles in the week before a meeting
nyt_premeeting <- nyt.df[0,]
nyt_premeeting$subsequent_meeting <- nyt.df[0,1]
# And in the week after the meeting
nyt_postmeeting <- nyt.df[0,]
nyt_postmeeting$recent_meeting <- nyt.df[0,1]
# Identify those articles in the week before publication of the minutes
nyt_prepub <- nyt.df[0,]
nyt_prepub$subsequent_pub <- nyt.df[0,1]
# And in the week after the publication of the minutes
nyt_postpub <- nyt.df[0,]
nyt_postpub$recent_pub <- nyt.df[0,1]
# Before a speech
nyt_prespeech <- nyt.df[0,]
nyt_prespeech$subsequent_speech <- nyt.df[0,1]
# And after a speech
nyt_postspeech <- nyt.df[0,]
nyt_postspeech$recent_speech <- nyt.df[0,1]

## Import the meeting details
fedminutes.df <- read.csv(paste0(clean_dir, "fedminutes_all.csv"), stringsAsFactors = FALSE)
fedspeeches.df <- read.csv(paste0(clean_dir, "fedspeeches_all.csv"), stringsAsFactors = FALSE)


meeting_df <- unique(fedminutes.df[,c("meeting_id", "meet_date", "pub_date")])
meeting_df$meet_date <- as.Date(meeting_df$meet_date)
meeting_df$pub_date <- as.Date(meeting_df$pub_date)
speech_df <- unique(fedspeeches.df[,c("speech_id", "date")])
speech_df$date <- as.Date(speech_df$date)


#### Match to the meetings
for (ii in 1:nrow(meeting_df)){
  # Meeting detail
  meeting_id <- meeting_df$meeting_id[ii]
  meet_date <- meeting_df$meet_date[ii]
  pub_date <- meeting_df$pub_date[ii]
  print(paste("Finding relevant articles for meeting", meeting_id))
  # Dates for bounds
  pre_meet = meet_date - 6
  post_meet = meet_date + 6
  pre_pub = pub_date - 6
  post_pub = pub_date + 6
  ### Around the meeting
  premeet_articles <- nyt.df[which(nyt.df$date_num < meet_date & nyt.df$date_num >= pre_meet), ]
  premeet_articles$subsequent_meeting <- meeting_id
  postmeet_articles <- nyt.df[which(nyt.df$date_num > meet_date & nyt.df$date_num <= post_meet), ]
  postmeet_articles$recent_meeting <- meeting_id
  ### Around the publication
  prepub_articles <- nyt.df[which(nyt.df$date_num < pub_date & nyt.df$date_num >= pre_pub), ]
  prepub_articles$subsequent_pub <- meeting_id
  postpub_articles <- nyt.df[which(nyt.df$date_num > pub_date & nyt.df$date_num <= post_pub), ]
  if (nrow(postpub_articles) > 0){
    postpub_articles$recent_pub <- meeting_id
  }
  # Combine into overall dfs
  nyt_premeeting <- rbind(nyt_premeeting, premeet_articles)
  nyt_postmeeting <- rbind(nyt_postmeeting, postmeet_articles)
  nyt_prepub <- rbind(nyt_prepub, prepub_articles)
  nyt_postpub <- rbind(nyt_postpub, postpub_articles)
}


#### Match to the speeches
for (ii in 1:nrow(speech_df)){
  # Meeting detail
  speech_id <- speech_df$speech_id[ii]
  sp_date <- speech_df$date[ii]
  print(paste("Finding relevant articles for speech", speech_id))
  # Dates for bounds
  pre_sp = sp_date - 1
  post_sp = sp_date + 1
  ### Around the speech
  prespeech_articles <- nyt.df[which(nyt.df$date_num < sp_date & nyt.df$date_num >= pre_sp), ]
  if (nrow(prespeech_articles) > 0){
    prespeech_articles$subsequent_speech <- speech_id
  }
  postspeech_articles <- nyt.df[which(nyt.df$date_num > sp_date & nyt.df$date_num <= post_sp), ]
  if (nrow(postspeech_articles) > 0){
    postspeech_articles$recent_speech <- speech_id
  }
  # Combine into overall dfs
  nyt_prespeech <- rbind(nyt_prespeech, prespeech_articles)
  nyt_postspeech <- rbind(nyt_postspeech, postspeech_articles)
}

nyt_prespeech_agg <- aggregate(list(subsequent_speech = nyt_prespeech$subsequent_speech), FUN = toString,
                           by = list(unique_id = nyt_prespeech$unique_id))
nyt_postspeech_agg <- aggregate(list(recent_speech = nyt_postspeech$recent_speech), FUN = toString,
                               by = list(unique_id = nyt_postspeech$unique_id))


nyt_relevant <- merge(nyt.df, nyt_premeeting[,c("unique_id", "subsequent_meeting")],
                      by = "unique_id", all.x = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_postmeeting[,c("unique_id", "recent_meeting")],
                      by = "unique_id", all.x = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_prepub[,c("unique_id", "subsequent_pub")],
                      by = "unique_id", all.x = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_postpub[,c("unique_id", "recent_pub")],
                      by = "unique_id", all.x = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_prespeech_agg, by = "unique_id", all.x = TRUE)
nyt_relevant <- merge(nyt_relevant, nyt_postspeech_agg, by = "unique_id", all.x = TRUE)

nyt_relevant$date <- nyt_relevant$date_num
nyt_relevant <- nyt_relevant[order(nyt_relevant$date),
                             c("unique_id", "date", "quarter",  "headline", "main_text",
                                "subsequent_meeting", "recent_meeting", "subsequent_pub",
                                "recent_pub", "subsequent_speech", "recent_speech")]

# Write the clean Federal Reserve minutes to a file
clean_filename = "~/Documents/DPhil/Clean_Data/New_York_Times/econ_news/nyt_articles_matched.csv"
write.csv(nyt_relevant, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)

