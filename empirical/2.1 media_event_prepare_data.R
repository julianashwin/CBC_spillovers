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
require(ggplot2)
require(viridis)
require(ggpubr)
require(scales)


standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}
felm_DK_se <- function(reg_formula, df_panel){
  
  # Estimate regressions with feols and felm
  model <- feols(reg_formula, data = df_panel)
  model_felm <- felm(reg_formula, data = df_panel)
  
  stopifnot(length(model_felm$se) ==  
              length(summary(model, vcov = DK ~ period)$coeftable[,"Std. Error"]))
  model_felm$se <- summary(model, vcov = DK ~ period)$coeftable[,"Std. Error"]
  model_felm$tval <- summary(model, vcov = DK ~ period)$coeftable[,"t value"]
  model_felm$pval <- summary(model, vcov = DK ~ period)$coeftable[,"Pr(>|t|)"]
  return(model_felm)
}


### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/topic_data/overall/"
#export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"

comp_K_df <- data.frame(K = 15:40, mins_coef = NA, mins_se = NA, speech_coef = NA, speech_se = NA, 
                        mins_t_coef = NA, mins_t_se = NA, speech_t_coef = NA, speech_t_se = NA,
                        mins_spec_coef = NA, mins_spec_se = NA, speech_spec_coef = NA, speech_spec_se = NA, 
                        mins_t_spec_coef = NA, mins_t_spec_se = NA, speech_t_spec_coef = NA, speech_t_spec_se = NA,
                        mins_std_coef = NA, mins_std_se = NA, speech_std_coef = NA, speech_std_se = NA, 
                        mins_t_std_coef = NA, mins_t_std_se = NA, speech_t_std_coef = NA, speech_t_std_se = NA)

k <- 29
for (k in 15:40){
  print(k)
  
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



for (ii in 1:max(as.numeric(str_remove_all(str_remove_all(names(minutes_df), "[A-Za-z]+"), "_")), na.rm = T)){
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
  minutes_long <- minutes_long[which(minutes_long$date < "2018-01-01"),]
  minutes_long$Topic <- ""
  
  minutes_long$Topic[minutes_long$topic ==1] <- "1: Healthcare"
  minutes_long$Topic[minutes_long$topic ==2] <- "2: Interest rates"
  minutes_long$Topic[minutes_long$topic ==3] <- "3: Inflation"
  minutes_long$Topic[minutes_long$topic ==4] <- "4: China"
  minutes_long$Topic[minutes_long$topic ==5] <- "5: Committee views"
  minutes_long$Topic[minutes_long$topic ==6] <- "6: Education"
  minutes_long$Topic[minutes_long$topic ==7] <- "7: Data I"
  minutes_long$Topic[minutes_long$topic ==8] <- "8: Real estate"
  minutes_long$Topic[minutes_long$topic ==9] <- "9: Expectations"
  minutes_long$Topic[minutes_long$topic ==10] <- "10: Bond markets"
  minutes_long$Topic[minutes_long$topic ==11] <- "11: Investment"
  minutes_long$Topic[minutes_long$topic ==12] <- "12: Production"
  minutes_long$Topic[minutes_long$topic ==13] <- "13: Fiscal policy"
  minutes_long$Topic[minutes_long$topic ==14] <- "14: General I"
  minutes_long$Topic[minutes_long$topic ==15] <- "15: Policy decision"
  minutes_long$Topic[minutes_long$topic ==16] <- "16: Infrastructure"
  minutes_long$Topic[minutes_long$topic ==17] <- "17: Politics"
  minutes_long$Topic[minutes_long$topic ==18] <- "18: Consumption"
  minutes_long$Topic[minutes_long$topic ==19] <- "19: General II"
  minutes_long$Topic[minutes_long$topic ==20] <- "20: Europe"
  minutes_long$Topic[minutes_long$topic ==21] <- "21: Corporations"
  minutes_long$Topic[minutes_long$topic ==22] <- "22: Finance"
  minutes_long$Topic[minutes_long$topic ==23] <- "23: Japan"
  minutes_long$Topic[minutes_long$topic ==24] <- "24: Foreign policy"
  minutes_long$Topic[minutes_long$topic ==25] <- "25: Stock market"
  minutes_long$Topic[minutes_long$topic ==26] <- "26: Growth"
  minutes_long$Topic[minutes_long$topic ==27] <- "27: Legal"
  minutes_long$Topic[minutes_long$topic ==28] <- "28: Data II"
  minutes_long$Topic[minutes_long$topic ==29] <- "29: Labour market"
  
  #minutes_long <- minutes_long[which(minutes_long$topic %in% c(1,2,3)),]
  
  minutes_long$Topic <- factor(as.character(minutes_long$Topic), ordered = TRUE,
         levels = c("1: Healthcare", "2: Interest rates", "3: Inflation", "4: China", 
                    "5: Committee views", "6: Education", "7: Data I", "8: Real estate",
                    "9: Expectations", "10: Bond markets", "11: Investment", "12: Production",
                    "13: Fiscal policy", "14: General I", "15: Policy decision", 
                    "16: Infrastructure", "17: Politics", "18: Consumption", "19: General II",
                    "20: Europe", "21: Corporations", "22: Finance", "23: Japan",
                    "24: Foreign policy", "25: Stock market", "26: Growth", "27: Legal",
                    "28: Data II", "29: Labour market"))
  col_theme <- scale_fill_manual("Topic", 
    values = c("1: Healthcare"= viridis(29)[1], "2: Interest rates" = viridis(29)[2], 
               "3: Inflation" = "red", "4: China" = viridis(29)[4], 
               "5: Committee views" = viridis(29)[5], "6: Education" = viridis(29)[6], 
               "7: Data I" = viridis(29)[7], "8: Real estate" = viridis(29)[8],
               "9: Expectations" = viridis(29)[9], "10: Bond markets" = viridis(29)[10], 
               "11: Investment" = viridis(29)[11], "12: Production" = viridis(29)[12],
               "13: Fiscal policy" = viridis(29)[13], "14: General I" = viridis(29)[14], 
               "15: Policy decision" = viridis(29)[15], 
               "16: Infrastructure" = viridis(29)[16], "17: Politics" = viridis(29)[17], 
               "18: Consumption" = viridis(29)[18], "19: General II" = viridis(29)[19],
               "20: Europe" = viridis(29)[20], "21: Corporations" = viridis(29)[21], 
               "22: Finance" = "firebrick", "23: Japan" = viridis(29)[23],
               "24: Foreign policy" = viridis(29)[24], "25: Stock market" = viridis(29)[25], 
               "26: Growth" = viridis(29)[26], "27: Legal" = viridis(29)[27],
               "28: Data II" = viridis(29)[28], "29: Labour market" = viridis(29)[29]))
  plt1 <- ggplot(minutes_long, aes(x = date)) + theme_bw() + col_theme + 
    geom_bar(aes(y = mins_theta, fill = Topic), positin = "stack", stat = "identity") +
    geom_line(aes(y = sentiment)) + xlab("Date") + ylab("FOMC minutes sentiment")
  
  plt2 <- ggplot(minutes_long[which(minutes_long$date >= "2010-01-01" & minutes_long$date < "2011-01-01"),], 
                 aes(x = date)) + theme_bw() + col_theme + 
    scale_x_date(date_labels="%b '%y") + #,date_breaks  ="6 month") +
    geom_bar(aes(y = mins_theta, fill = Topic), positin = "stack", stat = "identity") +
    geom_line(aes(y = sentiment)) + xlab("Date") + ylab("")
  ggarrange(plt1, plt2, ncol = 2, common.legend = T, widths = c(1, 0.25))
  
  ggsave("figures/fed_media_topics/minutes_sentiment_decomp.pdf", width = 14, height = 7)
  
  
  ggplot(minutes_long[which(minutes_long$topic %in% c(3,22)),], aes(x = date)) + theme_bw()  + 
    geom_line(aes(y = mins_theta, color = Topic))
  
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
                            id_cols = c(event, date, topic, sentiment), 
                            names_from = theta_sent, names_glue = "{.value}{theta_sent}",
                            values_from = c(speech_theta))
speeches_panel <- merge(speeches_long, articles_speeches_long, by = c("event", "topic"))
speeches_panel$news_speechwindow <- speeches_panel$theta_postspeech - speeches_panel$theta_prespeech
speeches_panel$news_sent_speechwindow <- speeches_panel$theta_sent_postspeech - 
  speeches_panel$theta_sent_prespeech

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
  minutes_panel$mins_theta_sent_std[obs] <- standardise(minutes_panel$mins_theta_sent[obs])
  # Pre meeting articles
  minutes_panel$theta_premeet_std[obs] <- standardise(minutes_panel$theta_premeet[obs])
  minutes_panel$theta_sent_premeet_std[obs] <- standardise(minutes_panel$theta_sent_premeet[obs])
  # Post meeting articles
  minutes_panel$theta_postmeet_std[obs] <- standardise(minutes_panel$theta_postmeet[obs])
  minutes_panel$theta_sent_postmeet_std[obs] <- standardise(minutes_panel$theta_sent_postmeet[obs])
  # Pre meeting articles
  minutes_panel$theta_prepub_std[obs] <- standardise(minutes_panel$theta_prepub[obs])
  minutes_panel$theta_sent_prepub_std[obs] <- standardise(minutes_panel$theta_sent_prepub[obs])
  # Post meeting articles
  minutes_panel$theta_postpub_std[obs] <- standardise(minutes_panel$theta_postpub[obs])
  minutes_panel$theta_sent_postpub_std[obs] <- standardise(minutes_panel$theta_sent_postpub[obs])
  # Publication window
  minutes_panel$news_pubwindow_std[obs] <- standardise(minutes_panel$news_pubwindow[obs])
  minutes_panel$news_sent_pubwindow_std[obs] <- standardise(minutes_panel$news_sent_pubwindow[obs])
}
# Speeches
for (ii in unique(speeches_panel$topic)){
  obs <- which(speeches_panel$topic == ii)
  # Mins theta
  speeches_panel$speech_theta_std[obs] <- standardise(speeches_panel$speech_theta[obs])
  speeches_panel$speech_theta_sent_std[obs] <- standardise(speeches_panel$speech_theta_sent[obs])
  # Pre meeting articles
  speeches_panel$theta_prespeech_std[obs] <- standardise(speeches_panel$theta_prespeech[obs])
  speeches_panel$theta_sent_prespeech_std[obs] <- standardise(speeches_panel$theta_sent_prespeech[obs])
  # Post meeting articles
  speeches_panel$theta_postspeech_std[obs] <- standardise(speeches_panel$theta_postspeech[obs])
  speeches_panel$theta_sent_postspeech_std[obs] <- standardise(speeches_panel$theta_sent_postspeech[obs])
  # Publication window
  speeches_panel$news_speechwindow_std[obs] <- standardise(speeches_panel$news_speechwindow[obs])
  speeches_panel$news_sent_speechwindow_std[obs] <- standardise(speeches_panel$news_sent_speechwindow[obs])
}


"
Some regressions
"
minutes_panel$mins_theta_1lag <- plm::lag(minutes_panel$mins_theta,1)
minutes_panel$mins_theta_2lag <- plm::lag(minutes_panel$mins_theta,2)
minutes_panel$mins_theta_3lag <- plm::lag(minutes_panel$mins_theta,3)
speeches_panel$speech_theta_1lag <- plm::lag(speeches_panel$speech_theta,1)
speeches_panel$speech_theta_2lag <- plm::lag(speeches_panel$speech_theta,2)
speeches_panel$speech_theta_3lag <- plm::lag(speeches_panel$speech_theta,3)

minutes_panel$mins_theta_sent_1lag <- plm::lag(minutes_panel$mins_theta_sent,1)
minutes_panel$mins_theta_sent_2lag <- plm::lag(minutes_panel$mins_theta_sent,2)
minutes_panel$mins_theta_sent_3lag <- plm::lag(minutes_panel$mins_theta_sent,3)
speeches_panel$speech_theta_sent_1lag <- plm::lag(speeches_panel$speech_theta_sent,1)
speeches_panel$speech_theta_sent_2lag <- plm::lag(speeches_panel$speech_theta_sent,2)
speeches_panel$speech_theta_sent_3lag <- plm::lag(speeches_panel$speech_theta_sent,3)

minutes_panel$news_pubwindow_1lag <- plm::lag(minutes_panel$news_pubwindow,1)
minutes_panel$news_pubwindow_2lag <- plm::lag(minutes_panel$news_pubwindow,2)
minutes_panel$news_pubwindow_3lag <- plm::lag(minutes_panel$news_pubwindow,3)
minutes_panel$news_sent_pubwindow_1lag <- plm::lag(minutes_panel$news_sent_pubwindow,1)
minutes_panel$news_sent_pubwindow_2lag <- plm::lag(minutes_panel$news_sent_pubwindow,2)
minutes_panel$news_sent_pubwindow_3lag <- plm::lag(minutes_panel$news_sent_pubwindow,3)

speeches_panel$news_speechwindow_1lag <- plm::lag(speeches_panel$news_speechwindow,1)
speeches_panel$news_speechwindow_2lag <- plm::lag(speeches_panel$news_speechwindow,2)
speeches_panel$news_speechwindow_3lag <- plm::lag(speeches_panel$news_speechwindow,3)
speeches_panel$news_sent_speechwindow_1lag <- plm::lag(speeches_panel$news_sent_speechwindow,1)
speeches_panel$news_sent_speechwindow_2lag <- plm::lag(speeches_panel$news_sent_speechwindow,2)
speeches_panel$news_sent_speechwindow_3lag <- plm::lag(speeches_panel$news_sent_speechwindow,3)


minutes_panel$sentiment_1lag <- plm::lag(minutes_panel$sentiment,1)
minutes_panel$sentiment_2lag <- plm::lag(minutes_panel$sentiment,2)
minutes_panel$sentiment_3lag <- plm::lag(minutes_panel$sentiment,3)

speeches_panel$sentiment_1lag <- plm::lag(speeches_panel$sentiment,1)
speeches_panel$sentiment_2lag <- plm::lag(speeches_panel$sentiment,2)
speeches_panel$sentiment_3lag <- plm::lag(speeches_panel$sentiment,3)

minutes_panel$sentiment_pubwindow <- minutes_panel$sentiment_postpub - minutes_panel$sentiment_prepub
speeches_panel$sentiment_speechwindow <- speeches_panel$sentiment_postspeech - speeches_panel$sentiment_prespeech

speeches_panel$quarter <- floor_date(speeches_panel$date, unit = "quarters")
speeches_agg <- aggregate(speeches_panel[,c("sentiment", "sentiment_prespeech")], by = list(speeches_panel$quarter),
                          FUN = mean)
cor.test(speeches_agg$sentiment, speeches_agg$sentiment_prespeech)


"
Focus
"
## Minutes
# Include past media
reg_formula <- formula(news_pubwindow ~ mins_theta + 
                         theta_prepub + theta_premeet + theta_postmeet 
                       |topic + period)
model1_dk <- felm_DK_se(reg_formula, minutes_panel)
model1_felm <- felm(reg_formula, minutes_panel)
reg_formula_std <- formula(news_pubwindow_std ~ mins_theta_std + 
                         theta_prepub_std + theta_premeet_std + theta_postmeet_std
                       |topic + period)
model1_std <- felm_DK_se(reg_formula_std, minutes_panel)
summary(model1_dk)
# Include topic-specific controls
reg_formula <- formula(news_pubwindow ~ mins_theta + topic*theta_prepub-topic + 
                         topic*theta_premeet-topic + topic*theta_postmeet-topic
                       |topic + period)
model2_dk <- felm_DK_se(reg_formula, minutes_panel)
model2_felm <- felm(reg_formula, minutes_panel)
reg_formula_std <- formula(news_pubwindow_std ~ mins_theta_std + topic*theta_prepub_std-topic + 
                             topic*theta_premeet_std-topic + topic*theta_postmeet_std-topic
                           |topic + period)
model2_std <- felm_DK_se(reg_formula_std, minutes_panel)
summary(model2_std)
# Save coeff
obs <- which(comp_K_df$K ==k)
# Unst 
model_sum <- summary(model1_dk)
comp_K_df$mins_coef[obs] <- model_sum$coefficients["mins_theta","Estimate"]
comp_K_df$mins_se[obs] <- model_sum$coefficients["mins_theta","Std. Error"]
# Topic-specific lags 
model_sum <- summary(model2_dk)
comp_K_df$mins_spec_coef[obs] <- model_sum$coefficients["mins_theta","Estimate"]
comp_K_df$mins_spec_se[obs] <- model_sum$coefficients["mins_theta","Std. Error"]
# Standardised
model_sum <- summary(model1_std)
comp_K_df$mins_std_coef[obs] <- model_sum$coefficients["mins_theta_std","Estimate"]
comp_K_df$mins_std_se[obs] <- model_sum$coefficients["mins_theta_std","Std. Error"]


## Speeches
# Include past media
reg_formula <- formula(news_speechwindow ~ speech_theta + theta_prespeech
               |topic + period)
model3_dk <- felm_DK_se(reg_formula, speeches_panel)
model3_felm <- felm(reg_formula, speeches_panel)
reg_formula_std <- formula(news_speechwindow_std ~ speech_theta_std + theta_prespeech_std
                       |topic + period)
model3_std <- felm_DK_se(reg_formula_std, speeches_panel)
summary(model3_felm)
# Include topic-specific controls
reg_formula <- formula(news_speechwindow ~ speech_theta + topic*theta_prespeech - topic
      | topic + period)
model4_dk <- felm_DK_se(reg_formula, speeches_panel)
model4_felm <- felm(reg_formula, speeches_panel)
reg_formula_std <- formula(news_speechwindow_std ~ speech_theta_std + topic*theta_prespeech_std - topic 
                       | topic + period)
model4_std <- felm_DK_se(reg_formula_std, speeches_panel)
summary(model4_std)
# Save coeff
obs <- which(comp_K_df$K ==k)
model_sum <- summary(model3_dk)
comp_K_df$speech_coef[obs] <- model_sum$coefficients["speech_theta","Estimate"]
comp_K_df$speech_se[obs] <- model_sum$coefficients["speech_theta","Std. Error"]
# Topic-specific lags 
model_sum <- summary(model4_dk)
comp_K_df$speech_spec_coef[obs] <- model_sum$coefficients["speech_theta","Estimate"]
comp_K_df$speech_spec_se[obs] <- model_sum$coefficients["speech_theta","Std. Error"]
# Standardised
model_sum <- summary(model3_std)
comp_K_df$speech_std_coef[obs] <- model_sum$coefficients["speech_theta_std","Estimate"]
comp_K_df$speech_std_se[obs] <- model_sum$coefficients["speech_theta_std","Std. Error"]


"
Tone
"
## Minutes
# Include past media
reg_formula <- formula(news_sent_pubwindow ~ mins_theta_sent + mins_theta + 
                         theta_sent_prepub + theta_sent_premeet + theta_sent_postmeet 
                       |topic + period)
model5_dk <- felm_DK_se(reg_formula, minutes_panel)
model5_felm <- felm(reg_formula, minutes_panel)
reg_formula_std <- formula(news_sent_pubwindow_std ~ mins_theta_sent_std + mins_theta_std + 
                         theta_sent_prepub_std + theta_sent_premeet_std + theta_sent_postmeet_std 
                       |topic + period)
model5_std <- felm_DK_se(reg_formula_std, minutes_panel)
summary(model5_std)
# Include topic-specific controls
reg_formula <- formula(news_sent_pubwindow ~ mins_theta_sent + mins_theta + 
                         topic*theta_sent_prepub-topic + topic*theta_sent_premeet-topic + topic*theta_sent_postmeet-topic
                         | topic + period)
model6_dk <- felm_DK_se(reg_formula, minutes_panel)
model6_felm <- felm(reg_formula, minutes_panel)
reg_formula_std <- formula(news_sent_pubwindow_std ~ mins_theta_sent_std + mins_theta_std + 
                         topic*theta_sent_prepub_std-topic + topic*theta_sent_premeet_std-topic + topic*theta_sent_postmeet_std-topic
                       | topic + period)
model6_std <- felm_DK_se(reg_formula_std, minutes_panel)
summary(model6_std)
# Save coeff
obs <- which(comp_K_df$K ==k)
model_sum <- summary(model5_dk)
comp_K_df$mins_t_coef[obs] <- model_sum$coefficients["mins_theta_sent","Estimate"]
comp_K_df$mins_t_se[obs] <- model_sum$coefficients["mins_theta_sent","Std. Error"]
# Topic-specific lags 
model_sum <- summary(model6_dk)
comp_K_df$mins_t_spec_coef[obs] <- model_sum$coefficients["mins_theta_sent","Estimate"]
comp_K_df$mins_t_spec_se[obs] <- model_sum$coefficients["mins_theta_sent","Std. Error"]
# Standardised
model_sum <- summary(model5_std)
comp_K_df$mins_t_std_coef[obs] <- model_sum$coefficients["mins_theta_sent_std","Estimate"]
comp_K_df$mins_t_std_se[obs] <- model_sum$coefficients["mins_theta_sent_std","Std. Error"]

## Speeches
# Include past media
reg_formula <- formula(news_sent_speechwindow ~ speech_theta_sent + speech_theta + theta_sent_prespeech 
                       |topic + period)
model7_dk <- felm_DK_se(reg_formula, speeches_panel)
model7_felm <- felm(reg_formula, speeches_panel)
reg_formula_std <- formula(news_sent_speechwindow_std ~ speech_theta_sent_std + speech_theta_std + theta_sent_prespeech_std 
                           |topic + period)
model7_std <- felm_DK_se(reg_formula_std, speeches_panel)
summary(model7_dk)
# Include topic-specific controls
reg_formula <- formula(news_sent_speechwindow ~ speech_theta_sent + speech_theta + topic*theta_sent_prespeech-topic 
                       | topic + period)
model8_dk <- felm_DK_se(reg_formula, speeches_panel)
model8_felm <- felm(reg_formula, speeches_panel)
reg_formula_std <- formula(news_sent_speechwindow_std ~ speech_theta_sent_std + speech_theta_std + topic*theta_sent_prespeech_std-topic
                           | topic + period)
model8_std <- felm_DK_se(reg_formula_std, speeches_panel)
summary(model8_std)
# Save coeff
obs <- which(comp_K_df$K ==k)
model_sum <- summary(model7_dk)
comp_K_df$speech_t_coef[obs] <- model_sum$coefficients["speech_theta_sent","Estimate"]
comp_K_df$speech_t_se[obs] <- model_sum$coefficients["speech_theta_sent","Std. Error"]
# Topic-specific lags 
model_sum <- summary(model8_dk)
comp_K_df$speech_t_spec_coef[obs] <- model_sum$coefficients["speech_theta_sent","Estimate"]
comp_K_df$speech_t_spec_se[obs] <- model_sum$coefficients["speech_theta_sent","Std. Error"]
# Standardised
model_sum <- summary(model7_std)
comp_K_df$speech_t_std_coef[obs] <- model_sum$coefficients["speech_theta_sent_std","Estimate"]
comp_K_df$speech_t_std_se[obs] <- model_sum$coefficients["speech_theta_sent_std","Std. Error"]


## Non-topic based sentiment
minutes_overall <- minutes_panel[which(minutes_panel$topic == "T1"),]
speeches_overall <- speeches_panel[which(speeches_panel$topic == "T1"),]

model9 <- lm(sentiment_pubwindow ~ sentiment + sentiment_prepub + sentiment_postmeet + 
               sentiment_premeet, data = minutes_overall)
summary(model9)
model10 <- lm(sentiment_speechwindow ~ sentiment + sentiment_prespeech, data = speeches_overall)
summary(model10)

n <- nrow(minutes_overall)
model <- lm(minutes_overall$sentiment_pubwindow[-1] ~ minutes_overall$sentiment_pubwindow[-n])
summary(model)
n <- nrow(speeches_overall)
model <- lm(speeches_overall$sentiment_speechwindow[-1] ~ speeches_overall$sentiment_speechwindow[-n])
summary(model)


ggplot(minutes_overall) + theme_bw() + 
  geom_line(aes(x = meet_date, y = sentiment_pubwindow)) + 
  geom_line(aes(x = meet_date, y = sentiment), color = "red")


}
beep()
####### End of loop over topics
stargazer(model1_dk, model2_dk, model3_dk, model4_dk,
          table.placement = "H", df = FALSE,
          title = "FOMC effect on focus of media coverage",
          label = "tab:fed_media_focus_effect")
stargazer(model5_dk, model6_dk, model7_dk, model8_dk, model9, model10,
          table.placement = "H", df = FALSE,
          title = "FOMC effect on tone of media coverage",
          label = "tab:fed_media_tone_effect")

stargazer(model2_felm, model2_std, model4_felm, model4_std,
          table.placement = "H", df = FALSE,
          title = "FOMC effect on focus of media coverage robustness",
          label = "tab:fed_media_focus_effect_robust")

stargazer(model6_felm, model8_felm,
          table.placement = "H", df = FALSE,
          title = "FOMC effect on tone of media coverage robustness",
          label = "tab:fed_media_tone_effect_robust")






comp_K_df_long <- melt(comp_K_df, id = c("K"),
                  variable.name = "type", value.name = "value")
comp_K_df_long <- comp_K_df_long[which(comp_K_df_long$type %in% c("mins_spec_coef", "mins_spec_se",
        "mins_t_spec_coef", "mins_t_spec_se", "speech_spec_coef", "speech_spec_se", 
        "speech_t_spec_coef", "speech_t_spec_se")),]
comp_K_df_long$model <- ""
comp_K_df_long$model[which(comp_K_df_long$type %in% c("mins_coef", "mins_se"))] <- "Minutes Focus (non-spec)"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("mins_t_coef", "mins_t_se"))] <- "Minutes Tone (non-spec)"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("speech_coef", "speech_se"))] <- "Speeches Focus (non-spec)"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("speech_t_coef", "speech_t_se"))] <- "Speeches Tone (non-spec)"

comp_K_df_long$model[which(comp_K_df_long$type %in% c("mins_spec_coef", "mins_spec_se"))] <- "Minutes Focus"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("mins_t_spec_coef", "mins_t_spec_se"))] <- "Minutes Tone"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("speech_spec_coef", "speech_spec_se"))] <- "Speeches Focus"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("speech_t_spec_coef", "speech_t_spec_se"))] <- "Speeches Tone"


comp_K_df_long$model[which(comp_K_df_long$type %in% c("mins_std_coef", "mins_std_se"))] <- "Minutes Focus (std)"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("mins_t_std_coef", "mins_t_std_se"))] <- "Minutes Tone (std)"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("speech_std_coef", "speech_std_se"))] <- "Speeches Focus (std)"
comp_K_df_long$model[which(comp_K_df_long$type %in% c("speech_t_std_coef", "speech_t_std_se"))] <- "Speeches Tone (std)"


comp_K_df_long$type <- str_remove_all(comp_K_df_long$type, "[a-z]+_")

comp_K_df_long <- pivot_wider(comp_K_df_long, id_cols = c(K,model),
                              names_from = type, values_from = value)
comp_K_df_long <- data.frame(comp_K_df_long)
comp_K_df_long <- comp_K_df_long[!is.na(comp_K_df_long$se),]

ggplot(comp_K_df_long, aes(y = K)) + theme_bw() + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_ribbon(aes(xmin = coef-1.282*abs(se), xmax = coef+1.282*abs(se)), 
              alpha = 0.5) + 
  geom_ribbon(aes(xmin = coef-1.96*abs(se), xmax = coef+1.96*abs(se)), 
              alpha = 0.2) + 
  geom_point(aes(x = coef)) + 
  #scale_x_continuous(breaks = c(-0.1,-0.05,0.0, 0.05,0.1)) + 
  facet_wrap(model~., nrow = 1) + 
  xlab("FOMC effect on media coverage")
ggsave("figures/fed_media_topics/fed_media_coefs.pdf", width = 8, height = 6)


comp_K_sptone <- comp_K_df_long[which(comp_K_df_long$model == "Speeches Tone"),]

comp_K_sptone$coef - 1.96*abs(comp_K_sptone$se)

############################# End ############################# 