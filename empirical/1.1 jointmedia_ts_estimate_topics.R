####
# This file estimates topics on the Federal Reserve and NYT articles combined
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(tm)
require(slam)
require(topicmodels)
require(seededlda)
require(tidyverse)
require(tidytext)
require(wordcloud)
require(stargazer)
require(usethis)
require(slam)
# usethis::edit_r_environ()


standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}


### Define the directories where raw data is stored and clean will be saved
import_dir <- "data/clean_text/"
export_dir <- "data/topic_data/"
fig_dir <- "figures/fed_media_topics/"


### Import the text data
clean_filename = paste0(import_dir, "fedminutes_clean.csv")
fedminutes_df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

clean_filename = paste0(import_dir, "fedspeeches_clean.csv")
fedspeeches_df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

clean_filename = paste0(import_dir, "NYT_clean_90s.csv")
nyt_df1 <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
clean_filename = paste0(import_dir, "NYT_clean_00s.csv")
nyt_df2 <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
clean_filename = paste0(import_dir, "NYT_clean_10s.csv")
nyt_df3 <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

nyt_df <- rbind(nyt_df1, nyt_df2, nyt_df3)
write.csv(nyt_df[,c("unique_id","date")], "data/clean_text/articles_key.csv", row.names = FALSE)
rm(nyt_df1,nyt_df2,nyt_df3)



### Combine articles into a corpus with minutes, speeches and some articles
total_df <- nyt_df[,c("unique_id", "quarter", "paragraph_clean", "sentiment")]
total_df <- rbind(total_df, fedminutes_df[,c("unique_id", "quarter", "paragraph_clean", "sentiment")])
total_df <- rbind(total_df, fedspeeches_df[,c("unique_id", "quarter", "paragraph_clean", "sentiment")])

total_df <- merge(total_df, nyt_df[,c("unique_id", "quarter", "subsequent_meeting", "recent_meeting")],
                  all.x = TRUE, by = c("unique_id", "quarter"))
total_df <- total_df[order(total_df$unique_id),]


############################# Convert to labelled DTM ############################# 

total_corpus <- Corpus(VectorSource(unlist(total_df[, "paragraph_clean"])))
total_dtm <- DocumentTermMatrix(total_corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of total_dtm are", dim(total_dtm)[1], "documents and", 
            dim(total_dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
total_dtm$dimnames$Docs <- total_df$unique_id
fed_dtm <- total_dtm[str_detect(total_dtm$dimnames$Docs, "FEDp"),]

table(total_dtm$dimnames$Docs == total_df$unique_id)
short_obs <- (((str_detect(total_df$unique_id, "FEDp") |
              (str_detect(total_df$unique_id, "nyt") & !is.na(total_df$subsequent_meeting)))) & 
                total_df$quarter < "2018-01-01")
              
                 #(str_detect(total_df$unique_id, "nyt") & !is.na(total_df$recent_meeting)))
short_dtm <- total_dtm[short_obs,]
table(str_detect(short_dtm$dimnames$Docs, "SPEECH"))

saveRDS(total_dtm, file = paste0(export_dir, "overall/total_dtm.rds"))
saveRDS(short_dtm, file = paste0(export_dir, "overall/short_dtm.rds"))


############################# Estimate the topics on the paragraphs and articles ############################# 

k <- 28
for (k in c(28, 30)){
#paragraph_lda <- LDA(total_dtm, k = k, method = "Gibbs", 
#                     control = list(verbose = 100, burnin = 1000, thin = 100, iter = 20000))
#full_lda_vem <- LDA(total_dtm, k = k, method = "VEM")
rm(total_corpus, fed_dtm)
set.seed(1234)
short_lda_gibbs <- LDA(short_dtm, k = k, method = "Gibbs",
                     control = list(verbose = 1000, burnin = 2000, thin = 10, iter = 10000))
#full_lda_gibbs <- LDA(total_dtm, k = k, method = "Gibbs",
#                       control = list(verbose = 100, burnin = 1000, thin = 50, iter = 20000))


# paragraph.lda <- LDA(paragraph.dtm, k = k, control = list( verbose = 1))
paragraph_lda <- short_lda_gibbs

saveRDS(paragraph_lda, file = paste0(export_dir, "overall/short_lda_k",k,".rds"))
#saveRDS(full_lda_vem, file = paste0(export_dir, "overall/full_lda.rds"))
paragraph_lda <- readRDS(file = paste0(export_dir, "overall/short_lda_k",k,".rds"))
#paragraph_lda <- readRDS(file = paste0(export_dir, "overall/full_lda_k",k,".rds"))
 
### Store the topic beta vectors
paragraph_topics <- tidy(paragraph_lda, matrix = "beta")
paragraph_topics


# Write the topic vectors to file
export_filename = paste0(export_dir, "overall/short_paragraph_topics_k",k,".csv")
write.csv(paragraph_topics, export_filename, fileEncoding = "utf-8", row.names = FALSE)
#paragraph_topics <- read.csv(export_filename, stringsAsFactors = FALSE)

# Calculate FREX scores (Bischof and Airoldi, 2012)
topics_desc <- pivot_wider(paragraph_topics, id_cols = c(term), names_from = topic, 
                           names_glue = "beta{topic}", values_from = beta)
topics_desc <- data.frame(topics_desc[order(topics_desc$term),])

topicnames <- paste0("beta", 1:k)
w <- 0.3
for (kk in 1:k){
  E_temp <- topics_desc[,paste0("beta",kk)]/row_sums(topics_desc[,topicnames])
  
  topics_desc[,paste0("FREX",kk)] <- (w/topics_desc[,paste0("beta",kk)] + 
                                        (1-w)/E_temp)^(-1)
}




# Identify the top ten terms for each topic
top_terms <- paragraph_topics %>%
  group_by(topic) %>%
  top_n(20,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot these top ten terms for each topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
ggsave(paste0(fig_dir, "short_all_topics_k",k,".pdf"), width = 12, height = 9)


############################# Separate out again ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
paragraph_posterior <- posterior(paragraph_lda, newdata = total_dtm)

# Extract the article topics with unique_id
individual_topics <- as.data.frame(paragraph_posterior$topic)
colnames(individual_topics) <- paste0("T", colnames(paragraph_posterior$topics))
individual_topics[,paste0("T", 1:k, "_sent")] <- individual_topics[,paste0("T", 1:k)]
individual_topics$unique_id <- rownames(individual_topics)


### Split back into NYT, speeches and minutes 
meetingtopics <- individual_topics[which(str_detect(individual_topics$unique_id, "FEDp")),]
export_filename = paste0(export_dir, "overall/meeting_topics_k",k,".csv")
write.csv(meetingtopics, export_filename, fileEncoding = "utf-8", row.names = FALSE)

speechtopics <- individual_topics[which(str_detect(individual_topics$unique_id, "SPEECHp")),]
export_filename = paste0(export_dir, "overall/speech_topics_k",k,".csv")
write.csv(speechtopics, export_filename, fileEncoding = "utf-8", row.names = FALSE)

articletopics <- individual_topics[which(str_detect(individual_topics$unique_id, "nyt")),]
export_filename = paste0(export_dir, "overall/article_topics_k",k,".csv")
write.csv(articletopics, export_filename, fileEncoding = "utf-8", row.names = FALSE)


### Summary table

topic_summary_df <- data.frame(Topic = paste("Topic", 1:k), Description = " ", Top.5.Words = "", 
                               mins = NA, speech = NA, nyt = NA)

for (kk in 1:k){
  topic_df <- data.frame(top_terms[which(top_terms$topic == kk),])
  topic_summary_df$Top.5.Words[kk] <- paste(topic_df$term[1:12], collapse = ", ")
  
  topic_summary_df$mins[kk] <- round(mean(meetingtopics[,paste0("T",kk)]),4)
  topic_summary_df$speech[kk] <- round(mean(speechtopics[,paste0("T",kk)]),4)
  topic_summary_df$nyt[kk] <- round(mean(articletopics[,paste0("T",kk)]),4)
  
}

stargazer(as.matrix(topic_summary_df), table.placement = "H")
export_filename = paste0(export_dir, "short_topics_summary_k",k,".csv")
write.csv(topic_summary_df, export_filename, fileEncoding = "utf-8", row.names = FALSE)












############################# Find the means of the meeting paragraphs ############################# 

meetingtopics <- merge(fedminutes_df[,c("unique_id", "meeting_id", "meet_date","pub_date", 
                                        "quarter", "sentiment", "wordcount")], 
                       individual_topics[which(str_detect(individual_topics$unique_id, "FEDp")),], 
                       by = "unique_id")
meetingtopics$sentiment <- standardise(meetingtopics$sentiment)
sent_topics <- which(str_detect(names(meetingtopics), "_sent"))
meetingtopics[,sent_topics] <- meetingtopics[,sent_topics]*meetingtopics$sentiment


# Quarterly time series
meetinglevel_qly <- meetingtopics %>%
  select(-c(unique_id, sentiment, meeting_id, meet_date, pub_date, wordcount)) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))
meetinglevel_qly <- meetinglevel_qly[order(meetinglevel_qly$quarter),]
# Export
write.csv(meetinglevel_qly, paste0(export_dir, "minutes_qly_k",k,".csv"), fileEncoding = "utf-8", row.names = FALSE)



# Meeting level time series
meetinglevel_means <- meetingtopics %>%
  select(-c(unique_id, sentiment)) %>%
  group_by(meeting_id,meet_date,pub_date, quarter) %>%
  summarise_all(list(mean))
meetinglevel_means <- meetinglevel_means[order(meetinglevel_means$meet_date),]
# Export
write.csv(meetinglevel_means, paste0(export_dir, "minutes_event_k",k,".csv"), fileEncoding = "utf-8", row.names = FALSE)


ggplot(meetinglevel_qly) + theme_bw() + 
  geom_line(aes(x = as.Date(quarter), y = T3))

############################# Find the means of the speech paragraphs ############################# 

speechtopics <- merge(fedspeeches_df[,c("unique_id", "speech_id", "date", 
                                        "quarter", "sentiment", "wordcount")], 
                       individual_topics[which(str_detect(individual_topics$unique_id, "SPEECHp")),], 
                       by = "unique_id")
speechtopics$sentiment <- standardise(speechtopics$sentiment)
sent_topics <- which(str_detect(names(speechtopics), "_sent"))
speechtopics[,sent_topics] <- speechtopics[,sent_topics]*speechtopics$sentiment

# Quarterly time series
speechlevel_qly <- speechtopics %>%
  select(-c(unique_id, sentiment, speech_id, date, wordcount)) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))
speechlevel_qly <- speechlevel_qly[order(speechlevel_qly$quarter),]
# Export
write.csv(speechlevel_qly, paste0(export_dir, "speeches_qly_k",k,".csv"), fileEncoding = "utf-8", row.names = FALSE)


# Speech level time series
speechlevel_means <- speechtopics %>%
  select(-c(unique_id, sentiment)) %>%
  group_by(speech_id, date, quarter) %>%
  summarise_all(list(mean))
speechlevel_means <- speechlevel_means[order(speechlevel_means$date),]
# Export
write.csv(speechlevel_means, paste0(export_dir, "speeches_event_k",k,".csv"), fileEncoding = "utf-8", row.names = FALSE)


ggplot(speechlevel_qly) + theme_bw() + 
  geom_line(aes(x = as.Date(quarter), y = T24))

############################# Find the means of the weekly newspaper ############################# 

# Create a data-frame with the key info for each meeting
articletopics <- merge(nyt_df[,c("unique_id", "date", "quarter", "subsequent_meeting",
                                 "recent_meeting", "subsequent_pub", "recent_pub", "subsequent_speech",
                                 "recent_speech", "sentiment", "wordcount")], 
                       individual_topics[which(str_detect(individual_topics$unique_id, "nyt")),], 
                       by = "unique_id")
articletopics$sentiment <- standardise(articletopics$sentiment)
sent_topics <- which(str_detect(names(articletopics), "_sent"))
articletopics[,sent_topics] <- articletopics[,sent_topics]*articletopics$sentiment

# Add a comma to the end of the subsequent and recent column to allow identification of multiple events
articletopics$subsequent_meeting <- paste0(articletopics$subsequent_meeting, ",")
articletopics$recent_meeting <- paste0(articletopics$recent_meeting, ",")
articletopics$subsequent_pub <- paste0(articletopics$subsequent_pub, ",")
articletopics$recent_pub <- paste0(articletopics$recent_pub, ",")
articletopics$subsequent_speech <- paste0(articletopics$subsequent_speech, ",")
articletopics$recent_speech <- paste0(articletopics$recent_speech, ",")

# Aggregate by quarter
article_qly <- articletopics %>%
  select(-c(unique_id, date, subsequent_meeting,recent_meeting, subsequent_pub, recent_pub, 
            subsequent_speech, recent_speech, sentiment)) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))
article_qly <- article_qly[order(article_qly$quarter),]
# Export
write.csv(article_qly, paste0(export_dir, "articles_qly_k",k,".csv"), fileEncoding = "utf-8", row.names = FALSE)


ggplot(article_qly) + theme_bw() + 
  geom_line(aes(x = as.Date(quarter), y = T21))


# Quite an involved aggregation to get the meeting/speech level topics
article_events <- data.frame(event = unique(c(fedminutes_df$meeting_id, fedspeeches_df$speech_id)))
topic_cols <- which(names(articletopics) %in% paste0("T", 1:k))

article_events[, paste0("pre_meet_", "T", 1:k)] <- NA
article_events[, paste0("post_meet_", "T", 1:k)] <- NA
article_events[, paste0("pre_pub_", "T", 1:k)] <- NA
article_events[, paste0("post_pub_", "T", 1:k)] <- NA

pb = txtProgressBar(min = 1, max = nrow(article_events), initial = 1) 
for (ii in 1:nrow(article_events)){
  event = paste0(article_events$event[ii], ",")
  
  if (str_detect(event, "FED")){
    pre_meet_topics <- articletopics[which(str_detect(articletopics$subsequent_meeting, event)),topic_cols]
    article_events[ii,paste0("pre_meet_", "T", 1:k)] <- colMeans(pre_meet_topics)
    
    post_meet_topics <- articletopics[which(str_detect(articletopics$recent_meeting, event)),topic_cols]
    article_events[ii,paste0("post_meet_", "T", 1:k)] <- colMeans(post_meet_topics)
    
    pre_pub_topics <- articletopics[which(str_detect(articletopics$subsequent_pub, event)),topic_cols]
    article_events[ii,paste0("pre_pub_", "T", 1:k)] <- colMeans(pre_pub_topics)
    
    post_pub_topics <- articletopics[which(str_detect(articletopics$recent_pub, event)),topic_cols]
    article_events[ii,paste0("post_pub_", "T", 1:k)] <- colMeans(post_pub_topics)
  }
  if (str_detect(event, "SPEECH")){
    pre_pub_topics <- articletopics[which(str_detect(articletopics$subsequent_speech, event)),topic_cols]
    article_events[ii,paste0("pre_pub_", "T", 1:k)] <- colMeans(pre_pub_topics)
    
    post_pub_topics <- articletopics[which(str_detect(articletopics$recent_speech, event)),topic_cols]
    article_events[ii,paste0("post_pub_", "T", 1:k)] <- colMeans(post_pub_topics)
  }
  setTxtProgressBar(pb,ii)
}

write.csv(article_events, paste0(export_dir, "articles_event_k",k,".csv"), fileEncoding = "utf-8", row.names = FALSE)


}

############################# End ############################# 