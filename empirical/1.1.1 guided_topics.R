setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(quanteda)
require(seededlda)
require(stringr)
require(dplyr)
require(ggplot2)




standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}





dict <- dictionary(file = "tests/data/topics.yml")
print(dict)

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
rm(nyt_df1,nyt_df2,nyt_df3)


### Combine articles into a corpus with minutes, speeches and some articles
total_df <- nyt_df[,c("unique_id", "paragraph_clean", "sentiment")]
total_df <- rbind(total_df, fedminutes_df[,c("unique_id", "paragraph_clean", "sentiment")])
total_df <- rbind(total_df, fedspeeches_df[,c("unique_id", "paragraph_clean", "sentiment")])

total_df <- merge(total_df, nyt_df[,c("unique_id", "subsequent_meeting", "recent_meeting")],
                  all.x = TRUE)
total_df <- total_df[order(total_df$unique_id),]

rownames(total_df) <- total_df$unique_id
corp_mins <- corpus(total_df, text_field = "paragraph_clean")
toks_mins <- tokens(corp_mins, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
dfm_mins <- dfm(toks_mins)


short_obs <- (str_detect(total_df$unique_id, "FEDp") |
                (str_detect(total_df$unique_id, "nyt") & !is.na(total_df$subsequent_meeting)))# | 
dfm_short <- dfm_mins[short_obs,]
#(str_detect(total_df$unique_id, "nyt") & !is.na(total_df$recent_meeting)))


total_tf <- data.frame(colnames(dfm_mins), tf = colSums(dfm_mins))


dict_mins <- dictionary(list(inflation = c("price", "inflat", "oil", "food", "energi"),
                        growth = c("economi", "growth",  "gdp", "grow", "recess", "recoveri"),
                        employment = c("job", "worker", "unemploy", "employ", "labor"),
                        rates = c("interest", "bond", "fed", "rate", "yield"),
                        profits = c("profit", "compani", "busi"),
                        indprod = c("industri", "product", "manufactur", "factori", "plant", "car"),
                        housing = c("mortgage", "house", "hous", "home", "residenti"),
                        nresinv = c("nonresidenti","invest", "investor", "capit","expenditur"),
                        consum = c("retail", "consum", "household", "sale"),
                        fedgov = c("tax", "budget", "govern"),
                        equities = c("stock", "index", "equiti"),
                        policy = c("committe", "polici", "monetari", "member")
                        ))

k <- 20

nresidual <- k - length(dict_mins)

set.seed(1234)
slda_short <- textmodel_seededlda(dfm_short, dict_mins, residual = nresidual, max_iter = 6000)
print(terms(slda, 20))
fedminutes_df$topic <- topics(slda_short)[which(str_detect(names(topics(slda_short)) , "FED"))]

slda_all <- textmodel_lda(dfm_mins, model = slda_short)
print(terms(slda_all, 10))
table(topics(slda_all))
saveRDS(slda_all, file = paste0(export_dir, "overall/guided_lda_k",k,".rds"))
#slda_all <- readRDS(file = paste0(export_dir, "overall/guided_lda_k",k,".rds"))


paragraph_beta <- data.frame(t(slda_all$phi))
paragraph_beta$term <- rownames(paragraph_beta)
paragraph_theta <- data.frame((slda_all$theta))
paragraph_theta$unique_id <- rownames(paragraph_theta)



### Split back into NYT, speeches and minutes 
meetingtopics <- paragraph_theta[which(str_detect(paragraph_theta$unique_id, "FEDp")),]
speechtopics <- paragraph_theta[which(str_detect(paragraph_theta$unique_id, "SPEECHp")),]
articletopics <- paragraph_theta[which(str_detect(paragraph_theta$unique_id, "nyt")),]


### Merge back into the metadata
meetingtopics <- merge(fedminutes_df[,c("unique_id", "meeting_id", "meet_date","pub_date", 
                                        "quarter", "wordcount", "sentiment")], 
                       meetingtopics,by = "unique_id")
speechtopics <- merge(fedspeeches_df[,c("unique_id", "speech_id", "date", 
                                       "quarter", "wordcount", "sentiment")], 
                      speechtopics,by = "unique_id")
articletopics <- merge(nyt_df[,c("unique_id", "date", "quarter", "subsequent_meeting",
                                 "recent_meeting", "subsequent_pub", "recent_pub", "subsequent_speech",
                                 "recent_speech", "wordcount", "sentiment")], 
                       articletopics,by = "unique_id")



### Aggregate to quarterly time series
# Minutes
meetinglevel_qly <- meetingtopics %>%
  select(-c(unique_id, sentiment, meeting_id, meet_date, pub_date)) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))
meetinglevel_qly <- meetinglevel_qly[order(meetinglevel_qly$quarter),]

ggplot(meetinglevel_qly) + theme_bw() + 
  geom_line(aes(x = as.Date(quarter), y = inflation))
# Speeches
speechlevel_qly <- speechtopics %>%
  select(-c(unique_id, sentiment, speech_id, date)) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))
speechlevel_qly <- speechlevel_qly[order(speechlevel_qly$quarter),]

ggplot(speechlevel_qly) + theme_bw() + 
  geom_line(aes(x = as.Date(quarter), y = inflation))
# Articles
article_qly <- articletopics %>%
  select(-c(unique_id, date, subsequent_meeting,recent_meeting, subsequent_pub, recent_pub, 
            subsequent_speech, recent_speech, sentiment)) %>%
  group_by(quarter) %>%
  summarise_all(funs(mean))
article_qly <- article_qly[order(article_qly$quarter),]

ggplot(article_qly) + theme_bw() + 
  geom_line(aes(x = as.Date(quarter), y = inflation))


### Export
write.csv(meetinglevel_qly, paste0(export_dir, "articles_guid_k",k,"_qly.csv"), row.names = FALSE)
write.csv(speechlevel_qly, paste0(export_dir, "articles_guid_k",k,"_qly.csv"), row.names = FALSE)
write.csv(article_qly, paste0(export_dir, "articles_guid_k",k,"_qly.csv"), row.names = FALSE)



"
End of script
"