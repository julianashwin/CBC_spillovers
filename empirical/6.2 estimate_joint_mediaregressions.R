####
# This file estimates panel regressions on the Fed minutes NYT articles combined/joint topics.
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
require(stargazer)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


# Import the weekly article data, averaged over articles
import_filename = paste(clean_dir, "CBC/articlemeans_jointtopics_long.csv", sep = "/")
articlelevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


# Import the minutes data estimated at the meeting level
# Import the text data
clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date)
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date)
meeting.key <- unique(fedminutes.df[,c("meeting_id", "pub_date", "meet_date")])

import_filename = paste(clean_dir, "CBC/fedmeetingmeans_jointtopics_long.csv", sep = "/")
meetinglevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


meeting.df <- merge(meetinglevel.means, meeting.key, by = "meeting_id", all.x = TRUE)


meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df$pub_date <- as.Date(meeting.df$pub_date)

ggplot(meeting.df, aes(x = meet_date)) + geom_line(aes(y = T2))



############################# Convert into a panel ############################# 

### Convert the Fed meetings into a panel
# Set which variable we will use (to "y", "T" or "lT")
var_used <- "T"

# Toggle the topic number (might need to adjust)
k <- 30
variablenames <- paste0(var_used, 1:k)
meeting.df <- meeting.df[,c("meeting_id", "meet_date", "pub_date", variablenames)]

# convert to long from for topic-meeting combinations
command <- paste0("meeting_long <- gather(meeting.df, topic, meeting_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))
#meeting_long <- gather(data_short, topic, var_value, y1:y30, factor_key=TRUE)
meeting_long <- meeting_long[order(meeting_long$meeting_id),]

### First run with the mean over articles 
premeet_articles <- articlelevel.means[which(!is.na(articlelevel.means$subsequent_meeting)),]
premeet_articles$meeting_id <- premeet_articles$subsequent_meeting
premeet_articles <- premeet_articles[,c("meeting_id", variablenames)]
command <- paste0("premeet_articles_long <- gather(premeet_articles, topic, premeetarticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))

postmeet_articles <- articlelevel.means[which(!is.na(articlelevel.means$recent_meeting)),]
postmeet_articles$meeting_id <- postmeet_articles$recent_meeting
postmeet_articles <- postmeet_articles[,c("meeting_id", variablenames)]
command <- paste0("postmeet_articles_long <- gather(postmeet_articles, topic, postmeetarticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))

prepub_articles <- articlelevel.means[which(!is.na(articlelevel.means$subsequent_pub)),]
prepub_articles$meeting_id <- prepub_articles$subsequent_pub
prepub_articles <- prepub_articles[,c("meeting_id", variablenames)]
command <- paste0("prepub_articles_long <- gather(prepub_articles, topic, prepubarticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))

postpub_articles <- articlelevel.means[which(!is.na(articlelevel.means$recent_pub)),]
postpub_articles$meeting_id <- postpub_articles$recent_pub
postpub_articles <- postpub_articles[,c("meeting_id", variablenames)]
command <- paste0("postpub_articles_long <- gather(postpub_articles, topic, postpubarticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))



### Merge the media and minutes data
full_panel.df <- merge(meeting_long, premeet_articles_long, by = c("meeting_id", "topic"))
full_panel.df <- merge(full_panel.df, postmeet_articles_long, by = c("meeting_id", "topic"))
full_panel.df <- merge(full_panel.df, prepub_articles_long, by = c("meeting_id", "topic"))
full_panel.df <- merge(full_panel.df, postpub_articles_long, by = c("meeting_id", "topic"))


full_panel.df$period <- as.numeric(as.factor(full_panel.df$meet_date))

full_panel.df <- pdata.frame(data.frame(full_panel.df), index = c("topic", "period"))


clean_filename = paste(clean_dir, "CBC/mediaminutes_panel_long.csv", sep = "/")
write.csv(full_panel.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)



### Standardise the variables
colnames(full_panel.df)
aggregate.panel <- select(full_panel.df, -meet_date, -pub_date, -period, -meeting_id)
colnames(aggregate.panel)

aggregate.panel <- aggregate.panel %>%
  group_by(topic) %>% 
  summarise_all(funs(mean, sd), na.rm = TRUE)


stand_panel.df <- merge(full_panel.df, aggregate.panel, by = "topic")

stand_panel.df$meeting_value_std <- (stand_panel.df$meeting_value - 
                                       stand_panel.df$meeting_value_mean)/stand_panel.df$meeting_value_sd
stand_panel.df$premeetarticle_value_std <- (stand_panel.df$premeetarticle_value - 
                                              stand_panel.df$premeetarticle_value_mean)/stand_panel.df$premeetarticle_value_sd
stand_panel.df$postmeetarticle_value_std <- (stand_panel.df$postmeetarticle_value - 
                                               stand_panel.df$postmeetarticle_value_mean)/stand_panel.df$postmeetarticle_value_sd
stand_panel.df$prepubarticle_value_std <- (stand_panel.df$prepubarticle_value - 
                                             stand_panel.df$prepubarticle_value_mean)/stand_panel.df$prepubarticle_value_sd
stand_panel.df$postpubarticle_value_std <- (stand_panel.df$postpubarticle_value - 
                                              stand_panel.df$postpubarticle_value_mean)/stand_panel.df$postpubarticle_value_sd


clean_filename = paste(clean_dir, "CBC/mediaminutes_panel_long_std.csv", sep = "/")
write.csv(stand_panel.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)


stand_panel.df <- pdata.frame(data.frame(stand_panel.df), index = c("topic", "period"))


# Do media articles predict the minutes?
model1 <- felm(meeting_value ~ premeetarticle_value | topic, data = full_panel.df)
summary(model1)
model1_std <- felm(meeting_value_std ~ premeetarticle_value_std | topic, data = stand_panel.df)
summary(model1_std)
model2 <- felm(meeting_value ~ plm::lag(premeetarticle_value, 0:3) + 
                 plm::lag(meeting_value, 1:3) | topic + period, data = full_panel.df)
summary(model2)
model2_std <- felm(meeting_value_std ~ plm::lag(premeetarticle_value_std, 0:3) + 
                     plm::lag(meeting_value_std, 1:3) | topic + period, data = stand_panel.df)
summary(model2_std)

# Quick test for autocorrelated residuals
res = model2_std$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)

# Do the minutes predict media articles?
model3 <- felm(postpubarticle_value - prepubarticle_value ~ meeting_value| topic, data = full_panel.df)
summary(model3)
model3_std <- felm(postpubarticle_value_std - prepubarticle_value_std ~ meeting_value_std | topic, data = stand_panel.df)
summary(model3_std)

model4 <- felm(postpubarticle_value - prepubarticle_value ~ plm::lag(meeting_value,0:1) +
                 + plm::lag(prepubarticle_value,0) + plm::lag(postmeetarticle_value,0) + 
                 plm::lag(premeetarticle_value,0) + 
                 plm::lag(postpubarticle_value - prepubarticle_value,1:3)| topic + period, data = full_panel.df)
summary(model4)
model4_std <- felm(postpubarticle_value_std - prepubarticle_value_std ~ plm::lag(meeting_value_std,0:1) +
                   + plm::lag(prepubarticle_value_std,0) + plm::lag(postmeetarticle_value_std,0) + 
                     plm::lag(premeetarticle_value_std,0) + 
                     plm::lag(postpubarticle_value_std - prepubarticle_value_std,1:3)| topic + period, data = stand_panel.df)
summary(model4_std)

model5 <- felm(postpubarticle_value ~ meeting_value + prepubarticle_value| topic, data = full_panel.df)
summary(model3)
model5_std <- felm(postpubarticle_value_std  ~ plm::lag(meeting_value_std,0) +
                     + plm::lag(prepubarticle_value_std,0)| topic, data = stand_panel.df)
summary(model5_std)
model6 <- felm(postpubarticle_value ~ plm::lag(meeting_value,0:1) +
                 + plm::lag(prepubarticle_value,0) + plm::lag(postmeetarticle_value,0) + 
                 plm::lag(premeetarticle_value,0)+ plm::lag(postpubarticle_value, 1:3)|
                 topic + period, data = full_panel.df)
summary(model6)
model6_std <- felm(postpubarticle_value_std ~ plm::lag(meeting_value_std,0:1) +
                     + plm::lag(prepubarticle_value_std,0) + plm::lag(postmeetarticle_value_std,0) + 
                     plm::lag(premeetarticle_value_std,0) + plm::lag(postpubarticle_value_std, 1:3)|
                     topic + period, data = stand_panel.df)
summary(model6_std)


# Quick test for autocorrelated residuals
res = model4_std$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)



## Print results
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "NYT and FOMC results (unstandardised data)")

stargazer(model1_std, model2_std, model3_std, model4_std, model5_std, model6_std,
          table.placement = "H", df = FALSE,
          title = "NYT and FOMC results")


############################# End ############################# 

