####
# This file estimates panel regressions on the Fed minutes topics and queried NYT articles
####


setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(dplyr)
require(tidyverse)
require(plm)
require(lfe)


### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"

# Import the article data estimated at weekly level
import_filename = paste(clean_dir, "CBC/combinedarticle_topics.csv", sep = "/")
combinedarticle.df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import the weekly article data, averaged over articles
import_filename = paste(clean_dir, "CBC/article_meantopics", sep = "/")
articlelevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


# Import the minutes data estimated at the meeting level
import_filename = paste(clean_dir, "CBC/fedmeeting_topics.csv", sep = "/")
meeting.df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df$pub_date <- as.Date(meeting.df$pub_date)

ggplot(meeting.df, aes(x = meet_date)) + geom_line(aes(y = y1))



############################# Convert into a panel ############################# 

### Convert the Fed meetings into a panel
# Set which variable we will use (to "y", "T" or "lT")
var_used <- "T"

# Toggle the topic number (might need to adjust)
k <- 10
variablenames <- paste0(var_used, 1:k)
meeting.df <- meeting.df[,c("meeting_id", "meet_date", "pub_date", variablenames)]

# convert to long from for topic-meeting combinations
command <- paste0("meeting_long <- gather(meeting.df, topic, meeting_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))
#meeting_long <- gather(data_short, topic, var_value, y1:y30, factor_key=TRUE)
meeting_long <- meeting_long[order(meeting_long$meeting_id),]



### First run with the mean over articles 
pre_articles <- articlelevel.means[which(!is.na(articlelevel.means$subsequent_meeting)),]
pre_articles$meeting_id <- pre_articles$subsequent_meeting
pre_articles <- pre_articles[,c("meeting_id", variablenames)]
command <- paste0("pre_articles_long <- gather(pre_articles, topic, prearticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))

post_articles <- articlelevel.means[which(!is.na(articlelevel.means$recent_meeting)),]
post_articles$meeting_id <- post_articles$recent_meeting
post_articles <- post_articles[,c("meeting_id", variablenames)]
command <- paste0("post_articles_long <- gather(post_articles, topic, postarticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))



full_panel.df <- merge(meeting_long, pre_articles_long, by = c("meeting_id", "topic"))
full_panel.df <- merge(full_panel.df, post_articles_long, by = c("meeting_id", "topic"))

full_panel.df <- pdata.frame(full_panel.df, index = c("topic", "meet_date"))

# Do media articles predict the minutes?
model <- felm(meeting_value ~ prearticle_value, data = full_panel.df)
summary(model)
model1 <- felm(meeting_value ~ prearticle_value | topic, data = full_panel.df)
summary(model1)
model2 <- felm(meeting_value ~ prearticle_value + postarticle_value | topic, data = full_panel.df)
summary(model2)
model3 <- felm(meeting_value ~ prearticle_value + plm::lag(meeting_value, 1:3) | topic, data = full_panel.df)
summary(model3)
model4 <- felm(meeting_value ~ prearticle_value + postarticle_value + plm::lag(meeting_value, 1:3) | topic, data = full_panel.df)
summary(model4)

# Quick test for autocorrelated residuals
res = model3$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)

# Do the minutes predict media articles?
model <- felm(postarticle_value ~ meeting_value, data = full_panel.df)
summary(model)
model5 <- felm(postarticle_value ~ meeting_value | topic, data = full_panel.df)
summary(model5)
model6 <- felm(postarticle_value ~ meeting_value + prearticle_value | topic, data = full_panel.df)
summary(model6)
model7 <- felm(postarticle_value ~ meeting_value + plm::lag(postarticle_value, 1:3) | topic, data = full_panel.df)
summary(model7)
model8 <- felm(postarticle_value ~ meeting_value + prearticle_value + plm::lag(postarticle_value, 1:3) | topic, data = full_panel.df)
summary(model8)

# Quick test for autocorrelated residuals
res = model7$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)



## Print results
stargazer(model1, model2, model3, model4, model5, model6, model7, model8,
          table.placement = "H", df = FALSE,
          title = "Topic proportion results, averaging across articles")


















### Now with the proportions estimated over the whole week together
pre_articles <- combinedarticle.df[which(!is.na(combinedarticle.df$subsequent_meeting)),]
pre_articles$meeting_id <- pre_articles$subsequent_meeting
pre_articles <- pre_articles[,c("meeting_id", variablenames)]
command <- paste0("pre_articles_long <- gather(pre_articles, topic, prearticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))

post_articles <- combinedarticle.df[which(!is.na(combinedarticle.df$recent_meeting)),]
post_articles$meeting_id <- post_articles$recent_meeting
post_articles <- post_articles[,c("meeting_id", variablenames)]
command <- paste0("post_articles_long <- gather(post_articles, topic, postarticle_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))



full_panel.df <- merge(meeting_long, pre_articles_long, by = c("meeting_id", "topic"))
full_panel.df <- merge(full_panel.df, post_articles_long, by = c("meeting_id", "topic"))

full_panel.df <- pdata.frame(full_panel.df, index = c("topic", "meet_date"))


# Do media articles predict the minutes?
model <- felm(meeting_value ~ prearticle_value, data = full_panel.df)
summary(model)
model1 <- felm(meeting_value ~ prearticle_value | topic, data = full_panel.df)
summary(model1)
model2 <- felm(meeting_value ~ prearticle_value + postarticle_value | topic, data = full_panel.df)
summary(model2)
model3 <- felm(meeting_value ~ prearticle_value + plm::lag(meeting_value, 1:3) | topic, data = full_panel.df)
summary(model3)
model4 <- felm(meeting_value ~ prearticle_value + postarticle_value + plm::lag(meeting_value, 1:3) | topic, data = full_panel.df)
summary(model4)

# Quick test for autocorrelated residuals
res = model3$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)

# Do the minutes predict media articles?
model <- felm(postarticle_value ~ meeting_value, data = full_panel.df)
summary(model)
model5 <- felm(postarticle_value ~ meeting_value | topic, data = full_panel.df)
summary(model5)
model6 <- felm(postarticle_value ~ meeting_value + prearticle_value | topic, data = full_panel.df)
summary(model6)
model7 <- felm(postarticle_value ~ meeting_value + plm::lag(postarticle_value, 1:3) | topic, data = full_panel.df)
summary(model7)
model8 <- felm(postarticle_value ~ meeting_value + prearticle_value + plm::lag(postarticle_value, 1:3) | topic, data = full_panel.df)
summary(model8)

# Quick test for autocorrelated residuals
res = model7$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)



## Print results
stargazer(model1, model2, model3, model4, model5, model6, model7, model8,
          table.placement = "H", df = FALSE,
          title = "Topic proportion results, estimated on combined articles")





model <- felm(postarticle_value ~ meeting_value, data = full_panel.df)
summary(model)
model1 <- felm(postarticle_value ~ meeting_value | topic, data = full_panel.df)
summary(model1)
model2 <- felm(postarticle_value ~ prearticle_value | topic, data = full_panel.df)
summary(model2)
model3 <- felm(postarticle_value ~ meeting_value + prearticle_value | topic, data = full_panel.df)
summary(model3)
model4 <- felm(postarticle_value ~ meeting_value + plm::lag(postarticle_value, 1:8) | topic, data = full_panel.df)
summary(model4)
model5 <- felm(postarticle_value ~ meeting_value + prearticle_value + plm::lag(postarticle_value, 1:8) | topic, data = full_panel.df)
summary(model5)


