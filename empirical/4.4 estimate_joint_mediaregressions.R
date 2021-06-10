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
import_filename = paste(clean_dir, "CBC/articlemeans_jointtopics.csv", sep = "/")
articlelevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


# Import the minutes data estimated at the meeting level
# Import the text data
clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date)
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date)
meeting.key <- unique(fedminutes.df[,c("meeting_id", "pub_date", "meet_date")])

import_filename = paste(clean_dir, "CBC/fedmeetingmeans_jointtopics.csv", sep = "/")
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


### Merge the media and minutes data
full_panel.df <- merge(meeting_long, pre_articles_long, by = c("meeting_id", "topic"))
full_panel.df <- merge(full_panel.df, post_articles_long, by = c("meeting_id", "topic"))

full_panel.df$period <- as.numeric(as.factor(full_panel.df$meet_date))

full_panel.df <- pdata.frame(data.frame(full_panel.df), index = c("topic", "period"))


clean_filename = paste(clean_dir, "CBC/mediaminutes_panel.csv", sep = "/")
write.csv(full_panel.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)





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



############################# Principal components ############################# 

variablenames <- paste0("T", 1:30)


### Fed

# Principal components of each meeting's topic attention variables
fed.pca <- prcomp(meeting.df[,variablenames], center = TRUE, scale. = TRUE)
summary(fed.pca)
fed.pca.df <- as.data.frame(fed.pca$x)
fed.pca.df$meeting_id <- meeting.df$meeting_id
meeting.df <- merge(meeting.df, fed.pca.df, by = "meeting_id")
rownames(meeting.df) <- meeting.df$meeting_id

# Aggregate the topic variables to quarterly
meeting.df$quarter <- floor_date(meeting.df$meet_date, unit = "quarter")
fed.quartave <- aggregate(meeting.df[,c(variablenames)], list(meeting.df$quarter), mean)
colnames(fed.quartave) <- c("quarter", paste0(variablenames, "q"))
rownames(fed.quartave) <- fed.quartave$quarter

merged <- merge(meeting.df, fed.quartave, by = "quarter", all.x = TRUE)
cor.test(merged$T1, merged$T1q)
cor.test(merged$T10, merged$T10q)

# Calculate principal components of the monthly averages
fed.pca.quart <- prcomp(fed.quartave[,paste0("T", 1:30, "q")], center = TRUE, scale. = TRUE)
summary(fed.pca.quart)
fed.pca.quart.df <- as.data.frame(fed.pca.quart$x)
colnames(fed.pca.quart.df) <- paste0(colnames(fed.pca.quart.df), "q")
fed.pca.quart.df$quarter <- as.Date(rownames(fed.pca.quart.df))
meeting.df <- merge(meeting.df, fed.pca.quart.df, by = "quarter", all.x = TRUE)
rownames(meeting.df) <- meeting.df$meeting_id


### NYT

articles.df <- merge(pre_articles, meeting.key, by = "meeting_id", all.x = TRUE)

articles.df$meet_date <- as.Date(articles.df$meet_date)


# Principal components of each meeting's topic attention variables
articles.pca <- prcomp(articles.df[,variablenames], center = TRUE, scale. = TRUE)
summary(articles.pca)
articles.pca.df <- as.data.frame(articles.pca$x)
articles.pca.df$meeting_id <- articles.df$meeting_id
articles.df <- merge(articles.df, articles.pca.df, by = "meeting_id")
rownames(articles.df) <- articles.df$meeting_id

# Aggregate the topic variables to quarterly
articles.df$quarter <- floor_date(articles.df$meet_date, unit = "quarter")
articles.quartave <- aggregate(articles.df[,c(variablenames)], list(articles.df$quarter), mean)
colnames(articles.quartave) <- c("quarter", paste0(variablenames, "q"))
rownames(articles.quartave) <- articles.quartave$quarter

# Calculate principal components of the monthly averages
articles.pca.quart <- prcomp(articles.quartave[,paste0("T", 1:30, "q")], center = TRUE, scale. = TRUE)
summary(articles.pca.quart)
articles.pca.quart.df <- as.data.frame(articles.pca.quart$x)
colnames(articles.pca.quart.df) <- paste0(colnames(articles.pca.quart.df), "q")
articles.pca.quart.df$quarter <- as.Date(rownames(articles.pca.quart.df))
articles.df <- merge(articles.df, articles.pca.quart.df, by = "quarter", all.x = TRUE)
rownames(articles.df) <- articles.df$meeting_id


### Reorder before analysing
meeting.df <- meeting.df[order(meeting.df$meet_date),]
articles.df <- articles.df[order(articles.df$meet_date),]
meeting.df$quarter <- as.Date(meeting.df$quarter)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)
articles.df$quarter <- as.Date(articles.df$quarter)
articles.df$meet_date <- as.Date(articles.df$meet_date)

### Plot the PCs

ggplot() + 
  scale_color_manual("", values = c("NYT PC1" = "dimgray", "Fed PC1" = "blue3", "ECB PC1" = "darkgoldenrod2",
                                    "NYT PC1q" = "dimgray", "Fed PC1q" = "blue3", "ECB PC1q" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("NYT PC1" = 1, "Fed PC1" = 1, "ECB PC1" = 1,
                                    "NYT PC1q" = 2, "Fed PC1q" = 2, "ECB PC1q" = 2)) +
  geom_line(data = articles.df, aes(x = meet_date, y = PC1 , color = "NYT PC1",linetype = "NYT PC1")) +
  geom_line(data = meeting.df, aes(x = meet_date, y = -PC1 , color = "Fed PC1", linetype = "Fed PC1")) +
  geom_line(data = articles.df, aes(x = quarter, y = PC1q , color = "NYT PC1q",linetype = "NYT PC1q")) +
  geom_line(data = meeting.df, aes(x = quarter, y = PC1q , color = "Fed PC1q",linetype = "Fed PC1q")) +
  xlab('Date') +
  ylab("First Principal Component")
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "PCA/PC_fednyt_1pc.png"))

ggplot() + 
  scale_color_manual("", values = c("NYT PC1 + PC2" = "dimgray", "Fed PC1 + PC2" = "blue3", "ECB PC1 + PC2" = "darkgoldenrod2",
                                    "NYT PC1q + PC2q" = "dimgray", "Fed PC1q + PC2q" = "blue3", "ECB PC1q" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("NYT PC1 + PC2" = 1, "Fed PC1 + PC2" = 1, "ECB PC1 + PC2" = 1,
                                    "NYT PC1q + PC2q" = 2, "Fed PC1q + PC2q" = 2, "ECB PC1q" = 2)) +
  geom_line(data = articles.df, aes(x = meet_date, y = PC1 + PC2, color = "NYT PC1 + PC2",linetype = "NYT PC1 + PC2")) +
  geom_line(data = meeting.df, aes(x = meet_date, y = -PC1 - PC2, color = "Fed PC1 + PC2", linetype = "Fed PC1 + PC2")) +
  geom_line(data = articles.df, aes(x = quarter, y = PC1q + PC2q, color = "NYT PC1q + PC2q",linetype = "NYT PC1q + PC2q")) +
  geom_line(data = meeting.df, aes(x = quarter, y = PC1q + PC2q, color = "Fed PC1q + PC2q",linetype = "Fed PC1q + PC2q")) +
  xlab('Date') +
  ylab("First 2 Principal Components")
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "PCA/PC_fednyt_2pc.png"))



### Are the principal components correlated
all.data <- merge(meeting.df, articles.df, by = "meet_date")
all.data$meet_date <- as.Date(all.data$meet_date)

cor.test(all.data$PC1.y, -all.data$PC1.x)
cor.test(all.data$PC1q.y, -all.data$PC1q.x)
cor.test(all.data$PC2q.y, all.data$PC2q.x)
cor.test(all.data$PC1q.y + all.data$PC2.y, all.data$PC1q.x - all.data$PC2.x)
cor.test(all.data$PC2q.y, all.data$PC2q.x)


ggplot(all.data) + 
  scale_color_manual("", values = c("NYT PC1 + PC2" = "dimgray", "Fed PC1 + PC2" = "blue3", "ECB PC1 + PC2" = "darkgoldenrod2",
                                    "NYT PC1q + PC2q" = "dimgray", "Fed PC1q + PC2q" = "blue3", "ECB PC1q" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("NYT PC1 + PC2" = 1, "Fed PC1 + PC2" = 1, "ECB PC1 + PC2" = 1,
                                    "NYT PC1q + PC2q" = 2, "Fed PC1q + PC2q" = 2, "ECB PC1q" = 2)) +
  geom_line(aes(x = meet_date, y = PC1.y , color = "NYT PC1q + PC2q",linetype = "NYT PC1q + PC2q")) +
  geom_line( aes(x = meet_date, y = -PC1.x, color = "Fed PC1q + PC2q",linetype = "Fed PC1q + PC2q")) +
  xlab('Date') +
  ylab("First 2 Principal Components")
ggplot(all.data) + 
  scale_color_manual("", values = c("NYT PC1 + PC2" = "dimgray", "Fed PC1 + PC2" = "blue3", "ECB PC1 + PC2" = "darkgoldenrod2",
                                    "NYT PC1q + PC2q" = "dimgray", "Fed PC1q + PC2q" = "blue3", "ECB PC1q" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("NYT PC1 + PC2" = 1, "Fed PC1 + PC2" = 1, "ECB PC1 + PC2" = 1,
                                    "NYT PC1q + PC2q" = 2, "Fed PC1q + PC2q" = 2, "ECB PC1q" = 2)) +
  geom_line(aes(x = meet_date, y = PC1.y + PC2.y , color = "NYT PC1q + PC2q",linetype = "NYT PC1q + PC2q")) +
  geom_line( aes(x = meet_date, y = -PC1.x - PC2.y, color = "Fed PC1q + PC2q",linetype = "Fed PC1q + PC2q")) +
  xlab('Date') +
  ylab("First 2 Principal Components")







############################# Stationarity tests ############################# 

# Set some parameters
topicnumber <- 30
var_used <- "T"
variablenames <- paste0(var_used, 1:topicnumber)

rownames(meeting.df) <- meeting.df$meeting_id
rownames(articles.df) <- articles.df$meeting_id





corpusnames <- c("Federal Reserve", "New York Times")

# ADF unit root test for each series, with trend and two lags
ur.tests <- as.data.frame(matrix(NA,nrow = (topicnumber), ncol = 3))
colnames(ur.tests) <- c("Topic", "Fed", "NYT")
ur.tests$Topic <- variablenames
for (topic in variablenames){
  print(topic)
  
  # Federal Reserve
  temp.series <- meeting.df[,topic]
  temp.test <- ur.df(temp.series, type = "none", lags = 1)
  summary(temp.test)
  temp.test@teststat
  temp.result <- as.character(round(temp.test@teststat,2))
  ur.tests[which(ur.tests$Topic == topic), "Fed"] <- temp.result
  
  # New York Times
  temp.series <- articles.df[,topic]
  temp.test <- ur.df(temp.series, type = "none", lags = 2)
  summary(temp.test)
  temp.test@teststat
  temp.result <- as.character(round(temp.test@teststat,2))
  ur.tests[which(ur.tests$Topic == topic), "NYT"] <- temp.result
  
}

stargazer(as.matrix(ur.tests))


# ADF unit root test for each series, with trend and two lags
ur.tests <- as.data.frame(matrix(NA,nrow = (topicnumber), ncol = 7))
colnames(ur.tests) <- c("Topic", "Fed tau3", "Fed phi2", "Fed phi3", "NYT tau3", "NYT phi2", "NYT phi3")
ur.tests$Topic <- variablenames
for (topic in variablenames){
  print(topic)
  
  # Bank of England
  temp.series <- meeting.df[,topic]
  temp.test <- ur.df(temp.series, type = "trend", lags = 1)
  summary(temp.test)
  temp.test@teststat[,"tau3"]
  temp.test@cval["tau3","5pct"]
  temp.result <- paste0(as.character(round(temp.test@teststat[,"tau3"],2)), " (", 
                        as.character(round(temp.test@cval["tau3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "Fed tau3"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi2"],2)), " (", 
                        as.character(round(temp.test@cval["phi2","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "Fed phi2"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi3"],2)), " (", 
                        as.character(round(temp.test@cval["phi3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "Fed phi3"] <- temp.result
  
  # Federal Reserve
  temp.series <- articles.df[,topic]
  temp.test <- ur.df(temp.series, type = "trend", lags = 1)
  summary(temp.test)
  temp.test@teststat
  temp.result <- paste0(as.character(round(temp.test@teststat[,"tau3"],2)), " (", 
                        as.character(round(temp.test@cval["tau3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "NYT tau3"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi2"],2)), " (", 
                        as.character(round(temp.test@cval["phi2","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "NYT phi2"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi3"],2)), " (", 
                        as.character(round(temp.test@cval["phi3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "NYT phi3"] <- temp.result
  
}

stargazer(as.matrix(ur.tests))



############################# Summarise the topics ############################# 

meeting.means <- colMeans(meeting.df[,variablenames])
article.means <- colMeans(articles.df[,variablenames])

topic.table <- as.data.frame(matrix(NA,nrow = (topicnumber), ncol = 5))
colnames(topic.table) <- c("Topic", "Description", "Top 5 words", "Fed mean", "NYT mean")
topic.table$Topic <- variablenames
topic.table$`Fed mean` <- meeting.means
topic.table$`NYT mean` <- article.means

stargazer(as.matrix(topic.table))





############################# End ############################# 

