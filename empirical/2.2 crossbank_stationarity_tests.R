setwd("~/Documents/GitHub/CBC_spillovers")
rm(list=ls())

require(tm)
require(stringr)
require(tidytext)
require(tidyr)
require(slam)
require(topicmodels)
require(ggplot2)
require(dplyr)
require(wordcloud)
require(lfe)
require(plm)
require(lubridate)
require(stargazer)
require(zoo)
require(urca)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"

### Import the topic proportion data
meeting.df <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df <- meeting.df[order(meeting.df$meet_date),]


# Set some parameters
topicnumber <- 30
var_used <- "y"
variablenames <- paste0(var_used, 1:topicnumber)


# Get some quarterly and monthly averages to merge with macro variables
meeting.df$quarter <- floor_date(meeting.df$meet_date, "quarter")
meeting.df$month <- floor_date(meeting.df$meet_date, "month")
meeting_short <- meeting.df[,c("meeting_id", "year", "month", "quarter", "central_bank", "pub_date", "meet_date")]

# Split into each separate central bank
bank.df <- meeting.df[which(meeting.df$central_bank == "Bank of England"),]
rownames(bank.df) <- bank.df$meeting_id
fed.df <- meeting.df[which(meeting.df$central_bank == "Federal Reserve"),]
rownames(fed.df) <- fed.df$meeting_id
ecb.df <- meeting.df[which(meeting.df$central_bank == "European Central Bank"),]
rownames(ecb.df) <- ecb.df$meeting_id








corpusnames <- c("Federal Reserve", "Bank of England", "European Central Bank")

# ADF unit root test for each series, with trend and two lags
ur.tests <- as.data.frame(matrix(NA,nrow = (topicnumber), ncol = 4))
colnames(ur.tests) <- c("Topic", "BoE", "Fed", "ECB")
ur.tests$Topic <- variablenames
for (topic in variablenames){
  print(topic)
  
  # Bank of England
  temp.series <- bank.df[,topic]
  temp.test <- ur.df(temp.series, type = "none", lags = 1)
  summary(temp.test)
  temp.test@teststat
  temp.result <- as.character(round(temp.test@teststat,2))
  ur.tests[which(ur.tests$Topic == topic), "BoE"] <- temp.result
  
  # Federal Reserve
  temp.series <- fed.df[,topic]
  temp.test <- ur.df(temp.series, type = "none", lags = 2)
  summary(temp.test)
  temp.test@teststat
  temp.result <- as.character(round(temp.test@teststat,2))
  ur.tests[which(ur.tests$Topic == topic), "Fed"] <- temp.result
  
  
  # European Central Bank
  temp.series <- ecb.df[,topic]
  temp.test <- ur.df(temp.series, type = "none", lags = 2)
  summary(temp.test)
  temp.test@teststat
  temp.result <- as.character(round(temp.test@teststat,2))
  ur.tests[which(ur.tests$Topic == topic), "ECB"] <- temp.result
  
}

stargazer(as.matrix(ur.tests))
  

# ADF unit root test for each series, with trend and two lags
ur.tests <- as.data.frame(matrix(NA,nrow = (topicnumber), ncol = 10))
colnames(ur.tests) <- c("Topic", "BoE tau3", "BoE phi2", "BoE phi3", "Fed tau3", "Fed phi2", "Fed phi3", 
                        "ECB tau3", "ECB phi2", "ECB phi3")
ur.tests$Topic <- variablenames
for (topic in variablenames){
  print(topic)
  
  # Bank of England
  temp.series <- bank.df[,topic]
  temp.test <- ur.df(temp.series, type = "trend", lags = 1)
  summary(temp.test)
  temp.test@teststat[,"tau3"]
  temp.test@cval["tau3","5pct"]
  temp.result <- paste0(as.character(round(temp.test@teststat[,"tau3"],2)), " (", 
                        as.character(round(temp.test@cval["tau3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "BoE tau3"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi2"],2)), " (", 
                        as.character(round(temp.test@cval["phi2","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "BoE phi2"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi3"],2)), " (", 
                        as.character(round(temp.test@cval["phi3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "BoE phi3"] <- temp.result
  
  # Federal Reserve
  temp.series <- fed.df[,topic]
  temp.test <- ur.df(temp.series, type = "trend", lags = 1)
  summary(temp.test)
  temp.test@teststat
  temp.result <- paste0(as.character(round(temp.test@teststat[,"tau3"],2)), " (", 
                        as.character(round(temp.test@cval["tau3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "Fed tau3"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi2"],2)), " (", 
                        as.character(round(temp.test@cval["phi2","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "Fed phi2"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi3"],2)), " (", 
                        as.character(round(temp.test@cval["phi3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "Fed phi3"] <- temp.result
  
  # European Central Bank
  temp.series <- ecb.df[,topic]
  temp.test <- ur.df(temp.series, type = "trend", lags = 1)
  summary(temp.test)
  temp.test@teststat
  temp.result <- paste0(as.character(round(temp.test@teststat[,"tau3"],2)), " (", 
                        as.character(round(temp.test@cval["tau3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "ECB tau3"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi2"],2)), " (", 
                        as.character(round(temp.test@cval["phi2","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "ECB phi2"] <- temp.result
  temp.result <- paste0(as.character(round(temp.test@teststat[,"phi3"],2)), " (", 
                        as.character(round(temp.test@cval["phi3","5pct"],2)), ")")
  ur.tests[which(ur.tests$Topic == topic), "ECB phi3"] <- temp.result
  
  
}

stargazer(as.matrix(ur.tests))



