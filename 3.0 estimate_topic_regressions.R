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

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"

### Import the Federal Reserve data
meeting.df <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops.csv"), encoding = "utf-8", stringsAsFactors = FALSE)

meeting.df$pub_date <- as.Date(meeting.df$pub_date)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)

plus <- difftime(meeting.df$pub_date[1], meeting.df$meet_date[1], units = "days")
minus <- difftime(meeting.df$meet_date[1], meeting.df$pub_date[1], units = "days")

# Start with the first observation
meeting.df$influenced_by <- ""

for (i in 1:nrow(meeting.df)){
  meeting_date <- meeting.df$meet_date[i] 
  influenced_by <- meeting.df[which((difftime(meeting_date, meeting.df$pub_date, units = "days") > 0) &
                     (difftime(meeting_date, meeting.df$pub_date, units = "days") < 62)), "meeting_id"]
  meeting.df$influenced_by[i] <- paste0(paste(influenced_by, collapse = "; "), ";")
}

# Order by meeting date 
meeting.df <- meeting.df[order(meeting.df$meet_date),]





### Set which variable we will use (to "y", "T" or "lT")
var_used <- "y"


# Toggle the topic number (might need to adjust)
k <- 30
variablenames <- paste0(var_used, 1:k)
data_short <- meeting.df[,c("meeting_id", "central_bank", "pub_date", "meet_date", 
                               "influenced_by", variablenames)]
data_short$meeting_id_str <- paste0(data_short$meeting_id, ";")

# convert to long from for topic-meeting combinations

command <- paste0("meeting_long <- gather(data_short, topic, var_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))
#meeting_long <- gather(data_short, topic, var_value, y1:y30, factor_key=TRUE)
meeting_long <- meeting_long[order(meeting_long$meeting_id),]

meeting_long$fed_N <- NA
meeting_long$fed_preaverage <- NA
meeting_long$bank_N <- NA
meeting_long$bank_preaverage <- NA
meeting_long$ecb_N <- NA
meeting_long$ecb_preaverage <- NA

for (i in 1:nrow(meeting_long)){
  
  temp.row <- meeting_long[i,]
  temp.topic <- temp.row$topic
  
  if (i %% 100 == 0){
    print(paste("Iter", i, "out of", nrow(meeting_long)))
  }
  
  influencers <- str_split(temp.row$influenced_by, " ")[[1]]
  fed_influencers <- influencers[str_detect(influencers, "F")]
  temp.row$fed_N <- length(fed_influencers)
  bank_influencers <- influencers[str_detect(influencers, "B")]
  temp.row$bank_N <- length(bank_influencers)
  ecb_influencers <- influencers[str_detect(influencers, "E")]
  temp.row$ecb_N <- length(ecb_influencers)
  
  
  # Fed average
  fed.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, fed_influencers) & 
                                  meeting_long$topic == temp.topic),]
  if (temp.row$fed_N > 0){
    temp.row$fed_preaverage <- mean(fed.df$var_value)
  } else if (temp.row$fed_N == 0){
    temp.row$fed_preaverage <- NA
  }
  
  # Bank average
  bank.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, bank_influencers) & 
                                 meeting_long$topic == temp.topic),]
  if (temp.row$bank_N > 0){
    temp.row$bank_preaverage <- mean(bank.df$var_value)
  } else if (temp.row$bank_N == 0){
    temp.row$bank_preaverage <- NA
  }
  
  # ECB average
  ecb.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, ecb_influencers) & 
                                 meeting_long$topic == temp.topic),]
  if (temp.row$ecb_N > 0){
    temp.row$ecb_preaverage <- mean(ecb.df$var_value)
  } else if (temp.row$ecb_N == 0){
    temp.row$ecb_preaverage <- NA
  }
  
  ### Replace row with new row
  meeting_long[i,] <- temp.row
  
}


# A proxy date to allow the inclusion of lags
meeting_long$meeting_no <- as.numeric(str_sub(meeting_long$meeting_id, 3))

# Include quarter dummies to control for seasonality
meeting_long$quarters <- quarter(meeting_long$meet_date, with_year = FALSE, fiscal_start = 1)



meeting_long$central_bank <- as.factor(meeting_long$central_bank)
meeting_long$centralbank_topic <- paste(meeting_long$central_bank, meeting_long$topic, sep = "_")

meeting_long <- pdata.frame(meeting_long, index = c("centralbank_topic", "meeting_no"))

model <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage, data = meeting_long) 
summary(model)
model1 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage | centralbank_topic, data = meeting_long) 
summary(model1)

model2 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value)| centralbank_topic, data = meeting_long) 
summary(model2)

model3 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:12)| centralbank_topic, data = meeting_long) 
summary(model3)

model3 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:12)| quarters + centralbank_topic, data = meeting_long) 
summary(model3)

stargazer(model, model1, model2, model3,   table.placement = "H", df = FALSE,
          title = "Transformed Topic proportion results")


#meeting_wide <- spread(meeting_long, topic, var_value)



############################# End ############################# 