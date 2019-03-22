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

### Import the topic attention data
meeting.df <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops.csv"), encoding = "utf-8", stringsAsFactors = FALSE)

meeting.df$pub_date <- as.Date(meeting.df$pub_date)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)


### Identify potentially influential meetings
plus <- difftime(meeting.df$pub_date[1], meeting.df$meet_date[1], units = "days")
minus <- difftime(meeting.df$meet_date[1], meeting.df$pub_date[1], units = "days")

meeting.df$difftime <- as.numeric(difftime(meeting.df$pub_date, meeting.df$meet_date, units = "days"))


# Plot the time difference between publication and meeting date across the sample

ggplot() + 
  scale_color_manual("",
                     values = c("BoE minutes" = "black", "Fed minutes" = "blue3", "ECB statements" = "darkgoldenrod2")) +
  geom_line(data = meeting.df[which(meeting.df$central_bank == "Bank of England"),],
            aes(x = meet_date, y = difftime, color = "BoE minutes")) +
  geom_line(data = meeting.df[which(meeting.df$central_bank == "Federal Reserve"),], 
            aes(x = meet_date, y = difftime, color = "Fed minutes")) +
  geom_line(data = meeting.df[which(meeting.df$central_bank == "European Central Bank"),],
            aes(x= meet_date, y = difftime, color = "ECB statements")) +
  xlab("Meeting date") +
  ylab("Days between meeting and publication date")
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "meeting_pub_date.png"))



# Start with the first observation
meeting.df$last_fed <- ""
meeting.df$fed_published <- 0
meeting.df$last_boe <- ""
meeting.df$boe_published <- 0
meeting.df$last_ecb <- ""
meeting.df$ecb_published <- 0

# Want meetings which are at most 100 days ago
for (i in 1:nrow(meeting.df)){
  meeting_date <- meeting.df$meet_date[i] 
  last_fed <- meeting.df[which( (meeting.df$central_bank == "Federal Reserve") &
                                  (difftime(meeting_date, meeting.df$meet_date, units = "days") > 0) &
                                 (difftime(meeting_date, meeting.df$meet_date, units = "days") < 100)),]
  last_fed <- last_fed[rev(order(last_fed$meet_date)),]
  last_fed <- last_fed$meeting_id[1]
  
  meeting.df$last_fed <- last_fed
  
  
  
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


# Write the influence-variable panel to csv
write.csv(meeting_long, file = paste0(clean_dir, "CBC/meetingtopicprops_panel.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)
#meeting_long <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops_panel.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
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





### Look at CB specific effects

# Dummy for each CB
meeting_long$BoE_dummy <- as.numeric(meeting_long$central_bank == "Bank of England")
table(meeting_long$BoE_dummy)
meeting_long$Fed_dummy <- as.numeric(meeting_long$central_bank == "Federal Reserve")
table(meeting_long$Fed_dummy)
meeting_long$ECB_dummy <- as.numeric(meeting_long$central_bank == "European Central Bank")
table(meeting_long$ECB_dummy)

# Calculate CB-specific influence
meeting_long$fed_on_fed <- meeting_long$fed_preaverage*meeting_long$Fed_dummy
meeting_long$boe_on_fed <- meeting_long$bank_preaverage*meeting_long$Fed_dummy
meeting_long$ecb_on_fed <- meeting_long$ecb_preaverage*meeting_long$Fed_dummy
meeting_long$fed_on_boe <- meeting_long$fed_preaverage*meeting_long$BoE_dummy
meeting_long$boe_on_boe <- meeting_long$bank_preaverage*meeting_long$BoE_dummy
meeting_long$ecb_on_boe <- meeting_long$ecb_preaverage*meeting_long$BoE_dummy
meeting_long$fed_on_ecb <- meeting_long$fed_preaverage*meeting_long$ECB_dummy
meeting_long$boe_on_ecb <- meeting_long$bank_preaverage*meeting_long$ECB_dummy
meeting_long$ecb_on_ecb <- meeting_long$ecb_preaverage*meeting_long$ECB_dummy


model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                fed_on_boe  + ecb_on_boe +
                fed_on_ecb + boe_on_ecb , data = meeting_long) 
summary(model)
model1 <- felm(var_value ~  boe_on_fed + ecb_on_fed +
                 fed_on_boe + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb | centralbank_topic, data = meeting_long) 
summary(model1)

model2 <- felm(var_value ~  boe_on_fed + ecb_on_fed +
                 fed_on_boe + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb +
                 plm::lag(var_value)| centralbank_topic, data = meeting_long) 
summary(model2)

model3 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb +
                 plm::lag(var_value, 1:12)| centralbank_topic, data = meeting_long) 
summary(model3)

model4 <- felm(var_value ~  boe_on_fed + ecb_on_fed +
                 fed_on_boe + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb +
                 plm::lag(var_value, 1:12)| quarters + centralbank_topic, data = meeting_long) 
summary(model4)


stargazer(model, model1, model2, model3, model4,  table.placement = "H", df = FALSE,
          title = "Transformed Topic proportion, CB specific, results", label = "tab:cb_influence")


### Now allow for CB specific lags
meeting_long$bank_value <- meeting_long$var_value*meeting_long$BoE_dummy
meeting_long$fed_value <- meeting_long$var_value*meeting_long$Fed_dummy
meeting_long$ecb_value <- meeting_long$var_value*meeting_long$ECB_dummy

# 5 lags for now 
meeting_long$fed_var_1lag <- plm::lag(meeting_long$fed_value, 1)
meeting_long$fed_var_2lag <- plm::lag(meeting_long$fed_value, 2)
meeting_long$fed_var_3lag <- plm::lag(meeting_long$fed_value, 3)
meeting_long$fed_var_4lag <- plm::lag(meeting_long$fed_value, 4)
meeting_long$fed_var_5lag <- plm::lag(meeting_long$fed_value, 5)

meeting_long$bank_var_1lag <- plm::lag(meeting_long$bank_value, 1)
meeting_long$bank_var_2lag <- plm::lag(meeting_long$bank_value, 2)
meeting_long$bank_var_3lag <- plm::lag(meeting_long$bank_value, 3)
meeting_long$bank_var_4lag <- plm::lag(meeting_long$bank_value, 4)
meeting_long$bank_var_5lag <- plm::lag(meeting_long$bank_value, 5)

meeting_long$ecb_var_1lag <- plm::lag(meeting_long$ecb_value, 1)
meeting_long$ecb_var_2lag <- plm::lag(meeting_long$ecb_value, 2)
meeting_long$ecb_var_3lag <- plm::lag(meeting_long$ecb_value, 3)
meeting_long$ecb_var_4lag <- plm::lag(meeting_long$ecb_value, 4)
meeting_long$ecb_var_5lag <- plm::lag(meeting_long$ecb_value, 5)

model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                fed_on_boe + ecb_on_boe +
                fed_on_ecb + boe_on_ecb , data = meeting_long) 
summary(model)
model1 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  | centralbank_topic, data = meeting_long) 
summary(model1)

model2 <- felm(var_value ~  boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb +
                 fed_var_1lag + bank_var_1lag + ecb_var_1lag| centralbank_topic, data = meeting_long) 
summary(model2)

model3 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | centralbank_topic, data = meeting_long) 
summary(model3)

model3 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | quarters + centralbank_topic, data = meeting_long) 
summary(model3)


stargazer(model, model1, model2, model3,  table.placement = "H", df = TRUE,
          title = "Transformed Topic proportion, CB specific influence and lags", label = "tab:cb_influence_speclags")





#meeting_wide <- spread(meeting_long, topic, var_value)


### Include inflation and output as controls
all.info <- read.csv(paste0(clean_dir, "CBC/meeting_details.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
all.info$month <- as.Date(all.info$month)

# Create a ZLB dummy
all.info$ZLB <- as.numeric(all.info$policy_rate <= 0.5)
# The first few ECB meetings don't have a policy rate, but assume this wasn't at the ZLB
all.info[which(is.na(all.info$ZLB)),"ZLB"] <- 0
table(as.numeric(all.info$ZLB))



meeting_controls <- merge(meeting_long, all.info[,c("meeting_id", "quarter", "growth", "inflation_quarterly", "inflation_monthly",
                                                    "policy_rate","PC1", "PC1q", "ZLB")], by = c("meeting_id"))
meeting_controls <- pdata.frame(meeting_controls, index = c("centralbank_topic", "meeting_no"))

model <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage  + inflation_monthly , data = meeting_controls) 
summary(model)
model1 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + inflation_monthly | centralbank_topic, data = meeting_controls) 
summary(model1)

model2 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:2) + inflation_monthly + growth | centralbank_topic, data = meeting_controls) 
summary(model2)

model3 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:5)+ inflation_monthly| centralbank_topic, data = meeting_controls) 
summary(model3)

model3 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:12) + inflation_monthly + growth| quarters + centralbank_topic, 
               data = meeting_controls) 
summary(model3)



### Control for the Zero Lower Bound


model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                fed_on_boe + ecb_on_boe +
                fed_on_ecb + boe_on_ecb + 
                ZLB*boe_on_fed + ZLB*ecb_on_fed +
                ZLB*fed_on_boe + ZLB*ecb_on_boe +
                ZLB*fed_on_ecb + ZLB*boe_on_ecb, data = meeting_controls) 
summary(model)
model1 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  + 
                 ZLB*boe_on_fed + ZLB*ecb_on_fed +
                 ZLB*fed_on_boe + ZLB*ecb_on_boe +
                 ZLB*fed_on_ecb + ZLB*boe_on_ecb| centralbank_topic, data = meeting_controls) 
summary(model1)

model2 <- felm(var_value ~  boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb + 
                 ZLB*boe_on_fed + ZLB*ecb_on_fed +
                 ZLB*fed_on_boe + ZLB*ecb_on_boe +
                 ZLB*fed_on_ecb + ZLB*boe_on_ecb +
                 fed_var_1lag + bank_var_1lag + ecb_var_1lag| centralbank_topic, data = meeting_controls) 
summary(model2)

model3 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  + 
                 ZLB*boe_on_fed + ZLB*ecb_on_fed +
                 ZLB*fed_on_boe + ZLB*ecb_on_boe +
                 ZLB*fed_on_ecb + ZLB*boe_on_ecb +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | centralbank_topic, data = meeting_controls) 
summary(model3)


stargazer(model, model1, model2, model3,  table.placement = "H", df = TRUE,
          title = "Transformed Topic proportion, CB specific influence and lags", label = "tab:cb_influence_zlb")



### Plot the Zero Lower Bound episodes

bank.df <- all.info[which(all.info$central_bank == "Bank of England"),]
fed.df <- all.info[which(all.info$central_bank == "Federal Reserve"),]
ecb.df <- all.info[which(all.info$central_bank == "European Central Bank"),]

### UK plot
dummy <- bank.df %>% 
  filter(ZLB == 1) %>% 
  filter(month %in% c(min(month), max(month))) %>%
  unique()

ggplot() + 
  scale_color_manual("",
                     values = c("UK policy rate" = "black", "US policy rate" = "blue3", "EZ policy rate" = "darkgoldenrod2")) +
  geom_line(data = bank.df, aes(x = month, y = policy_rate, color = "UK policy rate")) +
  #geom_line(data = fed.df, aes(x = month, y = policy_rate, color = "US policy rate")) +
  #geom_line(data = ecb.df, aes(x= month, y = policy_rate, color = "EZ policy rate")) +
  annotate(geom = "rect", xmin = min(dummy$month), xmax = max(dummy$month), 
           ymin = 0, ymax = Inf, fill = "gray", alpha = 0.4) +
  xlab('Date') +
  ylab("Policy rate (percent, from BIS)") + theme_light()
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/UK_policy_rate_ZLB.png"))\

### US plot
dummy <- fed.df %>% 
  filter(ZLB == 1) %>% 
  filter(month %in% c(min(month), max(month))) %>%
  unique()

ggplot() + 
  scale_color_manual("",
                     values = c("UK policy rate" = "black", "US policy rate" = "blue3", "EZ policy rate" = "darkgoldenrod2")) +
  #geom_line(data = bank.df, aes(x = month, y = policy_rate, color = "UK policy rate")) +
  geom_line(data = fed.df, aes(x = month, y = policy_rate, color = "US policy rate")) +
  #geom_line(data = ecb.df, aes(x= month, y = policy_rate, color = "EZ policy rate")) +
  annotate(geom = "rect", xmin = min(dummy$month), xmax = max(dummy$month), 
           ymin = 0, ymax = Inf, fill = "gray", alpha = 0.4) +
  xlab('Date') +
  ylab("Policy rate (percent, from BIS)") + theme_light()
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/US_policy_rate_ZLB.png"))

dummy <- ecb.df %>% 
  filter(ZLB == 1) %>% 
  filter(month %in% c(min(month), max(month))) %>%
  unique()
ggplot() + 
  scale_color_manual("",
                     values = c("UK policy rate" = "black", "US policy rate" = "blue3", "EZ policy rate" = "darkgoldenrod2")) +
  #geom_line(data = bank.df, aes(x = month, y = policy_rate, color = "UK policy rate")) +
  #geom_line(data = fed.df, aes(x = month, y = policy_rate, color = "US policy rate")) +
  geom_line(data = ecb.df, aes(x= month, y = policy_rate, color = "EZ policy rate")) +
  annotate(geom = "rect", xmin = min(dummy$month), xmax = max(dummy$month), 
           ymin = 0, ymax = Inf, fill = "gray", alpha = 0.4) +
  xlab('Date') +
  ylab("Policy rate (percent, from BIS)") + theme_light()
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/EZ_policy_rate_ZLB.png"))







### Estimate topic-by-topic regressions
for (i in 1:topicnumber){
  print(i)
  topicname <- paste0("y",i)
  temp.df <- meeting_long[which(meeting_long$topic == topicname),]
  #temp.df <- pdata.frame(temp.df, index = c("centralbank_topic", "meeting_no"))
  model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                  fed_on_boe  + ecb_on_boe +
                  fed_on_ecb + boe_on_ecb  +
                  fed_var_1lag + bank_var_1lag + ecb_var_1lag, data = temp.df) 
  felm(var_value ~ ecb_preaverage+ fed_preaverage+ bank_preaverage| centralbank_topic, data = temp.df)
  print(summary(model))
}



# Check if the result is different across the time period of the sample
meeting_controls$difftime <- as.numeric(difftime(meeting_controls$pub_date, meeting_controls$meet_date, units = "days"))

meeting_controls$publish_dummy <- as.numeric(meeting_controls$difftime < 25)

meeting_long$meet_date <- as.Date(meeting_long$meet_date)

meeting_early <- meeting_long[which(meeting_long$meet_date < "2005-01-01"),]
meeting_late <- meeting_long[which(meeting_long$meet_date >= "2005-01-01"),]
meeting_long$publish_dummy <- as.numeric(meeting_long$meet_date >= "2005-01-01")

model1 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  + 
                 publish_dummy*boe_on_fed + publish_dummy*ecb_on_fed +
                 publish_dummy*fed_on_boe + publish_dummy*ecb_on_boe +
                 publish_dummy*fed_on_ecb + publish_dummy*boe_on_ecb| centralbank_topic, data = meeting_long) 
summary(model1)

model2 <- felm(var_value ~  boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb + 
                 publish_dummy*boe_on_fed + publish_dummy*ecb_on_fed +
                 publish_dummy*fed_on_boe + publish_dummy*ecb_on_boe +
                 publish_dummy*fed_on_ecb + publish_dummy*boe_on_ecb +
                 fed_var_1lag + bank_var_1lag + ecb_var_1lag| centralbank_topic, data = meeting_long) 
summary(model2)

model3 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  + 
                 publish_dummy*boe_on_fed + publish_dummy*ecb_on_fed +
                 publish_dummy*fed_on_boe + publish_dummy*ecb_on_boe +
                 publish_dummy*fed_on_ecb + publish_dummy*boe_on_ecb +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | centralbank_topic, data = meeting_long) 
summary(model3)

model4 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  + 
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | centralbank_topic, data = meeting_long) 
summary(model3)

stargazer(model3, model4)


############################# End ############################# 