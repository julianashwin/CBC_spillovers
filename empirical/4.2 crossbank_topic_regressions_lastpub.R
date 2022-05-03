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
  geom_line(data = meeting.df[which(meeting.df$central_bank == "European Central Bank"),],
            aes(x= meet_date, y = difftime, color = "ECB statements")) +
  xlab("Meeting date") +
  ylab("Days between meeting and publication date")
#ggtitle("GDP growth")




# Start with the first observation
meeting.df$last_fed <- ""
meeting.df$fed_published <- 0
meeting.df$last_bank <- ""
meeting.df$bank_published <- 0
meeting.df$last_ecb <- ""
meeting.df$ecb_published <- 0

# Want most recent meetings which are at most 100 days ago,
for (i in 1:nrow(meeting.df)){
  
  # Fed
  meeting_date <- meeting.df$meet_date[i] 
  last_fed <- meeting.df[which( (meeting.df$central_bank == "Federal Reserve") &
                                  (difftime(meeting_date, meeting.df$pub_date, units = "days") > 0) &
                                 (difftime(meeting_date, meeting.df$pub_date, units = "days") < 100)),]
  last_fed <- last_fed[rev(order(last_fed$meet_date)),]
  pub_date <- last_fed$pub_date[1]
  last_fed <- last_fed$meeting_id[1]
  
  meeting.df$last_fed[i] <- last_fed
  meeting.df$fed_published[i] <- as.numeric(meeting.df$meet_date[i] >= pub_date)
  
  # BoE
  last_bank <- meeting.df[which( (meeting.df$central_bank == "Bank of England") &
                                  (difftime(meeting_date, meeting.df$pub_date, units = "days") > 0) &
                                  (difftime(meeting_date, meeting.df$pub_date, units = "days") < 100)),]
  last_bank <- last_bank[rev(order(last_bank$meet_date)),]
  pub_date <- last_bank$pub_date[1]
  last_bank <- last_bank$meeting_id[1]
  
  meeting.df$last_bank[i] <- last_bank
  meeting.df$bank_published[i] <- as.numeric(meeting.df$meet_date[i] >= pub_date)
  
  # ECB
  last_ecb <- meeting.df[which( (meeting.df$central_bank == "European Central Bank") &
                                   (difftime(meeting_date, meeting.df$pub_date, units = "days") > 0) &
                                   (difftime(meeting_date, meeting.df$pub_date, units = "days") < 100)),]
  last_ecb <- last_ecb[rev(order(last_ecb$meet_date)),]
  pub_date <- last_ecb$pub_date[1]
  last_ecb <- last_ecb$meeting_id[1]
  
  meeting.df$last_ecb[i] <- last_ecb
  meeting.df$ecb_published[i] <- as.numeric(meeting.df$meet_date[i] >= pub_date)
  
}
rm(last_ecb, last_fed, last_bank)

table(meeting.df$fed_published)
table(meeting.df$bank_published)
table(meeting.df$ecb_published)


# Order by meeting date 
meeting.df <- meeting.df[order(meeting.df$meet_date),]


### Set which variable we will use (to "y", "T" or "lT")
var_used <- "T"


# Toggle the topic number (might need to adjust)
k <- 30
variablenames <- paste0(var_used, 1:k)
data_short <- meeting.df[,c("meeting_id", "central_bank", "pub_date", "meet_date", 
                            "last_fed", "fed_published", "last_bank", "bank_published", 
                            "last_ecb", "ecb_published", variablenames)]


# convert to long from for topic-meeting combinations

command <- paste0("meeting_long <- gather(data_short, topic, var_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))
#meeting_long <- gather(data_short, topic, var_value, y1:y30, factor_key=TRUE)
meeting_long <- meeting_long[order(meeting_long$meet_date),]


#### Standardise the variable
stand = TRUE
if (stand){
  aggregate.meeting <- meeting_long[,c("central_bank", "topic", "var_value")]
  
  aggregate.meeting <- aggregate.meeting %>%
    group_by(central_bank, topic) %>% 
    summarise_all(funs(mean, sd), na.rm = TRUE)
  
  meeting_long <- merge(meeting_long, aggregate.meeting, by = c("topic", "central_bank"))
  
  meeting_long$var_value <- (meeting_long$var_value - 
                               meeting_long$mean)/meeting_long$sd
  
  
}




meeting_long$fed_preaverage <- NA
meeting_long$bank_preaverage <- NA
meeting_long$ecb_preaverage <- NA

for (i in 1:nrow(meeting_long)){
  
  temp.row <- meeting_long[i,]
  temp.topic <- temp.row$topic
  
  if (i %% 100 == 0){
    print(paste("Iter", i, "out of", nrow(meeting_long)))
  }
  
  fed_influencer <- temp.row$last_fed
  bank_influencer <- temp.row$last_bank
  ecb_influencer <- temp.row$last_ecb
  
  
  # Fed average
  fed.df <- meeting_long[which(meeting_long$meeting_id == fed_influencer & 
                                 meeting_long$topic == temp.topic),]
  if (nrow(fed.df) > 0){
    temp.row$fed_preaverage <- fed.df$var_value
  } else if (nrow(fed.df) == 0){
    temp.row$fed_preaverage <- NA
  }
  
  # Bank average
  bank.df <- meeting_long[which(meeting_long$meeting_id == bank_influencer & 
                                  meeting_long$topic == temp.topic),]
  if (nrow(bank.df) > 0){
    temp.row$bank_preaverage <- bank.df$var_value
  } else if (nrow(bank.df) == 0){
    temp.row$bank_preaverage <- NA
  }
  
  # ECB average
  ecb.df <- meeting_long[which(meeting_long$meeting_id == ecb_influencer & 
                                 meeting_long$topic == temp.topic),]
  if (nrow(ecb.df) > 0){
    temp.row$ecb_preaverage <- ecb.df$var_value
  } else if (nrow(ecb.df) == 0){
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
write.csv(meeting_long, file = paste0(clean_dir, "CBC/meetingtopicprops_panel_alt.csv"), 
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
meeting_long$fed_var_6lag <- plm::lag(meeting_long$fed_value, 6)
meeting_long$fed_var_7lag <- plm::lag(meeting_long$fed_value, 7)
meeting_long$fed_var_8lag <- plm::lag(meeting_long$fed_value, 8)
meeting_long$fed_var_9lag <- plm::lag(meeting_long$fed_value, 9)
meeting_long$fed_var_10lag <- plm::lag(meeting_long$fed_value, 10)

meeting_long$bank_var_1lag <- plm::lag(meeting_long$bank_value, 1)
meeting_long$bank_var_2lag <- plm::lag(meeting_long$bank_value, 2)
meeting_long$bank_var_3lag <- plm::lag(meeting_long$bank_value, 3)
meeting_long$bank_var_4lag <- plm::lag(meeting_long$bank_value, 4)
meeting_long$bank_var_5lag <- plm::lag(meeting_long$bank_value, 5)
meeting_long$bank_var_6lag <- plm::lag(meeting_long$bank_value, 6)
meeting_long$bank_var_7lag <- plm::lag(meeting_long$bank_value, 7)
meeting_long$bank_var_8lag <- plm::lag(meeting_long$bank_value, 8)
meeting_long$bank_var_9lag <- plm::lag(meeting_long$bank_value, 9)
meeting_long$bank_var_10lag <- plm::lag(meeting_long$bank_value, 10)

meeting_long$ecb_var_1lag <- plm::lag(meeting_long$ecb_value, 1)
meeting_long$ecb_var_2lag <- plm::lag(meeting_long$ecb_value, 2)
meeting_long$ecb_var_3lag <- plm::lag(meeting_long$ecb_value, 3)
meeting_long$ecb_var_4lag <- plm::lag(meeting_long$ecb_value, 4)
meeting_long$ecb_var_5lag <- plm::lag(meeting_long$ecb_value, 5)
meeting_long$ecb_var_6lag <- plm::lag(meeting_long$ecb_value, 6)
meeting_long$ecb_var_7lag <- plm::lag(meeting_long$ecb_value, 7)
meeting_long$ecb_var_8lag <- plm::lag(meeting_long$ecb_value, 8)
meeting_long$ecb_var_9lag <- plm::lag(meeting_long$ecb_value, 9)
meeting_long$ecb_var_10lag <- plm::lag(meeting_long$ecb_value, 10)


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

model4 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 fed_var_6lag + fed_var_7lag + fed_var_8lag + fed_var_9lag + fed_var_10lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 bank_var_6lag + bank_var_7lag + bank_var_8lag + bank_var_9lag + bank_var_10lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag +
                 ecb_var_6lag + ecb_var_7lag + ecb_var_8lag + ecb_var_9lag + ecb_var_10lag 
               | centralbank_topic, data = meeting_long) 
summary(model4)



stargazer (model1, model3,  table.placement = "H", df = TRUE,
          title = "Topic proportion (standardised) last meeting CB specific influence and lags", label = "tab:cb_influence_speclags")

meeting_long$fed_published <- as.numeric(meeting_long$meet_date >= "2005-01-01")

meeting_long$fed_on_boe_pub <- meeting_long$fed_on_boe*meeting_long$fed_published
meeting_long$fed_on_ecb_pub <- meeting_long$fed_on_ecb*meeting_long$fed_published
meeting_long$boe_on_fed_pub <- meeting_long$boe_on_fed*meeting_long$bank_published
meeting_long$boe_on_ecb_pub <- meeting_long$boe_on_ecb*meeting_long$bank_published
meeting_long$ecb_on_fed_pub <- 0
meeting_long$ecb_on_boe_pub <- 0



model4 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 boe_on_fed_pub +
                 fed_on_boe_pub  +
                 fed_on_ecb_pub + boe_on_ecb_pub  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | centralbank_topic, data = meeting_long) 
summary(model4)

stargazer(model1, model2, model3, model4,  table.placement = "H", df = TRUE,
          title = "Transformed Topic proportion", label = "tab:cb_pub_date")



### Include inflation and output as controls
import_filename = paste(clean_dir, "CBC/macro_data.csv", sep = "/")
macro.df <- read.csv(import_filename, stringsAsFactors = FALSE)
macro.df$month <- as.Date(macro.df$month)
macro.df$quarter <- as.Date(macro.df$quarter)
meeting_long$month <- floor_date(meeting_long$meet_date, unit = "month")
meeting_long$quarter <- floor_date(meeting_long$meet_date, unit = "quarter")
meeting_long$month <- as.Date(meeting_long$month)
meeting_long$quarter <- as.Date(meeting_long$quarter)

colnames(macro.df)

# Separate out each country
ukmacro.df <- macro.df
ukmacro.df$central_bank <- "Bank of England"
ukmacro.df$growth <- ukmacro.df$UK_growth
ukmacro.df$inflation <- ukmacro.df$UK_inflation_m
ukmacro.df$rate_change <- ukmacro.df$UK_policy_rate_change
ukmacro.df <- select(ukmacro.df, month, quarter, central_bank, growth, inflation, rate_change)

usmacro.df <- macro.df
usmacro.df$central_bank <- "Federal Reserve"
usmacro.df$growth <- usmacro.df$US_growth
usmacro.df$inflation <- usmacro.df$US_inflation_m
usmacro.df$rate_change <- usmacro.df$US_policy_rate_change
usmacro.df <- select(usmacro.df, month, quarter, central_bank, growth, inflation, rate_change)

ezmacro.df <- macro.df
ezmacro.df$central_bank <- "European Central Bank"
ezmacro.df$growth <- ezmacro.df$EZ_growth
ezmacro.df$inflation <- ezmacro.df$EZ_inflation_m
ezmacro.df$rate_change <- ezmacro.df$EZ_policy_rate_change
ezmacro.df <- select(ezmacro.df, month, quarter, central_bank, growth, inflation, rate_change)

meeting_long <- data.frame(meeting_long)
meeting_long$central_bank <- as.character(meeting_long$central_bank)
meeting_long$month <- as.Date(meeting_long$month)
meeting_long$quarter <- as.Date(meeting_long$quarter)
fed_panel <- meeting_long[which(meeting_long$central_bank == "Federal Reserve"),]
bank_panel <- data.frame(meeting_long[which(meeting_long$central_bank == "Bank of England"),])
ecb_panel <- data.frame(meeting_long[which(meeting_long$central_bank == "European Central Bank"),])



fed_controls <- merge(fed_panel, usmacro.df, by = c("month", "quarter", "central_bank"), all.x = TRUE)
bank_controls <- merge(bank_panel, ukmacro.df, by = c("month", "quarter", "central_bank"), all.x = TRUE)
ecb_controls <- merge(ecb_panel, ezmacro.df, by = c("month", "quarter", "central_bank"), all.x = TRUE)

meeting_controls <- rbind(fed_controls, bank_controls, ecb_controls)

### Get separate macro control variables for each topic, zeroing out everywhere else.
for (i in 1:k){
  
  command <- paste0("meeting_controls$growth_T",i," <- meeting_controls$growth*as.numeric(meeting_controls$topic == \"T", i, "\")")
  eval(parse(text=command))
  command <- paste0("meeting_controls$inflation_T",i," <- meeting_controls$inflation*as.numeric(meeting_controls$topic == \"T", i, "\")")
  eval(parse(text=command))
  command <- paste0("meeting_controls$rate_change_T",i," <- meeting_controls$rate_change*as.numeric(meeting_controls$topic == \"T", i, "\")")
  eval(parse(text=command))
  
}

model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                fed_on_boe + ecb_on_boe +
                fed_on_ecb + boe_on_ecb , data = meeting_controls) 
summary(model)
model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                fed_on_boe + ecb_on_boe +
                fed_on_ecb + boe_on_ecb + 
                growth_T1 + inflation_T1 + rate_change_T1 
              | centralbank_topic , data = meeting_controls) 
summary(model)


model3 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag 
               | centralbank_topic, data = meeting_controls) 
summary(model3)




command <- paste0("model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag 
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model)

command <- paste0("model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + 
                growth_T",i, "+ inflation_T", i, "+ rate_change_T", i, "
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model)

controls_text <- ""
for (i in 1:k){
  controls_text <- paste0(controls_text, " + growth_T",i, " + inflation_T", i, " + rate_change_T", i)
}

command <- paste0("model1 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + 
                 bank_var_1lag + 
                 ecb_var_1lag", 
                  controls_text, "
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model1)


command <- paste0("model2 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
  fed_var_6lag + fed_var_7lag + fed_var_8lag + fed_var_9lag + fed_var_10lag +
  bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
  bank_var_6lag + bank_var_7lag + bank_var_8lag + bank_var_9lag + bank_var_10lag + 
  ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag +
  ecb_var_6lag + ecb_var_7lag + ecb_var_8lag + ecb_var_9lag + ecb_var_10lag", 
                  controls_text, "
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model2)
stargazer(model1, model2)





### Test if change in Fed publication policy in 2005 is important

meeting_controls$dummy <- as.numeric(meeting_controls$pub_date >= "2005-01-01")
summary(meeting_controls$dummy)

meeting_controls$fed_on_boe_dummy <- meeting_controls$fed_on_boe*meeting_controls$dummy
meeting_controls$fed_on_ecb_dummy <- meeting_controls$fed_on_ecb*meeting_controls$dummy

### Control for the Zero Lower Bound
model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                fed_on_boe + ecb_on_boe +
                fed_on_ecb + boe_on_ecb + 
                fed_on_boe_dummy + fed_on_ecb_dummy, data = meeting_controls) 
summary(model)


command <- paste0("model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_on_boe_dummy + fed_on_ecb_dummy +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag +
                 bank_var_1lag + bank_var_2lag + bank_var_3lag + 
                 ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + 
                growth_T",i, "+ inflation_T", i, "+ rate_change_T", i, "
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model)

command <- paste0("model1 <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                 fed_on_boe  + ecb_on_boe +
                 fed_on_ecb + boe_on_ecb  +
                 fed_on_boe_dummy + fed_on_ecb_dummy  +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
  fed_var_6lag + fed_var_7lag + fed_var_8lag + fed_var_9lag + fed_var_10lag +
  bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
  bank_var_6lag + bank_var_7lag + bank_var_8lag + bank_var_9lag + bank_var_10lag + 
  ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag +
  ecb_var_6lag + ecb_var_7lag + ecb_var_8lag + ecb_var_9lag + ecb_var_10lag ", controls_text, "
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model1)



stargazer(model, model1)


















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
for (i in 1:k){
  print(i)
  topicname <- paste0("y",i)
  temp.df <- meeting_long[which(meeting_long$topic == topicname),]
  #temp.df <- pdata.frame(temp.df, index = c("centralbank_topic", "meeting_no"))
  model <- felm(var_value ~ boe_on_fed + ecb_on_fed +
                  fed_on_boe  + ecb_on_boe +
                  fed_on_ecb + boe_on_ecb  +
                  fed_var_1lag + bank_var_1lag + ecb_var_1lag| centralbank_topic, data = temp.df) 
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