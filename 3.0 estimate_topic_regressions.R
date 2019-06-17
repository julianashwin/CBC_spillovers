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
require(panelvar)

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



### Get a table of avergae topic proportions
var_used <- "T"
k <- 30
variablenames <- paste0(var_used, 1:k)

meeting.df <- meeting.df[,c("meeting_id", "year", "month", "central_bank", 
                            "pub_date", "meet_date", variablenames)]

aggregate.df <- meeting.df[,c("central_bank", variablenames)]

aggregate.df <- aggregate(aggregate.df[,variablenames], by = list(aggregate.df$central_bank), FUN = mean)
rownames(aggregate.df) <- aggregate.df[,1]
aggregate.df <- aggregate.df[,2:31]

aggregate.df <- t(as.matrix(aggregate.df))
aggregate.df <- data.frame(round(aggregate.df, 4))
aggregate.df$Top5 <- ""
aggregate.df$Description <- ""
aggregate.df$Topic <- rownames(aggregate.df)
aggregate.df <- aggregate.df[,c("Topic", "Description", "Top5", 
                                "Bank.of.England", "European.Central.Bank", "Federal.Reserve")]

stargazer(as.matrix(aggregate.df), rownames = FALSE)



### Aggregate to quarterly

meeting.df$quarter <- floor_date(meeting.df$meet_date, unit = "quarter")
meeting.df$month <- floor_date(meeting.df$meet_date, unit = "month")

meeting.df <- meeting.df[,c("meeting_id", "year", "quarter", "month", "central_bank", 
                            "pub_date", "meet_date", variablenames)]


quarterly.df <- aggregate(meeting.df[,c(variablenames)], list(meeting.df$quarter, meeting.df$central_bank), mean)

quarterly.df$quarter <- quarterly.df$Group.1
quarterly.df$central_bank <- quarterly.df$Group.2
quarterly.df <- quarterly.df[,c("central_bank", "quarter", variablenames)]

# convert to long from for topic-meeting combinations

command <- paste0("quarterly_panel <- gather(quarterly.df, topic, var_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))

fed_panel <- quarterly_panel[which(quarterly_panel$central_bank == "Federal Reserve"),]
fed_panel$fed_value <- fed_panel$var_value
fed_panel <- select(fed_panel, topic, quarter, fed_value)
bank_panel <- quarterly_panel[which(quarterly_panel$central_bank == "Bank of England"),]
bank_panel$bank_value <- bank_panel$var_value
bank_panel <- select(bank_panel, topic, quarter, bank_value)
ecb_panel <- quarterly_panel[which(quarterly_panel$central_bank == "European Central Bank"),]
ecb_panel$ecb_value <- ecb_panel$var_value
ecb_panel <- select(ecb_panel, topic, quarter, ecb_value)


quarterly_panel <- merge(fed_panel, bank_panel, by = c("topic", "quarter"))
quarterly_panel <- merge(quarterly_panel, ecb_panel, by = c("topic", "quarter"))
quarterly_panel$period <- as.integer(as.factor(quarterly_panel$quarter))
quarterly_panel <- pdata.frame(data.frame(quarterly_panel), index = c("topic", "period"))
quarterly_panel$quarter <- as.Date(quarterly_panel$quarter)




### Standardise the variables
colnames(quarterly_panel)
aggregate.panel <- quarterly_panel[,c("topic", "fed_value", "bank_value", "ecb_value")]
colnames(aggregate.panel)

aggregate.panel <- aggregate.panel %>%
  group_by(topic) %>% 
  summarise_all(funs(mean, sd), na.rm = TRUE)


stand_panel.df <- merge(quarterly_panel, aggregate.panel, by = "topic")

stand_panel.df$fed_value_std <- (stand_panel.df$fed_value - 
                                       stand_panel.df$fed_value_mean)/stand_panel.df$fed_value_sd
stand_panel.df$bank_value_std <- (stand_panel.df$bank_value - 
                                              stand_panel.df$bank_value_mean)/stand_panel.df$bank_value_sd
stand_panel.df$ecb_value_std <- (stand_panel.df$ecb_value - 
                                               stand_panel.df$ecb_value_mean)/stand_panel.df$ecb_value_sd


stand_panel.df <- pdata.frame(data.frame(stand_panel.df), index = c("topic", "period"))




model <- felm(fed_value ~ plm::lag(fed_value, 1) + plm::lag(bank_value, 1) + plm::lag(ecb_value, 1)| topic, data = quarterly_panel)
summary(model)
model_std <- felm(fed_value_std ~ plm::lag(fed_value_std, 1) + plm::lag(bank_value_std, 1) + plm::lag(ecb_value_std, 1)| topic, data = stand_panel.df)
summary(model_std)
model <- felm(bank_value ~  plm::lag(fed_value, 1) + plm::lag(bank_value, 1) + plm::lag(ecb_value, 1)| topic, data = quarterly_panel)
summary(model)
model_std <- felm(bank_value_std ~  plm::lag(fed_value_std, 1) + plm::lag(bank_value_std, 1) + plm::lag(ecb_value_std, 1)| topic, data = stand_panel.df)
summary(model_std)
model <- felm(ecb_value ~  plm::lag(fed_value, 1) + plm::lag(bank_value, 1) + plm::lag(ecb_value, 1)| topic, data = quarterly_panel)
summary(model)
model_std <- felm(ecb_value_std ~  plm::lag(fed_value_std, 1) + plm::lag(bank_value_std, 1) + plm::lag(ecb_value_std, 1)| topic, data = stand_panel.df)
summary(model_std)


model <- felm(ecb_value ~  plm::lag(fed_value, 1:3) + plm::lag(bank_value, 1:3) + plm::lag(ecb_value, 1:3)| topic, data = quarterly_panel)
summary(model)
model_std <- felm(ecb_value_std ~  plm::lag(fed_value_std, 1:3) + plm::lag(bank_value_std, 1:3) + plm::lag(ecb_value_std, 1:3)| topic, data = stand_panel.df)
summary(model_std)





pvar_model1 <- 
  pvarfeols(dependent_vars = c("fed_value", "bank_value", "ecb_value"),
            lags = 1,
            transformation = "demean",
            data = data.frame(quarterly_panel),
            panel_identifier= c("topic", "period"))
summary(pvar_model1)

pvar_model1_std <- 
  pvarfeols(dependent_vars = c("fed_value_std", "bank_value_std", "ecb_value_std"),
            lags = 1,
            transformation = "demean",
            data = data.frame(stand_panel.df),
            panel_identifier= c("topic", "period"))
summary(pvar_model1_std)


pvar_model3 <- 
  pvarfeols(dependent_vars = c("fed_value", "bank_value", "ecb_value"),
            lags = 3,
            transformation = "demean",
            data = data.frame(quarterly_panel),
            panel_identifier= c("topic", "period"))
summary(pvar_model3)

pvar_model3_std <- 
  pvarfeols(dependent_vars = c("fed_value_std", "bank_value_std", "ecb_value_std"),
            lags = 3,
            transformation = "demean",
            data = data.frame(stand_panel.df),
            panel_identifier= c("topic", "period"))
summary(pvar_model3_std)

stab_pvar_model <- stability(pvar_model3_std)
print(stab_pvar_model)
plot(stab_pvar_model)






################################ Identify meetings in previous two months ################################


# Start with the first observation
meeting.df$influenced_by <- ""

for (i in 1:nrow(meeting.df)){
  meeting_date <- meeting.df$meet_date[i] 
  influenced_by <- meeting.df[which((difftime(meeting_date, meeting.df$pub_date, units = "days") > 0) &
                     (difftime(meeting_date, meeting.df$pub_date, units = "days") < 95)), "meeting_id"]
  meeting.df$influenced_by[i] <- paste0(paste(influenced_by, collapse = "; "), ";")
}

# Order by meeting date 
meeting.df <- meeting.df[order(meeting.df$meet_date),]





### Set which variable we will use (to "y", "T" or "lT")
var_used <- "T"


# Toggle the topic number (might need to adjust)
k <- 30
variablenames <- paste0(var_used, 1:k)
data_short <- meeting.df[,c("meeting_id", "quarter", "month", "central_bank", "pub_date", "meet_date", 
                               "influenced_by", variablenames)]
data_short$meeting_id_str <- paste0(data_short$meeting_id, ";")

# convert to long from for topic-meeting combinations

command <- paste0("meeting_long <- gather(data_short, topic, var_value, ", var_used, "1:",
                  var_used, k, ", factor_key=TRUE)")
eval(parse(text=command))
#meeting_long <- gather(data_short, topic, var_value, y1:y30, factor_key=TRUE)
meeting_long <- meeting_long[order(meeting_long$meeting_id),]



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
  if (temp.row$fed_N > 0){
    fed.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, fed_influencers[1]) & 
                                   meeting_long$topic == temp.topic),]
    
    ## If there is more than one then we need to loop over these to identify them
    if (temp.row$fed_N > 1){
      for (j in 2:length(fed_influencers)){
        temp.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, fed_influencers[j]) & 
                                       meeting_long$topic == temp.topic),]
        
        fed.df <- rbind(fed.df, temp.df)
      }
    }
    
    temp.row$fed_preaverage <- mean(fed.df$var_value)
  } else if (temp.row$fed_N == 0){
    temp.row$fed_preaverage <- NA
  }
  
  # Bank average
  if (temp.row$bank_N > 0){
    bank.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, bank_influencers[1]) & 
                                   meeting_long$topic == temp.topic),]
    
    ## If there is more than one then we need to loop over these to identify them
    if (temp.row$bank_N > 1){
      for (j in 2:length(bank_influencers)){
        temp.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, bank_influencers[j]) & 
                                        meeting_long$topic == temp.topic),]
        
        bank.df <- rbind(bank.df, temp.df)
      }
    }
    temp.row$bank_preaverage <- mean(bank.df$var_value)
  } else if (temp.row$bank_N == 0){
    temp.row$bank_preaverage <- NA
  }
  
  # ECB average
  if (temp.row$ecb_N > 0){
    ecb.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, ecb_influencers[1]) & 
                                    meeting_long$topic == temp.topic),]
    
    ## If there is more than one then we need to loop over these to identify them
    if (temp.row$ecb_N > 1){
      for (j in 2:length(ecb_influencers)){
        temp.df <- meeting_long[which(str_detect(meeting_long$meeting_id_str, ecb_influencers[j]) & 
                                        meeting_long$topic == temp.topic),]
        
        ecb.df <- rbind(ecb.df, temp.df)
      }
    }
    temp.row$ecb_preaverage <- mean(ecb.df$var_value)
  } else if (temp.row$ecb_N == 0){
    temp.row$ecb_preaverage <- NA
  }
  
  ### Replace row with new row
  meeting_long[i,] <- temp.row
  
}


# A proxy date to allow the inclusion of lags
meeting_long$meeting_no <- as.numeric(str_sub(meeting_long$meeting_id, 3))


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


stargazer(model, model1, model2, model3, table.placement = "H", df = FALSE,
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



stargazer(model, model1, model2, model3,  table.placement = "H", df = FALSE,
          title = "Topic proportion results", label = "tab:cb_influence")


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




stargazer(model2, model4,  table.placement = "H", df = TRUE,
          title = "Topic proportion, CB specific influence and lags", label = "tab:cb_influence_speclags")





#meeting_wide <- spread(meeting_long, topic, var_value)


### Include inflation and output as controls
import_filename = paste(clean_dir, "CBC/macro_data.csv", sep = "/")
macro.df <- read.csv(import_filename, stringsAsFactors = FALSE)
macro.df$month <- as.Date(macro.df$month)
macro.df$quarter <- as.Date(macro.df$quarter)
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
                 fed_on_boe_dummy + fed_on_ecb_dummy +
                 fed_var_1lag + fed_var_2lag + fed_var_3lag + fed_var_4lag + fed_var_5lag +
  bank_var_1lag + bank_var_2lag + bank_var_3lag + bank_var_4lag + bank_var_5lag + 
  ecb_var_1lag + ecb_var_2lag + ecb_var_3lag + ecb_var_4lag + ecb_var_5lag ", "
               | centralbank_topic, data = meeting_controls)")
eval(parse(text=command))
summary(model1)






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
  
}



# Check if the result is different across the time period of the sample
meeting_long$meet_date <- as.Date(meeting_long$meet_date)

meeting_early <- meeting_long[which(meeting_long$meet_date < "2006-01-01"),]
meeting_late <- meeting_long[which(meeting_long$meet_date >= "2006-01-01"),]

model3 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:12)| quarters + centralbank_topic, 
               data = meeting_early) 
summary(model3)

model3 <- felm(var_value ~ ecb_preaverage + fed_preaverage + bank_preaverage + 
                 plm::lag(var_value, 1:12)| quarters + centralbank_topic, 
               data = meeting_late) 
summary(model3)



############################# End ############################# 