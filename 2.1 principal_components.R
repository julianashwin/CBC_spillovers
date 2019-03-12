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

# Set some parameters
topicnumber <- 30
var_used <- "y"
variablenames <- paste0(var_used, 1:topicnumber)


############################# Import data ############################# 

### Import the topic proportion data
meeting.df <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df <- meeting.df[order(meeting.df$meet_date),]

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


# Import growth and inflation data for each economy






############################# Principal Components analysis

bank.quartave <- aggregate(bank.df[,c(variablenames)], list(bank.df$quarter), mean)
colnames(bank.quartave) <- c("quarter", paste0(variablenames, "q"))
rownames(bank.quartave) <- bank.quartave$quarter
bank.monthave <- aggregate(bank.df[,c(variablenames)], list(bank.df$month), mean)
colnames(bank.monthave) <- c("month", paste0(variablenames, "m"))

bank.df <- merge(bank.df, bank.quartave, by = "quarter", all.x = TRUE)
bank.df <- merge(bank.df, bank.monthave, by = "month", all.x = TRUE)

### Import relevant macro data
ukmacro.df <- read.csv(paste0(clean_dir, "UK_macro/UK_macro_data.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
ukmacro.df$Date <- as.Date(ukmacro.df$Date)
ukmacro.df <- ukmacro.df[which(ukmacro.df$Date >= "1997-01-01"),]
ukmacro.df <- ukmacro.df[which(ukmacro.df$Date < "2016-01-01"),]
ukmacro.df$quarter <- floor_date(ukmacro.df$Date, "quarter")
ggplot() + 
  geom_line(data = ukmacro.df, aes(x = Date, y = Leading_ind - 100, color = "Leading indicator")) + 
  geom_line(data = ukmacro.df, aes(x = Date, y = Inflation_CPI, color = "Inflation"))
  

bank.df <- merge(bank.df, ukmacro.df, by = "quarter", all.x = TRUE)
rownames(bank.df) <- bank.df$meeting_id


### Compute principle components of the attention measures
topicnumber <- 30
var_used <- "y"
variablenames <- paste0(var_used, 1:topicnumber)

bank.pca <- prcomp(bank.df[,variablenames], center = TRUE, scale. = TRUE)
summary(bank.pca)
bank.pca.df <- as.data.frame(bank.pca$x)
bank.pca.df$meeting_id <- rownames(bank.pca.df)

bank.pca.quart <- prcomp(bank.quartave[,paste0(var_used, 1:topicnumber, "q")], center = TRUE, scale. = TRUE)
summary(bank.pca.quart)
bank.pca.quart.df <- as.data.frame(bank.pca.quart$x)
bank.pca.quart.df$quarter <- as.Date(rownames(bank.quartave))


bank.df <- merge(bank.df, bank.pca.df, by = "meeting_id") # no all.x bc dimensions should be identical

ggplot() + 
  scale_color_manual("Central Bank",
    values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "Inflation" = "red")) +
  geom_line(data = bank.df, aes(x = meet_date, y = -PC1, color = "BoE")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = PC2+ PC1, color = "BoE")) +
  geom_line(data = bank.df, aes(x= Date, y = Inflation_CPI, color = "Inflation")) +
         xlab('Meeting date') +
         ylab(expression(theta[bt])) + 
         ggtitle("Topic 2 over time")



ggplot() + 
  scale_color_manual("",
                     values = c("BoE PC1" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "UK Inflation" = "red")) +
  geom_line(data = bank.df, aes(x = meet_date, y = PC1, color = "BoE PC1")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = -PC2- PC1, color = "BoE")) +
  geom_line(data = ukmacro.df, aes(x= quarter, y = (-3*Inflation_CPI) + 5, color = "UK Inflation")) +
  xlab('Meeting date') +
  #ylab(expression(y[bt])) + 
  scale_y_continuous("BoE PC1",sec.axis = sec_axis(~ (. /-3) +(5/3) , name = "UK Inflation")) +
  ggtitle("First Principal Component of BoE minutes and Inflation (CPI)")
ggsave(paste0(export_dir, "PCA/BoE_PC1_Inflation.png"))


ggplot() + 
  scale_color_manual("",
                     values = c("BoE PC1" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "UK Inflation" = "red")) +
  geom_line(data = bank.pca.quart.df, aes(x = quarter, y = PC1, color = "BoE PC1")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = -PC2- PC1, color = "BoE")) +
  geom_line(data = ukmacro.df, aes(x= quarter, y = (3*Inflation_CPI) - 5, color = "UK Inflation")) +
  xlab('Meeting date') +
  #ylab(expression(y[bt])) + 
  scale_y_continuous("BoE PC1",sec.axis = sec_axis(~ (. /-3) +(5/3) , name = "UK Inflation")) +
  ggtitle("First Principal Component of BoE minutes and Inflation (CPI)")
ggsave(paste0(export_dir, "PCA/BoE_PC1_Inflation_quarterly.png"))



cor.test(-bank.df$PC1, bank.df$Inflation_CPI)
bank.lm <- lm(Inflation_CPI ~ dplyr::lead(PC1, 0), data = bank.df)
summary(bank.lm)
bank.lm <- lm(Inflation_CPI ~ PC1 + PC2 + PC3, data = bank.df)
summary(bank.lm)

bank.df$inf_fitted_values <- bank.lm$fitted.values

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE PC1:PC3" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "UK Inflation" = "red")) +
  geom_line(data = bank.df, aes(x = meet_date, y = inf_fitted_values, color = "BoE PC1:PC3")) +
  geom_line(data = bank.df, aes(x= meet_date, y = Inflation_CPI, color = "UK Inflation")) +
  ylab("BoE PC1:PC3") + 
  ggtitle("UK inflation and first 3 Principal Components")
ggsave(paste0(export_dir, "PCA/BoE_PC1to3_Inflation.png"))



import_filename <- paste(raw_dir, "US_macro/FRED_downloads/NAEXKP01GBQ657S.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$growth <- import.df$NAEXKP01GBQ657S

bank.df <- merge(bank.df, import.df, by = "quarter", all.x = TRUE)
bank.pca.quart.df <- merge(bank.pca.quart.df, import.df, by = "quarter", all.x = TRUE)

bank.lm <- lm(growth ~ PC1, data = bank.pca.quart.df)
summary(bank.lm)
bank.lm <- lm(growth ~ PC1 + PC2 + PC3, data = bank.pca.quart.df)
summary(bank.lm)

bank.lm <- lm(growth ~ PC1, data = bank.df)
summary(bank.lm)
bank.lm <- lm(growth ~ PC1 + PC2 + PC3, data = bank.df)
summary(bank.lm)













### Now try the same for the Federal Reserve


fed.quartave <- aggregate(fed.df[,c(variablenames)], list(fed.df$quarter), mean)
colnames(fed.quartave) <- c("quarter", paste0(variablenames, "q"))
fed.monthave <- aggregate(fed.df[,c(variablenames)], list(fed.df$month), mean)
colnames(fed.monthave) <- c("month", paste0(variablenames, "m"))

fed.df <- merge(fed.df, fed.quartave, by = "quarter", all.x = TRUE)
fed.df <- merge(fed.df, fed.monthave, by = "month", all.x = TRUE)

### Import relevant macro data

import_filename <- paste(clean_dir, "US_macro/Fernald_clean.csv", sep = "")
usmacro.df <- read.csv(import_filename)
usmacro.df$quarter <- as.Date(usmacro.df$quarter)

usmacro.df <- usmacro.df[which(usmacro.df$quarter >= "1997-01-01"),]
usmacro.df <- usmacro.df[which(usmacro.df$quarter < "2016-01-01"),]
usmacro.df$quarter <- floor_date(usmacro.df$quarter, "quarter")

ggplot() + 
  geom_line(data = usmacro.df, aes(x = quarter, y = dhours, color = "Hours growth")) + 
  geom_line(data = usmacro.df, aes(x = quarter, y = dY, color = "GDP growth"))


fed.df <- merge(fed.df, usmacro.df, by = "quarter", all.x = TRUE)
rownames(fed.df) <- fed.df$meeting_id

### Compute principle components of the attention measures
topicnumber <- 30
var_used <- "y"
variablenames <- paste0(var_used, 1:topicnumber)

fed.pca <- prcomp(fed.df[,variablenames], center = TRUE, scale. = TRUE)
summary(bank.pca)
fed.pca.df <- as.data.frame(fed.pca$x)
fed.pca.df$meeting_id <- rownames(fed.pca.df)

fed.df <- merge(fed.df, fed.pca.df, by = "meeting_id") # no all.x bc dimensions should be identical

# Import US inflation data
import_filename <- paste(raw_dir, "US_macro/FRED_downloads/CPGRLE01USQ659N.csv", sep = "")
usinflation.df <- read.csv(import_filename)
usinflation.df$quarter <- as.Date(usinflation.df$DATE)
usinflation.df$inflation <- usinflation.df$CPGRLE01USQ659N
fed.df <- merge(fed.df, usinflation.df, by = "quarter", all.x = TRUE)



ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed PC1:PC2" = "blue3", "ECB" = "darkgoldenrod2", "US Inflation" = "red")) +
  geom_line(data = fed.df, aes(x = meet_date, y =-PC1 -PC2, color = "Fed PC1:PC2")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = PC2+ PC1, color = "BoE")) +
  geom_line(data = fed.df, aes(x= quarter, y = (inflation), color = "US Inflation")) +
  xlab('Meeting date') +
  ylab("Fed PC1:PC2") + 
  ggtitle("US inflation and first 2 Principal Components")
ggsave(paste0(export_dir, "PCA/Fed_PC1toPC1_Inflation.png"))



cor.test((fed.df$PC1+fed.df$PC2), fed.df$inflation)

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "GDP growth" = "red")) +
  geom_line(data = fed.df, aes(x = meet_date, y = -PC1-PC2-PC3, color = "Fed")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = -PC2- PC1, color = "BoE")) +
  geom_line(data = fed.df, aes(x= quarter, y = dY, color = "GDP growth")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("Topic 2 over time")

cor.test(-fed.df$PC1, fed.df$dY)
fed.lm <- lm(dY ~ PC1, data = fed.df)
summary(fed.lm)
fed.lm <- lm(du_consumption ~ PC1 + PC2 + PC3, data = fed.df)
summary(fed.lm)

fed.df$Y_fitted_values <- fed.lm$fitted.values

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "GDP growth" = "red")) +
  geom_line(data = fed.df, aes(x = meet_date, y = Y_fitted_values, color = "Fed")) +
  geom_line(data = fed.df, aes(x= meet_date, y = dY, color = "GDP growth")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("US growth and first 3 Principal Components")

cor.test(-fed.df$PC1, fed.df$inflation)
fed.lm <- lm(inflation ~ PC1, data = fed.df)
summary(fed.lm)
fed.lm <- lm(inflation ~ PC1 + PC2 + PC3, data = fed.df)
summary(fed.lm)

fed.df$inf_fitted_values <- fed.lm$fitted.values

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "GDP growth" = "red")) +
  geom_line(data = fed.df, aes(x = meet_date, y = inf_fitted_values, color = "Fed")) +
  geom_line(data = fed.df, aes(x= meet_date, y = inflation, color = "GDP growth")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("US inflation and first 3 Principal Components")




### Now try the same for the ECB

# Import EZ macro data
import_filename <- paste(raw_dir, "US_macro/FRED_downloads/EA19CPHPLA01GYM.csv", sep = "")
ezinflation.df <- read.csv(import_filename)
ezinflation.df$quarter <- as.Date(ezinflation.df$DATE)
ezinflation.df$inflation <- ezinflation.df$EA19CPHPLA01GYM
ecb.df <- merge(ecb.df, ezinflation.df, by = "quarter", all.x = TRUE)

rownames(ecb.df) <- ecb.df$meeting_id


ggplot() + 
  geom_line(data = ecb.df, aes(x = quarter, y = inflation, color = "Inflation"))



### Compute principle components of the attention measures
topicnumber <- 30
var_used <- "y"
variablenames <- paste0(var_used, 1:topicnumber)

ecb.pca <- prcomp(ecb.df[,variablenames], center = TRUE, scale. = TRUE)
summary(ecb.pca)
ecb.pca.df <- as.data.frame(ecb.pca$x)
ecb.pca.df$meeting_id <- rownames(ecb.pca.df)

ecb.df <- merge(ecb.df, ecb.pca.df, by = "meeting_id") # no all.x bc dimensions should be identical

rownames(ecb.df) <- ecb.df$meeting_id

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "Inflation" = "red")) +
  geom_line(data = ecb.df, aes(x = meet_date, y = PC2, color = "ECB")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = PC2+ PC1, color = "BoE")) +
  geom_line(data = ecb.df, aes(x= quarter, y = (inflation), color = "Inflation")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("Topic 2 over time")

cor.test((ecb.df$PC2), ecb.df$inflation)
ecb.lm <- lm(inflation ~ PC1, data = ecb.df)
summary(ecb.lm)
ecb.lm <- lm(inflation ~ PC1 + PC2 + PC3, data = ecb.df)
summary(ecb.lm)

ecb.df$inf_fitted_values <- ecb.lm$fitted.values

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "GDP growth" = "red")) +
  geom_line(data = ecb.df, aes(x = meet_date, y = inf_fitted_values, color = "ECB")) +
  geom_line(data = ecb.df, aes(x= meet_date, y = inflation, color = "GDP growth")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("ECB inflation and first 3 Principal Components")




