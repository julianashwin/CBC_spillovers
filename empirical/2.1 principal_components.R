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
require(readxl)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"

# Set some parameters
topicnumber <- 10
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


### Import growth and inflation data for each economy


### UK data

# UK growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/UK_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$growth <- import.df[,2]
bank.df <- merge(bank.df, import.df[,c("quarter", "growth")], by = "quarter", all.x = TRUE)
rownames(bank.df) <- bank.df$meeting_id

# UK quarterly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/UK_inflation_quarterly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$inflation_quarterly <- import.df[,2]
bank.df <- merge(bank.df, import.df[,c("quarter", "inflation_quarterly")], by = "quarter", all.x = TRUE)
rownames(bank.df) <- bank.df$meeting_id

# UK monthly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/UK_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$month <- as.Date(import.df$DATE)
import.df$inflation_monthly <- import.df[,2]
bank.df <- merge(bank.df, import.df[,c("month", "inflation_monthly")], by = "month", all.x = TRUE)
rownames(bank.df) <- bank.df$meeting_id


### US data

# US growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$growth <- import.df[,2]
fed.df <- merge(fed.df, import.df[,c("quarter", "growth")], by = "quarter", all.x = TRUE)
rownames(fed.df) <- fed.df$meeting_id

# US quarterly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_inflation_quarterly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$inflation_quarterly <- import.df[,2]
fed.df <- merge(fed.df, import.df[,c("quarter", "inflation_quarterly")], by = "quarter", all.x = TRUE)
rownames(fed.df) <- fed.df$meeting_id

# US monthly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$month <- as.Date(import.df$DATE)
import.df$inflation_monthly <- import.df[,2]
fed.df <- merge(fed.df, import.df[,c("month", "inflation_monthly")], by = "month", all.x = TRUE)
rownames(fed.df) <- fed.df$meeting_id


### EZ data

# EZ growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/EZ_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$growth <- import.df[,2]
ecb.df <- merge(ecb.df, import.df[,c("quarter", "growth")], by = "quarter", all.x = TRUE)
rownames(ecb.df) <- ecb.df$meeting_id

# EZ quarterly inflation (should be fine to use the monthly here as it's growth from same period, previous year)
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/EZ_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$inflation_quarterly <- import.df[,2]
ecb.df <- merge(ecb.df, import.df[,c("quarter", "inflation_quarterly")], by = "quarter", all.x = TRUE)
rownames(ecb.df) <- ecb.df$meeting_id

# EZ monthly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/EZ_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$month <- as.Date(import.df$DATE)
import.df$inflation_monthly <- import.df[,2]
ecb.df <- merge(ecb.df, import.df[,c("month", "inflation_monthly")], by = "month", all.x = TRUE)
rownames(ecb.df) <- ecb.df$meeting_id



### Import policy rates for each bank from BIS database
import_filename <- paste(raw_dir, "Macro_data/BIS_website/cbpol_1902.xlsx", sep = "")
import.df <- read_xlsx(import_filename, sheet = "Monthly Series", skip = 3)
import.df <- as.data.frame(import.df)
import.df$Period <- as.Date(import.df$Period)
import.df$month <- floor_date(import.df$Period, "month")
import.df$fed_policy_rate <- import.df[,"M:US"]
import.df$bank_policy_rate <- import.df[,"M:GB"]
import.df$ecb_policy_rate <- import.df[,"M:XM"]

import.df <- import.df[,c("month", "fed_policy_rate", "bank_policy_rate", "ecb_policy_rate")]

fed.df$month <- as.Date(fed.df$month)
fed.df <- merge(fed.df, import.df[,c("month", "fed_policy_rate")], by = "month", all.x = TRUE)
rownames(fed.df) <- fed.df$meeting_id
fed.df$policy_rate <- fed.df$fed_policy_rate
cor.test(fed.df$policy_rate, fed.df$fed_policy_rate)

bank.df <- merge(bank.df, import.df[,c("month", "bank_policy_rate")], by = "month", all.x = TRUE)
rownames(bank.df) <- bank.df$meeting_id
bank.df$policy_rate <- bank.df$bank_policy_rate
cor.test(bank.df$policy_rate, bank.df$bank_policy_rate)

ecb.df <- merge(ecb.df, import.df[,c("month", "ecb_policy_rate")], by = "month", all.x = TRUE)
rownames(ecb.df) <- ecb.df$meeting_id
ecb.df$policy_rate <- ecb.df$ecb_policy_rate
cor.test(ecb.df$policy_rate, ecb.df$ecb_policy_rate)



# Plot each of the series as a sense check
ggplot() + 
  scale_color_manual("",
                     values = c("UK growth" = "black", "US growth" = "blue3", "EZ growth" = "darkgoldenrod2")) +
  geom_line(data = bank.df, aes(x = quarter, y = growth, color = "UK growth")) +
  geom_line(data = fed.df, aes(x = quarter, y = growth, color = "US growth")) +
  geom_line(data = ecb.df, aes(x= quarter, y = growth, color = "EZ growth")) +
  xlab('Date') +
  ylab("GDP growth (same period, previous year")
  #ggtitle("GDP growth")

ggplot() + 
  scale_color_manual("", values = c("UK inflation (monthly)" = "black", "US inflation (monthly)" = "blue3", "EZ inflation (monthly)" = "darkgoldenrod2",
                                "UK inflation (quarterly)" = "black", "US inflation (quarterly)" = "blue3", "EZ inflation (quarterly)" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("UK inflation (monthly)" = 1, "US inflation (monthly)" = 1, "EZ inflation (monthly)" = 1,
                                             "UK inflation (quarterly)" = 2, "US inflation (quarterly)" = 2, "EZ inflation (quarterly)" = 2)) +
  geom_line(data = bank.df, aes(x = month, y = inflation_monthly, color = "UK inflation (monthly)",linetype = "UK inflation (monthly)")) +
  geom_line(data = bank.df, aes(x = quarter, y = inflation_quarterly, color = "UK inflation (quarterly)", linetype = "UK inflation (quarterly)")) +
  geom_line(data = fed.df, aes(x = month, y = inflation_monthly, color = "US inflation (monthly)", linetype = "US inflation (monthly)")) +
  geom_line(data = fed.df, aes(x = quarter, y = inflation_quarterly, color = "US inflation (quarterly)", linetype = "US inflation (quarterly)")) +
  geom_line(data = ecb.df, aes(x = month, y = inflation_monthly, color = "EZ inflation (monthly)", linetype = "EZ inflation (monthly)")) +
  geom_line(data = ecb.df, aes(x = quarter, y = inflation_quarterly, color = "EZ inflation (quarterly)", linetype = "EZ inflation (quarterly)")) +
  xlab('Date') +
  ylab("CPI inflation (same period, previous year")
#ggtitle("GDP growth")

ggplot() + 
  scale_color_manual("",
                     values = c("UK policy rate" = "black", "US policy rate" = "blue3", "EZ policy rate" = "darkgoldenrod2")) +
  geom_line(data = bank.df, aes(x = month, y = policy_rate, color = "UK policy rate")) +
  geom_line(data = fed.df, aes(x = month, y = policy_rate, color = "US policy rate")) +
  geom_line(data = ecb.df, aes(x= month, y = policy_rate, color = "EZ policy rate")) +
  xlab('Date') +
  ylab("Policy rate (percent, from BIS)")
#ggtitle("GDP growth")




############################# Principal Components analysis ############################# 


### UK

# Principal components of each meeting's topic attention variables
bank.pca <- prcomp(bank.df[,variablenames], center = TRUE, scale. = TRUE)
summary(bank.pca)
bank.pca.df <- as.data.frame(bank.pca$x)
bank.pca.df$meeting_id <- rownames(bank.pca.df)
bank.df <- merge(bank.df, bank.pca.df, by = "meeting_id") # no all.x bc dimensions should be identical
rownames(bank.df) <- bank.df$meeting_id

# Aggregate the topic variables to quarterly
bank.quartave <- aggregate(bank.df[,c(variablenames)], list(bank.df$quarter), mean)
colnames(bank.quartave) <- c("quarter", paste0(variablenames, "q"))
rownames(bank.quartave) <- bank.quartave$quarter

# Calculate principal components of the monthly averages
bank.pca.quart <- prcomp(bank.quartave[,paste0(var_used, 1:topicnumber, "q")], center = TRUE, scale. = TRUE)
summary(bank.pca.quart)
bank.pca.quart.df <- as.data.frame(bank.pca.quart$x)
colnames(bank.pca.quart.df) <- paste0(colnames(bank.pca.quart.df), "q")
bank.pca.quart.df$quarter <- as.Date(rownames(bank.pca.quart.df))
bank.df <- merge(bank.df, bank.pca.quart.df, by = "quarter", all.x = TRUE)
rownames(bank.df) <- bank.df$meeting_id


### US

# Principal components of each meeting's topic attention variables
fed.pca <- prcomp(fed.df[,variablenames], center = TRUE, scale. = TRUE)
summary(fed.pca)
fed.pca.df <- as.data.frame(fed.pca$x)
fed.pca.df$meeting_id <- rownames(fed.pca.df)
fed.df <- merge(fed.df, fed.pca.df, by = "meeting_id")
rownames(fed.df) <- fed.df$meeting_id

# Aggregate the topic variables to quarterly
fed.quartave <- aggregate(fed.df[,c(variablenames)], list(fed.df$quarter), mean)
colnames(fed.quartave) <- c("quarter", paste0(variablenames, "q"))
rownames(fed.quartave) <- fed.quartave$quarter

merged <- merge(fed.df, fed.quartave, by = "quarter", all.x = TRUE)
cor.test(merged$y1, merged$y1q)
cor.test(merged$y10, merged$y10q)

# Calculate principal components of the monthly averages
fed.pca.quart <- prcomp(fed.quartave[,paste0(var_used, 1:topicnumber, "q")], center = TRUE, scale. = TRUE)
summary(fed.pca.quart)
fed.pca.quart.df <- as.data.frame(fed.pca.quart$x)
colnames(fed.pca.quart.df) <- paste0(colnames(fed.pca.quart.df), "q")
fed.pca.quart.df$quarter <- as.Date(rownames(fed.pca.quart.df))
fed.df <- merge(fed.df, fed.pca.quart.df, by = "quarter", all.x = TRUE)
rownames(fed.df) <- fed.df$meeting_id


### EZ

# Principal components of each meeting's topic attention variables
ecb.pca <- prcomp(ecb.df[,variablenames], center = TRUE, scale. = TRUE)
summary(ecb.pca)
ecb.pca.df <- as.data.frame(ecb.pca$x)
ecb.pca.df$meeting_id <- rownames(ecb.pca.df)
ecb.df <- merge(ecb.df, ecb.pca.df, by = "meeting_id")
rownames(ecb.df) <- ecb.df$meeting_id

# Aggregate the topic variables to quarterly
ecb.quartave <- aggregate(ecb.df[,c(variablenames)], list(ecb.df$quarter), mean)
colnames(ecb.quartave) <- c("quarter", paste0(variablenames, "q"))
rownames(ecb.quartave) <- ecb.quartave$quarter

# Calculate principal components of the monthly averages
ecb.pca.quart <- prcomp(ecb.quartave[,paste0(var_used, 1:topicnumber, "q")], center = TRUE, scale. = TRUE)
summary(ecb.pca.quart)
ecb.pca.quart.df <- as.data.frame(ecb.pca.quart$x)
colnames(ecb.pca.quart.df) <- paste0(colnames(ecb.pca.quart.df), "q")
ecb.pca.quart.df$quarter <- as.Date(rownames(ecb.pca.quart.df))
ecb.df <- merge(ecb.df, ecb.pca.quart.df, by = "quarter", all.x = TRUE)
rownames(ecb.df) <- ecb.df$meeting_id


### Reorder before analysing
bank.df <- bank.df[order(bank.df$meet_date),]
fed.df <- fed.df[order(fed.df$meet_date),]
ecb.df <- ecb.df[order(ecb.df$meet_date),]


### Plot the PCs

ggplot() + 
  scale_color_manual("", values = c("BoE PC1" = "black", "Fed PC1" = "blue3", "ECB PC1" = "darkgoldenrod2",
                                    "BoE PC1q" = "black", "Fed PC1q" = "blue3", "ECB PC1q" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("BoE PC1" = 1, "Fed PC1" = 1, "ECB PC1" = 1,
                                    "BoE PC1q" = 2, "Fed PC1q" = 2, "ECB PC1q" = 2)) +
  geom_line(data = bank.df, aes(x = meet_date, y = PC1, color = "BoE PC1",linetype = "BoE PC1")) +
  geom_line(data = fed.df, aes(x = meet_date, y = PC1, color = "Fed PC1", linetype = "Fed PC1")) +
  geom_line(data = ecb.df, aes(x = meet_date, y = -PC1, color = "ECB PC1", linetype = "ECB PC1")) +
  geom_line(data = bank.df, aes(x = quarter, y = PC1q, color = "BoE PC1q",linetype = "BoE PC1q")) +
  geom_line(data = fed.df, aes(x = quarter, y = PC1q, color = "Fed PC1q",linetype = "Fed PC1q")) +
  geom_line(data = ecb.df, aes(x = quarter, y = PC1q, color = "ECB PC1q",linetype = "ECB PC1q")) +
  xlab('Date') +
  ylab("First Principal Component")
#ggtitle("GDP growth")
#ggsave(paste0(export_dir, "PCA/PC_acrossbanks_pc1.png"))



### Plot the PCs

ggplot() + 
  scale_color_manual("", values = c("BoE PC" = "black", "Fed PC" = "blue3", "ECB PC" = "darkgoldenrod2",
                                    "BoE PCq" = "black", "Fed PCq" = "blue3", "ECB PCq" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("BoE PC" = 1, "Fed PC" = 1, "ECB PC" = 1,
                                    "BoE PCq" = 2, "Fed PCq" = 2, "ECB PCq" = 2)) +
  geom_line(data = bank.df, aes(x = meet_date, y = PC1 + PC2, color = "BoE PC",linetype = "BoE PC")) +
  geom_line(data = fed.df, aes(x = meet_date, y = -PC1 - PC2, color = "Fed PC", linetype = "Fed PC")) +
  geom_line(data = ecb.df, aes(x = meet_date, y = PC1 + PC2, color = "ECB PC", linetype = "ECB PC")) +
  geom_line(data = bank.df, aes(x = quarter, y = -PC1q + PC2q, color = "BoE PCq",linetype = "BoE PCq")) +
  geom_line(data = fed.df, aes(x = quarter, y = PC1q + PC2q , color = "Fed PCq",linetype = "Fed PCq")) +
  geom_line(data = ecb.df, aes(x = quarter, y = PC1q + PC2q, color = "ECB PCq",linetype = "ECB PCq")) +
  xlab('Date') +
  ylab("First 2 Principal Components")
#ggtitle("GDP growth")



### Are the principal components correlated
all.data <- merge(ecb.pca.quart.df, fed.pca.quart.df, by = "quarter")
all.data <- merge(all.data, bank.pca.quart.df, by = "quarter")

cor.test(all.data$PC1q, all.data$PC1q.x)
cor.test(all.data$PC1q, all.data$PC1q.y)
cor.test(all.data$PC1q.x, all.data$PC1q.y)


# A little experiment showing that uncorrelated autoregressive series might appear correlated
shock1 <- rnorm(500, 0,1)
shock2 <- rnorm(500, 0,1)
cor.test(shock1,shock2)
x <- rep(0,500)
y <- rep(0, 500)
for (i in 2:500){
  x[i] <- 0.8*x[i-1] +shock1[i] 
  y[i] <- 0.8*y[i-1] +shock2[i]
}
cor.test(x,y)



############################# Compare PCs to macro series ############################# 


### BoE text and UK data
cor.test(bank.df$PC1, bank.df$inflation_monthly)
cor.test(bank.df$PC1, bank.df$inflation_quarterly)
cor.test(bank.df$PC1, bank.df$growth)
cor.test(bank.df$PC2, bank.df$inflation_monthly)
cor.test(bank.df$PC2, bank.df$inflation_quarterly)
cor.test(bank.df$PC2, bank.df$growth)
cor.test(bank.df$PC3, bank.df$inflation_monthly)
cor.test(bank.df$PC3, bank.df$inflation_quarterly)
cor.test(bank.df$PC3, bank.df$growth)

bank.lm.inflationm_1 <- lm(inflation_monthly ~ PC1, data = bank.df)
summary(bank.lm.inflationm_1)
bank.lm.inflationm_123 <- lm(inflation_monthly ~ PC1 + PC2 + PC3, data = bank.df)
summary(bank.lm.inflationm_123)
bank.lm.inflationq_1 <- lm(inflation_quarterly ~ PC1, data = bank.df)
summary(bank.lm.inflationq_1)
bank.lm.inflationq_123 <- lm(inflation_quarterly ~ PC1 + PC2 + PC3, data = bank.df)
summary(bank.lm.inflationq_123)
bank.lm.growth_1 <- lm(growth ~ PC1, data = bank.df)
summary(bank.lm.growth_1)
bank.lm.growth_123 <- lm(growth ~ PC1 + PC2 + PC3, data = bank.df)
summary(bank.lm.growth_123)

stargazer(bank.lm.inflationm_1, bank.lm.inflationm_123, bank.lm.inflationq_1, bank.lm.inflationq_123,
          bank.lm.growth_1, bank.lm.growth_123, title = "BoE attention Principal Components and Macro variables",
          df = FALSE)


### Fed text and US data
cor.test(fed.df$PC1, fed.df$inflation_monthly)
cor.test(fed.df$PC1, fed.df$inflation_quarterly)
cor.test(fed.df$PC1, fed.df$growth)
cor.test(fed.df$PC2, fed.df$inflation_monthly)
cor.test(fed.df$PC2, fed.df$inflation_quarterly)
cor.test(fed.df$PC2, fed.df$growth)
cor.test(fed.df$PC3, fed.df$inflation_monthly)
cor.test(fed.df$PC3, fed.df$inflation_quarterly)
cor.test(fed.df$PC3, fed.df$growth)

fed.lm.inflationm_1 <- lm(inflation_monthly ~ PC1, data = fed.df)
summary(fed.lm.inflationm_1)
fed.lm.inflationm_123 <- lm(inflation_monthly ~ PC1 + PC2, data = fed.df)
summary(fed.lm.inflationm_123)
fed.lm.inflationq_1 <- lm(inflation_quarterly ~ PC1, data = fed.df)
summary(fed.lm.inflationq_1)
fed.lm.inflationq_123 <- lm(inflation_quarterly ~ PC1 + PC2, data = fed.df)
summary(fed.lm.inflationq_123)
fed.lm.growth_1 <- lm(growth ~ PC1, data = fed.df)
summary(fed.lm.growth_1)
fed.lm.growth_123 <- lm(growth ~ PC1 + PC2, data = fed.df)
summary(fed.lm.growth_123)

stargazer(fed.lm.inflationm_1, fed.lm.inflationm_123, fed.lm.inflationq_1, fed.lm.inflationq_123,
          fed.lm.growth_1, fed.lm.growth_123, title = "Frd attention Principal Components and Macro variables",
          df = FALSE)


### ECB text and EZ data
cor.test(ecb.df$PC1, ecb.df$inflation_monthly)
cor.test(ecb.df$PC1, ecb.df$inflation_quarterly)
cor.test(ecb.df$PC1, ecb.df$growth)
cor.test(ecb.df$PC2, ecb.df$inflation_monthly)
cor.test(ecb.df$PC2, ecb.df$inflation_quarterly)
cor.test(ecb.df$PC2, ecb.df$growth)
cor.test(ecb.df$PC3, ecb.df$inflation_monthly)
cor.test(ecb.df$PC3, ecb.df$inflation_quarterly)
cor.test(ecb.df$PC3, ecb.df$growth)

ecb.lm.inflationm_1 <- lm(inflation_monthly ~ PC1, data = ecb.df)
summary(ecb.lm.inflationm_1)
ecb.lm.inflationm_123 <- lm(inflation_monthly ~ PC1 + PC2 + PC3, data = ecb.df)
summary(ecb.lm.inflationm_123)
ecb.lm.inflationq_1 <- lm(inflation_quarterly ~ PC1, data = ecb.df)
summary(ecb.lm.inflationq_1)
ecb.lm.inflationq_123 <- lm(inflation_quarterly ~ PC1 + PC2 + PC3, data = ecb.df)
summary(ecb.lm.inflationq_123)
ecb.lm.growth_1 <- lm(growth ~ PC1, data = ecb.df)
summary(ecb.lm.growth_1)
ecb.lm.growth_123 <- lm(growth ~ PC1 + PC2 + PC3, data = ecb.df)
summary(ecb.lm.growth_123)

stargazer(ecb.lm.inflationm_1, ecb.lm.inflationm_123, ecb.lm.inflationq_1, ecb.lm.inflationq_123,
          ecb.lm.growth_1, ecb.lm.growth_123, title = "ECB attention Principal Components and Macro variables",
          df = FALSE, table.placement = "H")



bank.info <- bank.df[,c("quarter", "month", "meeting_id", "central_bank", "growth", "inflation_quarterly", 
                        "inflation_monthly", "policy_rate", "PC1", "PC1q")]
fed.info <- fed.df[,c("quarter", "month", "meeting_id", "central_bank", "growth", "inflation_quarterly", 
                      "inflation_monthly", "policy_rate", "PC1", "PC1q")]
ecb.info <- ecb.df[,c("quarter", "month", "meeting_id", "central_bank", "growth", "inflation_quarterly", 
                      "inflation_monthly", "policy_rate", "PC1", "PC1q")]

all.info <- rbind(bank.info, fed.info, ecb.info)

### Write the df with macro data to file
write.csv(all.info, file = paste0(clean_dir, "CBC/meeting_details.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)
#all.info <- read.csv(paste0(clean_dir, "CBC/meeting_details.csv"), encoding = "utf-8", stringsAsFactors = FALSE)




############################# Plot some examples ############################# 

inf.mean <- mean(bank.df$inflation_monthly)
inf.var <- sd(bank.df$inflation_monthly)
PC1.mean <- mean(bank.df$PC1)
PC1.var <- sd(bank.df$PC1)

ggplot() + 
  scale_color_manual("",
                     values = c("BoE PC1" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2", "UK Inflation" = "red")) +
  geom_line(data = bank.df, aes(x = meet_date, y = -(PC1 - PC1.mean)/PC1.var, color = "BoE PC1")) +
  #geom_line(data = bank.df, aes(x = meet_date, y = -PC2- PC1, color = "BoE")) +
  geom_line(data = bank.df, aes(x= meet_date, y = (inflation_monthly - inf.mean)/inf.var, color = "UK Inflation")) +
  xlab('Meeting date') +
  ylab("Normalised value") + 
  #scale_y_continuous("BoE PC1",sec.axis = sec_axis(~ (. /inf.var) + (inf.mean/inf.var) , name = "UK Inflation (monthly)")) +
  ggtitle("First Principal Component of BoE minutes and Inflation (CPI)")
ggsave(paste0(export_dir, "PCA/BoE_PC1_Inflation.png"))










############################# End  ############################# 