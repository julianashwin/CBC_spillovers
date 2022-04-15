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


### Import growth and inflation data for each economy


### UK data

# UK growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/UK_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$UK_growth <- 4*import.df[,2] # Annualise the growth rates by multiplying by 4
macro.df <- import.df[,c("quarter", "UK_growth")]

# UK quarterly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/UK_inflation_quarterly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$UK_inflation_q <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("quarter", "UK_inflation_q")], by = "quarter", all.x = TRUE)

# UK monthly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/UK_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$month <- as.Date(import.df$DATE)
import.df$UK_inflation_m <- import.df[,2]
import.df$quarter <- floor_date(import.df$month, unit = "quarter")
macro.df <- merge(macro.df, import.df[,c("month", "quarter", "UK_inflation_m")], by = "quarter", all.x = TRUE, all.y = TRUE)


### US data

# US growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$US_growth <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("quarter", "US_growth")], by = "quarter", all.x = TRUE)

# US quarterly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_inflation_quarterly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$US_inflation_q <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("quarter", "US_inflation_q")], by = "quarter", all.x = TRUE)

# US monthly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$month <- as.Date(import.df$DATE)
import.df$US_inflation_m <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("month", "US_inflation_m")], by = "month", all.x = TRUE)


### EZ data

# EZ growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/EZ_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$EZ_growth <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("quarter", "EZ_growth")], by = "quarter", all.x = TRUE)

# EZ quarterly inflation (should be fine to use the monthly here as it's growth from same period, previous year)
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/EZ_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$EZ_inflation_q <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("quarter", "EZ_inflation_q")], by = "quarter", all.x = TRUE)

# EZ monthly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/EZ_inflation_monthly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$month <- as.Date(import.df$DATE)
import.df$EZ_inflation_m <- import.df[,2]
macro.df <- merge(macro.df, import.df[,c("month", "EZ_inflation_m")], by = "month", all.x = TRUE)



### Import policy rates for each bank from BIS database
import_filename <- paste(raw_dir, "Macro_data/BIS_website/cbpol_1902.xlsx", sep = "")
import.df <- read_xlsx(import_filename, sheet = "Monthly Series", skip = 3)
import.df <- as.data.frame(import.df)
import.df$Period <- as.Date(import.df$Period)
import.df$month <- floor_date(import.df$Period, "month")
import.df$US_policy_rate <- import.df[,"M:US"]
import.df$UK_policy_rate <- import.df[,"M:GB"]
import.df$EZ_policy_rate <- import.df[,"M:XM"]

import.df <- import.df[,c("month", "US_policy_rate", "UK_policy_rate", "EZ_policy_rate")]


macro.df <- merge(macro.df, import.df, by = "month", all.x = TRUE)

macro.df$month <- as.Date(macro.df$month)
macro.df$quarter <- as.Date(macro.df$quarter)
macro.df <- macro.df[order(macro.df$month),]

macro.df$ind <- 1
macro.df$period <- as.integer(as.factor(macro.df$month))

macro.df <- pdata.frame(data.frame(macro.df), index = c("ind", "period"))

macro.df$UK_policy_rate_change <- macro.df$UK_policy_rate - plm::lag(macro.df$UK_policy_rate)
macro.df$US_policy_rate_change <- macro.df$US_policy_rate - plm::lag(macro.df$US_policy_rate)
macro.df$EZ_policy_rate_change <- macro.df$EZ_policy_rate - plm::lag(macro.df$EZ_policy_rate)

macro.df <- select(macro.df, -ind, -period)

macro.df <- macro.df[which(macro.df$quarter >= "1997-01-01"),]





# Plot each of the series as a sense check
ggplot() + 
  scale_color_manual("",
                     values = c("UK growth" = "black", "US growth" = "blue3", "EZ growth" = "darkgoldenrod2")) +
  geom_line(data = macro.df, aes(x = quarter, y = UK_growth, color = "UK growth")) +
  geom_line(data = macro.df, aes(x = quarter, y = US_growth, color = "US growth")) +
  geom_line(data = macro.df, aes(x= quarter, y = EZ_growth, color = "EZ growth")) +
  xlab('Date') +
  ylab("GDP growth (same period, previous year")
  #ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/Growth_rates.png"))

ggplot() + 
  scale_color_manual("", values = c("UK inflation (monthly)" = "black", "US inflation (monthly)" = "blue3", "EZ inflation (monthly)" = "darkgoldenrod2",
                                "UK inflation (quarterly)" = "black", "US inflation (quarterly)" = "blue3", "EZ inflation (quarterly)" = "darkgoldenrod2")) +
  scale_linetype_manual("",values=c("UK inflation (monthly)" = 1, "US inflation (monthly)" = 1, "EZ inflation (monthly)" = 1,
                                             "UK inflation (quarterly)" = 2, "US inflation (quarterly)" = 2, "EZ inflation (quarterly)" = 2)) +
  geom_line(data = macro.df, aes(x = month, y = UK_inflation_m, color = "UK inflation (monthly)",linetype = "UK inflation (monthly)")) +
  geom_line(data = macro.df, aes(x = quarter, y = UK_inflation_q, color = "UK inflation (quarterly)", linetype = "UK inflation (quarterly)")) +
  geom_line(data = macro.df, aes(x = month, y = US_inflation_m, color = "US inflation (monthly)", linetype = "US inflation (monthly)")) +
  geom_line(data = macro.df, aes(x = quarter, y = US_inflation_q, color = "US inflation (quarterly)", linetype = "US inflation (quarterly)")) +
  geom_line(data = macro.df, aes(x = month, y = EZ_inflation_m, color = "EZ inflation (monthly)", linetype = "EZ inflation (monthly)")) +
  geom_line(data = macro.df, aes(x = quarter, y = EZ_inflation_q, color = "EZ inflation (quarterly)", linetype = "EZ inflation (quarterly)")) +
  xlab('Date') +
  ylab("CPI inflation (same period, previous year)")
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/Inflation_rates.png"))

ggplot() + 
  scale_color_manual("",
                     values = c("UK policy rate" = "black", "US policy rate" = "blue3", "EZ policy rate" = "darkgoldenrod2")) +
  geom_line(data = macro.df, aes(x = month, y = UK_policy_rate, color = "UK policy rate")) +
  geom_line(data = macro.df, aes(x = month, y = US_policy_rate, color = "US policy rate")) +
  geom_line(data = macro.df, aes(x= month, y = EZ_policy_rate, color = "EZ policy rate")) +
  xlab('Date') +
  ylab("Policy rate (percent, from BIS)")
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/Policy_rates.png"))

ggplot() + 
  scale_color_manual("",
                     values = c("UK policy rate change" = "black", "US policy rate change" = "blue3", "EZ policy rate change" = "darkgoldenrod2")) +
  geom_line(data = macro.df, aes(x = month, y = UK_policy_rate_change, color = "UK policy rate change")) +
  geom_line(data = macro.df, aes(x = month, y = US_policy_rate_change, color = "US policy rate change")) +
  geom_line(data = macro.df, aes(x= month, y = EZ_policy_rate_change, color = "EZ policy rate change")) +
  xlab('Date') +
  ylab("Policy rate change (percent, from BIS)")
#ggtitle("GDP growth")
ggsave(paste0(export_dir, "Macro_series/Policy_rate_changes.png"))






### Write to csv

clean_filename = paste(clean_dir, "CBC/macro_data.csv", sep = "/")
write.csv(macro.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)






############################# End  ############################# 