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
require(readxl)
require(openxlsx)
require(lubridate)
require(zoo)


### Define the directories where raw data is stored and clean will be saved
raw_dir <- "~/Documents/DPhil/Raw_Data/"
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


### Import the SPF-text data panel

import_filename <- paste(clean_dir, "topics_forecasts_panel.csv", sep = "/")
total.panel <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)






############################# Inflation data ############################# 

# Import the weekly article data, averaged over articles
import_filename = paste(raw_dir, "MSC/Price_change_quarterly-2019-Apr-23.csv", sep = "/")
inflation_exp <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE, skip = 1)

quarter <- paste(" ", as.character(inflation_exp$Quarter))
quarter <- str_replace_all(quarter, " 1", "01-01")
quarter <- str_replace_all(quarter, " 2", "04-01")
quarter <- str_replace_all(quarter, " 3", "07-01")
quarter <- str_replace_all(quarter, " 4", "10-01")
quarter <- str_trim(quarter)

date <- paste(as.character(inflation_exp$Year), quarter, sep = "-")

inflation_exp$quarter <- as.Date(date)
inflation_exp$msc_disp <- inflation_exp$Interquartile.Range..75th.25th.

inflation_exp <- inflation_exp %>%
  select(quarter, msc_disp) %>%
  filter(!is.na(msc_disp))

inflation_exp$msc_mean <- mean(inflation_exp$msc_disp)
inflation_exp$msc_sd <- sd(inflation_exp$msc_disp)
inflation_exp$msc_disp <- (inflation_exp$msc_disp - inflation_exp$msc_mean)/inflation_exp$msc_sd

inflation.df <- total.panel %>%
  filter(series == "CPI")

inflation.df$quarter <- as.Date(inflation.df$quarter)
inflation_exp$quarter <- as.Date(inflation_exp$quarter)

inflation.df <- merge(inflation.df, inflation_exp, by = "quarter", all.x = TRUE)
inflation.df$quarter <- as.Date(inflation.df$quarter)

ggplot(inflation.df, aes(x = quarter)) + 
  scale_color_manual("Legend",
                      values = c("SPF dispersion" = "black", "Fed minutes" = "blue3", 
                      "NYT articles" = "darkgoldenrod2", "MSC dispersion" = "red")) +
  geom_line(aes(y = msc_disp, color = "MSC dispersion")) +
  geom_line(aes(y = dispersion, color = "SPF dispersion")) + 
  geom_line(aes(y = fed_std, color = "Fed minutes")) +
  geom_line(aes(y = news_std, color = "NYT articles")) +
  ggtitle("Inflation topic attention, SPF and MSC and expectation dispersion")
ggsave(paste0(export_dir, "SPF_topics/CPIdisp_topic9_MSC.png"))

cor.test(inflation.df$dispersion, inflation.df$msc_disp)
cor.test(inflation.df$fed_std, inflation.df$msc_disp)
cor.test(inflation.df$news_std, inflation.df$msc_disp)















############################# Unemployment data ############################# 

# Import the weekly MSC data data, averaged over articles
import_filename = paste(raw_dir, "MSC/Unemployment_quarterly-2019-Apr-23.csv", sep = "/")
unemp_exp <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE, skip = 1)

quarter <- paste(" ", as.character(unemp_exp$Quarter))
quarter <- str_replace_all(quarter, " 1", "01-01")
quarter <- str_replace_all(quarter, " 2", "04-01")
quarter <- str_replace_all(quarter, " 3", "07-01")
quarter <- str_replace_all(quarter, " 4", "10-01")
quarter <- str_trim(quarter)

date <- paste(as.character(unemp_exp$Year), quarter, sep = "-")

unemp_exp$quarter <- as.Date(date)

### No I-Q range available so minus the absolute difference between less and more
unemp_exp$msc_disp <- unemp_exp$DK..NA
unemp_exp$msc_disp <- -abs(unemp_exp$Less - unemp_exp$More)

unemp_exp <- unemp_exp %>%
  select(quarter, msc_disp) %>%
  filter(!is.na(msc_disp))

unemp_exp$msc_mean <- mean(unemp_exp$msc_disp)
unemp_exp$msc_sd <- sd(unemp_exp$msc_disp)
unemp_exp$msc_disp <- (unemp_exp$msc_disp - unemp_exp$msc_mean)/unemp_exp$msc_sd

unemp.df <- total.panel %>%
  filter(series == "UNEMP")

unemp.df$quarter <- as.Date(unemp.df$quarter)
unemp_exp$quarter <- as.Date(unemp_exp$quarter)

unemp.df <- merge(unemp.df, unemp_exp, by = "quarter", all.x = TRUE)
unemp.df$quarter <- as.Date(unemp.df$quarter)

ggplot(unemp.df, aes(x = quarter)) + 
  scale_color_manual("Legend",
                     values = c("SPF dispersion" = "black", "Fed minutes" = "blue3", 
                                "NYT articles" = "darkgoldenrod2", "MSC dispersion" = "red")) +
  geom_line(aes(y = msc_disp, color = "MSC dispersion")) +
  geom_line(aes(y = dispersion, color = "SPF dispersion")) + 
  geom_line(aes(y = fed, color = "Fed minutes")) +
  geom_line(aes(y = news, color = "NYT articles")) +
  ggtitle("Employment topic attention, SPF and MSC and expectation dispersion")
ggsave(paste0(export_dir, "SPF_topics/UNEMPdisp_topic16_MSC.png"))

cor.test(unemp.df$dispersion, unemp.df$msc_disp)
cor.test(unemp.df$fed, unemp.df$msc_disp)
cor.test(unemp.df$news, unemp.df$msc_disp)

















############################# Interest rate data ############################# 

# Import the weekly MSC data data, averaged over articles
import_filename = paste(raw_dir, "MSC/Interest_rates_quarterly-2019-Apr-23.csv", sep = "/")
rates_exp <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE, skip = 1)

quarter <- paste(" ", as.character(rates_exp$Quarter))
quarter <- str_replace_all(quarter, " 1", "01-01")
quarter <- str_replace_all(quarter, " 2", "04-01")
quarter <- str_replace_all(quarter, " 3", "07-01")
quarter <- str_replace_all(quarter, " 4", "10-01")
quarter <- str_trim(quarter)

date <- paste(as.character(rates_exp$Year), quarter, sep = "-")

rates_exp$quarter <- as.Date(date)

### No I-Q range available so minus the absolute difference between less and more
rates_exp$msc_disp <- rates_exp$DK..NA
rates_exp$msc_disp <- -abs(rates_exp$Go.Up - rates_exp$Go.Down)

rates_exp <- rates_exp %>%
  select(quarter, msc_disp) %>%
  filter(!is.na(msc_disp))

rates_exp$msc_mean <- mean(rates_exp$msc_disp)
rates_exp$msc_sd <- sd(rates_exp$msc_disp)
rates_exp$msc_disp <- (rates_exp$msc_disp - rates_exp$msc_mean)/rates_exp$msc_sd

rates.df <- total.panel %>%
  filter(series == "TBILL")

rates.df$quarter <- as.Date(rates.df$quarter)
rates_exp$quarter <- as.Date(rates_exp$quarter)

rates.df <- merge(rates.df, rates_exp, by = "quarter", all.x = TRUE)
rates.df$quarter <- as.Date(rates.df$quarter)

ggplot(rates.df, aes(x = quarter)) + 
  scale_color_manual("Legend",
                     values = c("SPF dispersion" = "black", "Fed minutes" = "blue3", 
                                "NYT articles" = "darkgoldenrod2", "MSC dispersion" = "red")) +
  geom_line(aes(y = msc_disp, color = "MSC dispersion")) +
  geom_line(aes(y = dispersion, color = "SPF dispersion")) + 
  geom_line(aes(y = fed, color = "Fed minutes")) +
  geom_line(aes(y = news, color = "NYT articles")) +
  ggtitle("Interest rate topic attention, SPF and MSC and expectation dispersion")
ggsave(paste0(export_dir, "SPF_topics/TBILLdisp_topic24_MSC.png"))

cor.test(rates.df$dispersion, rates.df$msc_disp)
cor.test(rates.df$fed, rates.df$msc_disp)
cor.test(rates.df$news, rates.df$msc_disp)
