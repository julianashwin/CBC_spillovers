####
# This file combines SPF-topic panel with Fed greenbook forecasts where available
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
require(readxl)
require(openxlsx)
require(lubridate)
require(zoo)
require(dplyr)


### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/"
export_dir <- "figures/"

# Import 
import_filename = paste(clean_dir, "topics_forecasts_panel.csv", sep = "/")
panel_df <- read.csv(import_filename, stringsAsFactors = FALSE)

# Define correspondence between SPF and Greenbook variables
spf2gb_df <- data.frame(series = unique(panel_df$series))
spf2gb_df$GB_series <- NA
spf2gb_df$GB_series[which(spf2gb_df$series == "NGDP")] <- "gNGDP"
spf2gb_df$GB_series[which(spf2gb_df$series == "RGDP")] <- "gRGDP"
spf2gb_df$GB_series[which(spf2gb_df$series == "CPI")] <- "gPCPI"
spf2gb_df$GB_series[which(spf2gb_df$series == "UNEMP")] <- "UNEMP"
spf2gb_df$GB_series[which(spf2gb_df$series == "INDPROD")] <- "gIP"
spf2gb_df$GB_series[which(spf2gb_df$series == "HOUSING")] <- "HSTART"
spf2gb_df$GB_series[which(spf2gb_df$series == "RRESINV")] <- "gRRES"
spf2gb_df$GB_series[which(spf2gb_df$series == "RNRESINV")] <- "gRBF"
spf2gb_df$GB_series[which(spf2gb_df$series == "RFEDGOV")] <- "gRGOVF"
spf2gb_df$GB_series[which(spf2gb_df$series == "RSLGOV")] <- "gRGOVSL"

# Import greenbook data

variable <- "gRGDP"
spf_variable <- spf2gb_df$series[which(spf2gb_df$GB_series == variable)]

import_filename <- paste0(clean_dir, "GreenBook_Row_Format.xlsx")
greenbook_df <- read_xlsx(import_filename, sheet = variable, skip = 0)

# Convert to date
year <- str_sub(greenbook_df$DATE, 1,4)
quarter <- paste0("Q",str_sub(greenbook_df$DATE, 6,6))
quarter <- str_replace(quarter, "Q1", "01")
quarter <- str_replace(quarter, "Q2", "04")
quarter <- str_replace(quarter, "Q3", "07")
quarter <- str_replace(quarter, "Q4", "10")

date <- paste(year, quarter, "01", sep = "-")
greenbook_df$Date <- as.Date(date)
# Keep only first value for each month
greenbook_df <- greenbook_df %>% group_by(Date) %>% 
  slice(2) %>% ungroup()
# Order by date again
greenbook_df <- greenbook_df[which(greenbook_df$Date >= "1993-01-01"),]
greenbook_df <- as.data.frame(greenbook_df[order(greenbook_df$Date),])
# Get the true data out from the lags
greenbook_df[,variable] <- NA
greenbook_df[1:(nrow(greenbook_df)-2),variable] <- 
  greenbook_df[3:nrow(greenbook_df),paste0(variable, "B2")]



# Import the SPF data (note that NGDP2 is the nowcast here)

import_filename <- paste0(clean_dir, "GreenBook_Row_Format.xlsx")
spf_df <- read_xlsx(import_filename, sheet = variable, skip = 0)



ggplot(greenbook_df) + theme_bw() + 
  geom_line(aes(x = Date, y = gRGDP, color = "Actual")) +
  geom_line(aes(x = Date, y = gRGDPF0, color = "GB forecast")) 




