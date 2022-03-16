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
spf2gb_df$version <- NA
spf2gb_df[which(spf2gb_df$series == "NGDP"), c("GB_series", "version")] <- c("gNGDP",2)
spf2gb_df[which(spf2gb_df$series == "RGDP"), c("GB_series", "version")] <- c("gRGDP",2)
spf2gb_df[which(spf2gb_df$series == "CPI"), c("GB_series", "version")] <- c("gPCPI",1)
spf2gb_df[which(spf2gb_df$series == "UNEMP"), c("GB_series", "version")] <- c("UNEMP",1)
spf2gb_df[which(spf2gb_df$series == "INDPROD"), c("GB_series", "version")] <- c("gIP",2)
spf2gb_df[which(spf2gb_df$series == "HOUSING"), c("GB_series", "version")] <- c("HSTART",1)
spf2gb_df[which(spf2gb_df$series == "RRESINV"), c("GB_series", "version")] <- c("gRRES",2)
spf2gb_df[which(spf2gb_df$series == "RNRESINV"), c("GB_series", "version")] <- c("gRBF",2)
spf2gb_df[which(spf2gb_df$series == "RFEDGOV"), c("GB_series", "version")] <- c("gRGOVF",2)
spf2gb_df[which(spf2gb_df$series == "RSLGOV"), c("GB_series", "version")] <- c("gRGOVSL",2)



all_nowcasts_df <- data.frame(matrix(NA, nrow = 0, ncol = 5))
names(all_nowcasts_df) <- c("series", "quarter","variable_act", "GB_nowcast", "SPF_nowcast")
# Import greenbook data
for (ii in 1:nrow(spf2gb_df)){
  GB_sheet <- spf2gb_df$GB_series[ii]
  SPF_sheet <- spf2gb_df$series[ii]
  vers <- spf2gb_df$version[ii]
  if (!is.na(spf2gb_df$GB_series[ii])){
    
    print(paste("Import GB data from", GB_sheet))
    import_filename <- paste0(clean_dir, "GreenBook_Row_Format.xlsx")
    gb_df <- read_xlsx(import_filename, sheet = GB_sheet, skip = 0)
    # Convert to date
    year <- str_sub(gb_df$DATE, 1,4)
    quarter <- paste0("Q",str_sub(gb_df$DATE, 6,6))
    quarter <- str_replace(quarter, "Q1", "01")
    quarter <- str_replace(quarter, "Q2", "04")
    quarter <- str_replace(quarter, "Q3", "07")
    quarter <- str_replace(quarter, "Q4", "10")
    date <- paste(year, quarter, "01", sep = "-")
    gb_df$quarter <- as.Date(date)
    # If there is NA in second entry of quarter for B2, use first entry of quarter
    gb_df <- data.frame(gb_df[which(gb_df$DATE > 1980),])
    gb_df[which(is.na(gb_df[,paste0(GB_sheet, "B2")])), paste0(GB_sheet, "B2")] <- 
      gb_df[which(is.na(gb_df[,paste0(GB_sheet, "B2")]))-1, paste0(GB_sheet, "B2")]
    # Keep only second value for each month
    gb_df <- gb_df %>% group_by(quarter) %>% 
      slice(2) %>% ungroup()
    # Order by date again
    gb_df <- gb_df[which(gb_df$quarter >= "1993-01-01"),]
    gb_df <- as.data.frame(gb_df[order(gb_df$quarter),])
    # Get the true data out from the lags
    gb_df[, "variable_act"] <- NA
    gb_df[1:(nrow(gb_df)-2), "variable_act"] <- 
      gb_df[3:nrow(gb_df), paste0(GB_sheet, "B2")]
    
    
    gb_df[,"GB_nowcast"] <- gb_df[, paste0(GB_sheet, "F0")]
    gb_df <- gb_df[, c("quarter", "variable_act", "GB_nowcast")]
  
    print(paste("Import SPF data from", SPF_sheet))
    if (vers == 1){
      import_filename <- paste0(clean_dir, "SPF/medianLevel.xlsx")
    } else {
      import_filename <- paste0(clean_dir, "SPF/medianGrowth.xlsx")
    }
    spf_df <- read_xlsx(import_filename, sheet = SPF_sheet, skip = 0)
    # Convert to date
    year <- str_sub(spf_df$YEAR, 1,4)
    quarter <- paste0("Q",spf_df$QUARTER)
    quarter <- str_replace(quarter, "Q1", "01")
    quarter <- str_replace(quarter, "Q2", "04")
    quarter <- str_replace(quarter, "Q3", "07")
    quarter <- str_replace(quarter, "Q4", "10")
    date <- paste(year, quarter, "01", sep = "-")
    spf_df$quarter <- as.Date(date)
    # Keep only first value for each month
    spf_df <- spf_df %>% group_by(quarter) %>% 
      slice(1) %>% ungroup()
    # Order by date again
    spf_df <- spf_df[which(spf_df$quarter >= "1993-01-01"),]
    spf_df <- as.data.frame(spf_df[order(spf_df$quarter),])
    # Identify the median SPF forecast
    if (vers == 1){
      spf_df[,"SPF_nowcast"] <- as.numeric(spf_df[, paste0(SPF_sheet, "2")])
    } else {
      spf_df[,"SPF_nowcast"] <- as.numeric(spf_df[, paste0("d", tolower(SPF_sheet), "2")])
    }
    spf_df <- spf_df[, c("quarter", "SPF_nowcast")]
    
    nowcast_df <- merge(gb_df, spf_df, by = "quarter", all.x = TRUE)
    nowcast_df$quarter <- as.Date(nowcast_df$quarter)
    nowcast_df$series <- SPF_sheet
    
    # Plot to check they look sensible
    ggplot(nowcast_df) + theme_bw() + guides(color=guide_legend(title="Legend")) +
      geom_line(aes(x = quarter, y = variable_act, color = "Actual")) +
      geom_line(aes(x = quarter, y = GB_nowcast, color = "GB forecast"), linetype = "dashed") +
      geom_line(aes(x = quarter, y = SPF_nowcast, color = "SPF forecast"), linetype = "dashed") +
      ylab(SPF_sheet) + xlab("Date") + 
      ggtitle(paste("Nowcasts for", SPF_sheet))
    ggsave(paste0("figures/nowcasts/", SPF_sheet, ".pdf"), width = 6, height = 3)
    
    # Append to all_nowcasts_df
    all_nowcasts_df <- rbind(
      all_nowcasts_df, nowcast_df[,c("series", "quarter","variable_act", "GB_nowcast", "SPF_nowcast")])

  }
}

ggplot(all_nowcasts_df) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_line(aes(x = quarter, y = variable_act, color = "Actual")) + 
  geom_line(aes(x = quarter, y = GB_nowcast, color = "GB forecast"), linetype = "dashed") +
  geom_line(aes(x = quarter, y = SPF_nowcast, color = "SPF forecast"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_nowcasts.pdf", width = 8, height = 6)

ggplot(all_nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_line(aes(y = abs(variable_act - GB_nowcast), color = "GB error"), linetype = "dashed") +
  geom_smooth(aes(y = abs(variable_act - GB_nowcast), color = "GB error"), method = "loess") +
  geom_line(aes(y = abs(variable_act - SPF_nowcast), color = "SPF error"), linetype = "dashed") +
  geom_smooth(aes(y = abs(variable_act - SPF_nowcast), color = "SPF error"), method = "loess") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_errors.pdf", width = 8, height = 6)


# Merge nowcasts with the topics and dispersion measures
df <- panel_df[,c("series", "quarter", "fed", "news",  "fed_std", "news_std", 
                  "dispersion", "dispersion_std")]
df$quarter <- as.Date(df$quarter)
all_nowcasts_df$quarter <- as.Date(all_nowcasts_df$quarter)
df <- merge(df, all_nowcasts_df, by = c("series", "quarter"), all.x = T)

ggplot(df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_smooth(aes(y = abs(variable_act - GB_nowcast), color = "GB error"), method = "loess") +
  geom_smooth(aes(y = abs(variable_act - SPF_nowcast), color = "SPF error"), method = "loess") +
  geom_smooth(aes(y = abs(dispersion), color = "SPF dispersion"), method = "loess") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_errors.pdf", width = 8, height = 6)






