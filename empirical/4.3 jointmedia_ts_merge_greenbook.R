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

# Import the SPF panel data
import_filename = paste(clean_dir, "topics_forecasts_panel.csv", sep = "/")
panel_df <- read.csv(import_filename, stringsAsFactors = FALSE)

panel_df$quarter <- as.Date(panel_df$quarter)
ggplot(panel_df[which(panel_df$series == "CPI"),]) + theme_bw() + 
  geom_line(aes(x = quarter, y = fed_std, color = "Fed")) + 
  geom_line(aes(x = quarter, y = news_std, color = "NYT")) + 
  geom_line(aes(x = quarter, y = dispersion_std, color = "disp"))

# Define correspondence between SPF and Greenbook variables
spf2gb_df <- data.frame(series = unique(panel_df$series))
spf2gb_df$GB_series <- NA
spf2gb_df$version <- 1
spf2gb_df$topic <- 0
spf2gb_df[which(spf2gb_df$series == "NGDP"), c("GB_series", "version", "topic")] <- c("gNGDP",2, 20)
spf2gb_df[which(spf2gb_df$series == "RGDP"), c("GB_series", "version", "topic")] <- c("gRGDP",2, 20)
spf2gb_df[which(spf2gb_df$series == "CPI"), c("GB_series", "version", "topic")] <- c("gPCPI",1, 9)
spf2gb_df[which(spf2gb_df$series == "UNEMP"), c("GB_series", "version", "topic")] <- c("UNEMP",1, 16)
spf2gb_df[which(spf2gb_df$series == "EMP"), c("GB_series", "version", "topic")] <- c("UNEMP",1, 16)
spf2gb_df[which(spf2gb_df$series == "CPROF"), c("GB_series", "version", "topic")] <- c(NA,1, 23)
spf2gb_df[which(spf2gb_df$series == "INDPROD"), c("GB_series", "version", "topic")] <- c("gIP",2, 29)
spf2gb_df[which(spf2gb_df$series == "HOUSING"), c("GB_series", "version", "topic")] <- c("HSTART",1, 26)
spf2gb_df[which(spf2gb_df$series == "RRESINV"), c("GB_series", "version", "topic")] <- c("gRRES",2, 22)
spf2gb_df[which(spf2gb_df$series == "RNRESIN"), c("GB_series", "version", "topic")] <- c("gRBF",2, 15)
spf2gb_df[which(spf2gb_df$series == "RCONSUM"), c("GB_series", "version", "topic")] <- c("gRPCE",2, 19)
spf2gb_df[which(spf2gb_df$series == "RFEDGOV"), c("GB_series", "version", "topic")] <- c("gRGOVF",2, 3)
spf2gb_df[which(spf2gb_df$series == "RSLGOV"), c("GB_series", "version", "topic")] <- c("gRGOVSL",2, 3)



all_nowcasts_df <- data.frame(matrix(NA, nrow = 0, ncol = 5))
names(all_nowcasts_df) <- c("series", "quarter", "topic", "variable_act", "GB_nowcast", "SPF_nowcast",
                            "GB_forecast1", "SPF_forecast1")
# Import greenbook data
for (ii in 1:nrow(spf2gb_df)){
  GB_sheet <- spf2gb_df$GB_series[ii]
  SPF_sheet <- spf2gb_df$series[ii]
  vers <- spf2gb_df$version[ii]
  
  if (SPF_sheet == "EMP"){
    SPF_series <- "UNEMP"
  } else {
    SPF_series <- SPF_sheet 
  }
  
  print(paste("Import SPF data from", SPF_sheet))
  if (vers == 1){
    import_filename <- paste0(clean_dir, "SPF/meanLevel.xlsx")
  } else {
    import_filename <- paste0(clean_dir, "SPF/meanGrowth.xlsx")
  }
  spf_df <- read_xlsx(import_filename, sheet = SPF_series, skip = 0)
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
  spf_df <- spf_df[which(spf_df$quarter >= "1990-01-01"),]
  spf_df <- as.data.frame(spf_df[order(spf_df$quarter),])
  # Identify the median SPF forecast
  if (vers == 1){
    spf_df[,"SPF_nowcast"] <- as.numeric(spf_df[, paste0(SPF_series, "2")])
    spf_df[,"SPF_forecast1"] <- as.numeric(spf_df[, paste0(SPF_series, "3")])
  } else {
    spf_df[,"SPF_nowcast"] <- as.numeric(spf_df[, paste0("d", tolower(SPF_series), "2")])
    spf_df[,"SPF_forecast1"] <- as.numeric(spf_df[, paste0("d", tolower(SPF_series), "3")])
  }
  spf_df <- spf_df[, c("quarter", "SPF_nowcast", "SPF_forecast1")]
  
  
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
    gb_df <- gb_df[which(gb_df$quarter >= "1990-01-01"),]
    gb_df <- as.data.frame(gb_df[order(gb_df$quarter),])
    # Get the true data out from the lags
    gb_df[, "variable_act"] <- NA
    gb_df[1:(nrow(gb_df)-2), "variable_act"] <- 
      gb_df[3:nrow(gb_df), paste0(GB_sheet, "B2")]
    
    
    gb_df[,"GB_nowcast"] <- gb_df[, paste0(GB_sheet, "F0")]
    gb_df[,"GB_forecast1"] <- gb_df[, paste0(GB_sheet, "F1")]
    gb_df <- gb_df[, c("quarter", "variable_act", "GB_nowcast", "GB_forecast1")]
  
  } else {
    
    gb_df <- data.frame(quarter = spf_df[, c("quarter")])
    gb_df$variable_act <- NA
    gb_df$GB_nowcast <- NA
    gb_df$GB_forecast1 <- NA
    
    if (SPF_sheet == "TBILL"){
      tbill_df <- read.csv("data/SPF/DTB3.csv", stringsAsFactors = FALSE)
      tbill_df$DATE <- as.Date(tbill_df$DATE, format = "%d/%m/%Y")
      tbill_df$quarter <- floor_date(tbill_df$DATE, unit = "quarters")
      tbill_df <- tbill_df[which(tbill_df$quarter >= "1990-01-01"),]
      tbill_df <- tbill_df[which(tbill_df$DTB3 != "."),]
      tbill_df$DTB3 <- as.numeric(tbill_df$DTB3)
      tbill_df <- aggregate(tbill_df[,c("DTB3")], by = list(quarter = tbill_df$quarter), 
                            FUN = mean, na.action = na.rm)
      tbill_df$variable_act <- tbill_df$x
      gb_df <- merge(tbill_df[,c("quarter", "variable_act")], gb_df[c("quarter", "GB_nowcast")], 
                        by = "quarter", all.y = TRUE)
      gb_df$GB_nowcast <- gb_df$variable_act
      gb_df$GB_forecast1 <- gb_df$variable_act
      
    }
    
  }
    
  nowcast_df <- merge(gb_df, spf_df, by = "quarter", all.x = TRUE)
  nowcast_df$quarter <- as.Date(nowcast_df$quarter)
  nowcast_df$series <- SPF_sheet
  nowcast_df$topic <- spf2gb_df$topic[ii]
  
  # Plot to check they look sensible
  ggplot(nowcast_df) + theme_bw() + guides(color=guide_legend(title="Legend")) +
    geom_line(aes(x = quarter, y = variable_act, color = "Actual")) +
    geom_line(aes(x = quarter, y = GB_forecast1, color = "GB forecast"), linetype = "dashed") +
    geom_line(aes(x = quarter, y = SPF_forecast1, color = "SPF forecast"), linetype = "dashed") +
    ylab(SPF_sheet) + xlab("Date") + 
    ggtitle(paste("Nowcasts for", SPF_sheet))
  ggsave(paste0("figures/nowcasts/", SPF_sheet, ".pdf"), width = 6, height = 3)
    
  # Append to all_nowcasts_df
  all_nowcasts_df <- rbind(
    all_nowcasts_df, nowcast_df[,c("series", "topic", "quarter","variable_act", "GB_nowcast", "SPF_nowcast", 
                                   "GB_forecast1", "SPF_forecast1")])

}
rm(nowcast_df)
# Calculate errors
all_nowcasts_df$GB_now_error <- all_nowcasts_df$GB_nowcast - all_nowcasts_df$variable_act
all_nowcasts_df$SPF_now_error <- all_nowcasts_df$SPF_nowcast - all_nowcasts_df$variable_act
all_nowcasts_df$GB_SPF_now_gap <- all_nowcasts_df$GB_nowcast - all_nowcasts_df$SPF_nowcast
all_nowcasts_df$GB_SPF_f1_gap <- all_nowcasts_df$GB_forecast1 - all_nowcasts_df$SPF_forecast1

all_nowcasts_df <- pdata.frame(all_nowcasts_df,index = c("series", "quarter"))

all_nowcasts_df$GB_update <- all_nowcasts_df$GB_nowcast - plm::lag(all_nowcasts_df$GB_forecast1)
all_nowcasts_df$SPF_update <- all_nowcasts_df$SPF_nowcast - plm::lag(all_nowcasts_df$SPF_forecast1)
all_nowcasts_df <- data.frame(all_nowcasts_df)

# Calculate absolute errors
all_nowcasts_df$GB_now_error_abs <- abs(all_nowcasts_df$GB_now_error)
all_nowcasts_df$SPF_now_error_abs <- abs(all_nowcasts_df$SPF_now_error)
all_nowcasts_df$GB_SPF_now_gap_abs <- abs(all_nowcasts_df$GB_SPF_now_gap)
all_nowcasts_df$GB_SPF_f1_gap_abs <- abs(all_nowcasts_df$GB_SPF_f1_gap)
all_nowcasts_df$GB_update_abs <- abs(all_nowcasts_df$GB_update)
all_nowcasts_df$SPF_update_abs <- abs(all_nowcasts_df$SPF_update)


# Compute mean and sd
nowcast_means <- all_nowcasts_df %>%
  group_by(series) %>% 
  select(-quarter) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)
nowcast_means$GB_now_error_abs_sd[which(nowcast_means$series == "TBILL")] <- 1
nowcast_means$GB_now_error_sd[which(nowcast_means$series == "TBILL")] <- 1

## Standardise the nowcast variables
nowcasts_df <- merge(all_nowcasts_df, as.data.frame(nowcast_means), by = "series")
var_names <- c("GB_now_error_abs", "SPF_now_error_abs", "GB_SPF_now_gap_abs", 
               "GB_SPF_f1_gap_abs", "GB_SPF_f1_gap_abs", "SPF_update_abs", "GB_update_abs")
for (var_name in var_names){
  nowcasts_df[,paste0(var_name,"_std")] <- 
    (nowcasts_df[,var_name] - nowcasts_df[,paste0(var_name,"_mean")])/nowcasts_df[,paste0(var_name,"_sd")]

}
nowcasts_df <- nowcasts_df[,which(!str_detect(names(nowcasts_df), "_sd"))]
nowcasts_df <- nowcasts_df[,which(!str_detect(names(nowcasts_df), "_mean"))]

nowcasts_df$quarter <- as.Date(nowcasts_df$quarter)

ggplot(nowcasts_df) + theme_bw() + 
  facet_wrap(series~., nrow = 4, scales = "free") +
  geom_line(aes(x = quarter, y = variable_act, color = "Actual")) + 
  geom_line(aes(x = quarter, y = GB_nowcast, color = "GB nowcast"), linetype = "dashed") +
  geom_line(aes(x = quarter, y = SPF_nowcast, color = "SPF nowcast"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_nowcasts.pdf", width = 8, height = 6)

ggplot(nowcasts_df) + theme_bw() + 
  facet_wrap(series~., nrow = 4, scales = "free") +
  geom_line(aes(x = quarter, y = variable_act, color = "Actual")) + 
  geom_line(aes(x = quarter, y = GB_forecast1, color = "GB forecast"), linetype = "dashed") +
  geom_line(aes(x = quarter, y = SPF_forecast1, color = "SPF forecast"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_forecasts.pdf", width = 8, height = 6)



ggplot(nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_now_error_abs_std, color = "GB error"), linetype = "dashed") +
  geom_line(aes(y = SPF_now_error_abs_std, color = "SPF error"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_nowcast_errors.pdf", width = 8, height = 6)


ggplot(nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_SPF_f1_gap_abs_std, color = "Forecast gap"), linetype = "dashed") +
  geom_line(aes(y = GB_SPF_now_gap_abs_std, color = "Nowcast gap"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_gaps.pdf", width = 8, height = 6)

ggplot(nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_update_abs_std, color = "GB update"), linetype = "dashed") +
  geom_line(aes(y = SPF_update_abs_std, color = "SPF update"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_updates.pdf", width = 8, height = 6)







# Merge nowcasts with the topics and dispersion measures
df <- panel_df[,c("series", "quarter", "fed", "news",  "fed_std", "news_std", 
                  "dispersion", "dispersion_std")]
df$quarter <- as.Date(df$quarter)
nowcasts_df <- nowcasts_df[,c("series", "quarter", "variable_act", "GB_nowcast", "SPF_nowcast",
                              "GB_forecast1", "SPF_forecast1",
                              "GB_now_error_abs", "SPF_now_error_abs", "GB_now_error_abs_std", "SPF_now_error_abs_std",
                              "GB_SPF_now_gap_abs", "GB_SPF_f1_gap_abs", "GB_SPF_now_gap_abs_std", "GB_SPF_f1_gap_abs_std",
                              "SPF_update_abs", "GB_update_abs", "SPF_update_abs_std", "GB_update_abs_std"
                              )]
nowcasts_df$quarter <- as.Date(nowcasts_df$quarter)
df <- merge(df, nowcasts_df, by = c("series", "quarter"), all.x = T)

# COnvert to panel form
df$quarter <- as.Date(df$quarter)
df$period <- as.numeric(as.factor(df$quarter))
df <- pdata.frame(data.frame(df), index = c("series", "period"))

cor.test(df$dispersion_std, df$GB_SPF_now_gap_abs)
cor.test(df$dispersion_std, df$GB_SPF_f1_gap_abs)
cor.test(df$dispersion_std, df$GB_update_abs)
cor.test(df$dispersion_std, df$SPF_update_abs)

model1_std <- felm(dispersion_std ~ plm::lag(fed_std, 0) + plm::lag(news_std, 0) | series, data = df)
summary(model1_std)
model2_std <- felm(dispersion_std ~ plm::lag(fed_std, 0:1) + plm::lag(news_std, 0:1) + 
                     plm::lag(dispersion_std, 1:3) | series + period, data = df)
summary(model2_std)


model <- felm(fed_std ~ plm::lag(dispersion_std, 0) + plm::lag(news_std, 0) | series, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(dispersion_std, 0:1) + plm::lag(news_std, 0:1) + 
                + plm::lag(fed_std, 1:3)| series + period, data = df)
summary(model)


model <- felm(fed_std ~ plm::lag(GB_SPF_now_gap_abs_std, 0) + plm::lag(news_std, 0) | series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(GB_SPF_f1_gap_abs_std, 0) + plm::lag(news_std, 0) | series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(GB_now_error_abs_std, 0) + plm::lag(news_std, 0) | series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(SPF_now_error_abs_std, 0) + plm::lag(news_std, 0) | series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(SPF_update_abs_std, 0) + plm::lag(news_std, 0) | series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(GB_update_abs_std, 0) + plm::lag(news_std, 0) | series + period, data = df)
summary(model)


model <- felm(fed_std ~ plm::lag(GB_update_abs_std, 0:1) + plm::lag(news_std, 0:1) +
                plm::lag(fed_std, 1:3)| series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(SPF_update_abs_std, 0:1) + plm::lag(GB_update_abs_std, 0:1) + 
                plm::lag(dispersion_std, 0:1) + plm::lag(GB_SPF_f1_gap_abs_std, 0) +
                plm::lag(news_std, 0:1) + plm::lag(fed_std, 1:3)| series + period, data = df)
summary(model)
model <- felm(fed_std ~ plm::lag(GB_SPF_f1_gap_abs_std, 0:1) + 
                plm::lag(news_std, 0:1) + plm::lag(fed_std, 1:3)| series + period, data = df)
summary(model)


model <- felm(news_std ~ plm::lag(SPF_error_sq_std, 0) + plm::lag(dispersion_std, 0:1) | series + period, data = df)
summary(model)

model <- felm(fed_std ~ plm::lag(SPF_error_sq_std, 0) + plm::lag(dispersion_std, 0:1) | series + period, data = df)
summary(model)


model <- felm(fed_std ~ plm::lag(GB_error_sq_std, 0:1) +  plm::lag(SPF_error_sq_std, 0:1) + 
                plm::lag(news_std, 0:1) + plm::lag(fed_std, 1:3) | series + period, data = df)
summary(model)

write.csv(df, "data/jointmedia_spf_gb_panel.csv", row.names = FALSE)


ggplot(df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(series~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_error_abs_std, color = "GB SPF gap"), linetype = "dashed") +
  geom_line(aes(y = fed_std, color = "Fed"), linetype = "dashed") +
  geom_line(aes(y = dispersion_std, color = "SPF dispersion"), linetype = "dashed") +
  xlab("Date") + ylab("")




ggplot(df[df$series == "HOUSING",], aes(x=quarter)) + theme_bw() + 
  geom_line(aes(y = GB_error_abs, color = "GB SPF gap")) +
  geom_line(aes(y = fed_std, color = "Fed"), linetype = "dashed") +
  geom_line(aes(y = dispersion_std, color = "SPF dispersion"), linetype = "dashed") +
  xlab("Date") + ylab("")





ggsave("figures/nowcasts/all_gaps.pdf", width = 8, height = 6)




