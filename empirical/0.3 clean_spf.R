####
# This file combines SPF data with the NYT and Fed topics
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
require(reshape2)


############################# Import SPF data ############################# 

# Import the fedminutes to make sure we know how much to cover
minutes_df <- read.csv("data/clean_text/fedminutes_all.csv", stringsAsFactors = FALSE)

# Initialise SPF dataframe with quarters
spf_df <- data.frame(quarter = unique(minutes_df$quarter))
# disp = 1 is levels, and 2 is growth
SPF_variables <- list("NGDP" = 2, "RGDP" = 2, "CPI" = 1, "EMP" = 2, "UNEMP" = 1, 
                      "CPROF" = 2, "INDPROD" = 2, "HOUSING" = 2, "RRESINV" = 2, "RNRESIN" = 2, 
                      "RCONSUM" = 2, "RFEDGOV" = 2, "RSLGOV" = 2)
for (ii in 1:length(SPF_variables)){
  code <- names(SPF_variables)[ii]
  disp_measure <- SPF_variables[[ii]]
  
  import_filename <- paste0("data/SPF/Dispersion_", disp_measure, ".xlsx")
  dispersion.df <- data.frame(read_xlsx(import_filename, sheet = code, skip = 9))
  
  # Relabel the dispersion at various horizons
  command <- paste0("dispersion.df$",code,"_dispersion <- dispersion.df$",code,"_D",disp_measure,".T.")
  eval(parse(text=command))
  dispersion.df[which(dispersion.df[,paste0(code,"_dispersion")] == "#N/A"),
                paste0(code,"_dispersion")] <- NA
  dispersion.df[,paste0(code,"_dispersion")] <- as.numeric(dispersion.df[,paste0(code,"_dispersion")])
  
  for (ff in 1:4){
    command <- paste0("dispersion.df$",code,"_f",ff,"_dispersion <- dispersion.df$",
                      code,"_D",disp_measure,".T.",ff,".")
    eval(parse(text=command))
    dispersion.df[which(dispersion.df[,paste0(code,"_f",ff,"_dispersion")] == "#N/A"),
                  paste0(code,"_f",ff,"_dispersion")] <- NA
    dispersion.df[,paste0(code,"_f",ff,"_dispersion")] <- as.numeric(
      dispersion.df[,paste0(code,"_f",ff,"_dispersion")])
  }
  
  # Convert dates to quarterly 
  dispersion.df$Date <- dispersion.df$Survey_Date.T.
  year <- str_sub(dispersion.df$Date, 1,4)
  quarter <- str_sub(dispersion.df$Date, 5,6)
  quarter <- str_replace(quarter, "Q1", "01")
  quarter <- str_replace(quarter, "Q2", "04")
  quarter <- str_replace(quarter, "Q3", "07")
  quarter <- str_replace(quarter, "Q4", "10")
  date <- paste(year, quarter, "01", sep = "-")
  
  dispersion.df$Date <- as.Date(date)
  dispersion.df$quarter <- floor_date(dispersion.df$Date, "quarter")
  
  keep_cols <- c("quarter", names(dispersion.df)[which(str_detect(names(dispersion.df), "_dispersion"))])
  dispersion.df <- dispersion.df[,keep_cols]
  
  spf_df$quarter <- as.Date(spf_df$quarter)
  dispersion.df$quarter <- as.Date(dispersion.df$quarter)
  spf_df <- merge(spf_df, dispersion.df, by = "quarter", all.x = TRUE)
  spf_df$quarter <- as.Date(spf_df$quarter)
}

spf_df$quarter <- as.Date(spf_df$quarter)

# Plot as sense check
ggplot(spf_df) + theme_bw() + 
  geom_line(aes(x = quarter, y = CPI_dispersion, color = "CPI")) + 
  xlab("Quarter")

ggplot(spf_df) + theme_bw() + 
  geom_line(aes(x = quarter, y = RGDP_dispersion, color = "RGDP")) + 
  geom_line(aes(x = quarter, y = NGDP_dispersion, color = "NGDP")) + 
  xlab("Quarter")

ggplot(spf_df) + theme_bw() + 
  geom_point(aes(x = CPI_dispersion, y = RGDP_dispersion)) + 
  geom_smooth(aes(x = CPI_dispersion, y = NGDP_dispersion), method = "lm")



### Export just this bit to match with the text
write.csv(spf_df, "data/SPF/spf_disp_clean.csv", row.names = FALSE)


############################# Import Greenbook data ############################# 

spf_panel <- melt(spf_df, id = "quarter")
spf_panel$type <- str_replace(spf_panel$variable, "[A-Z]+_", "")
spf_panel$variable <- str_replace_all(spf_panel$variable, "_.*", "")
spf_panel <- pivot_wider(spf_panel, id_cols = c(quarter, variable), names_from = type, 
                         values_from = value)

# Define correspondence between SPF and Greenbook variables
spf2gb_df <- data.frame(series = names(SPF_variables))
spf2gb_df$GB_series <- NA
spf2gb_df$version <- 1
spf2gb_df[which(spf2gb_df$series == "NGDP"), c("GB_series", "version")] <- c("gNGDP",2)
spf2gb_df[which(spf2gb_df$series == "RGDP"), c("GB_series", "version")] <- c("gRGDP",2)
spf2gb_df[which(spf2gb_df$series == "CPI"), c("GB_series", "version")] <- c("gPCPI",1)
spf2gb_df[which(spf2gb_df$series == "UNEMP"), c("GB_series", "version")] <- c("UNEMP",1)
spf2gb_df[which(spf2gb_df$series == "EMP"), c("GB_series", "version")] <- c(NA,1)
spf2gb_df[which(spf2gb_df$series == "CPROF"), c("GB_series", "version")] <- c(NA,1)
spf2gb_df[which(spf2gb_df$series == "INDPROD"), c("GB_series", "version")] <- c("gIP",2)
spf2gb_df[which(spf2gb_df$series == "HOUSING"), c("GB_series", "version")] <- c("HSTART",1)
spf2gb_df[which(spf2gb_df$series == "RRESINV"), c("GB_series", "version")] <- c("gRRES",2)
spf2gb_df[which(spf2gb_df$series == "RNRESIN"), c("GB_series", "version")] <- c("gRBF",2)
spf2gb_df[which(spf2gb_df$series == "RCONSUM"), c("GB_series", "version")] <- c("gRPCE",2)
spf2gb_df[which(spf2gb_df$series == "RFEDGOV"), c("GB_series", "version")] <- c("gRGOVF",2)
spf2gb_df[which(spf2gb_df$series == "RSLGOV"), c("GB_series", "version")] <- c("gRGOVSL",2)



nowcast_names <- c("quarter", "variable", "variable_act", "GB_nowcast", "SPF_nowcast",
  "GB_forecast1", "SPF_forecast1")
all_nowcasts_df <- data.frame(matrix(NA, nrow = 0, ncol = length(nowcast_names)))

# Import greenbook data
for (ii in 1:nrow(spf2gb_df)){
  GB_sheet <- spf2gb_df$GB_series[ii]
  SPF_sheet <- spf2gb_df$series[ii]
  vers <- spf2gb_df$version[ii]
  
  SPF_series <- SPF_sheet 
  
  
  print(paste("Import SPF data from", SPF_sheet))
  if (vers == 1){
    import_filename <- paste0("data/SPF/meanLevel.xlsx")
  } else {
    import_filename <- paste0("data/SPF/meanGrowth.xlsx")
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
    import_filename <-"data/GB/GreenBook_Row_Format.xlsx"
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
    gb_df$GB_nowcast <- NA
    gb_df$GB_forecast1 <- NA
    if (SPF_series == "EMP"){
      real_data <- read.csv("data/SPF/EMP_data.csv", stringsAsFactors = FALSE)
      real_data <- real_data[which(str_sub(real_data$DATE, 6,7) %in% c("01","04","07","10")),]
      real_data$quarter <- as.Date(paste(str_sub(real_data$DATE, 1, 4), 
                                         str_sub(real_data$DATE, 6, 7), "01", sep = "-"))
      real_data$variable_act <- as.numeric(str_remove(real_data$EMPLOY22M6, ","))
      
      gb_df <- merge(gb_df, real_data[,c("quarter", "variable_act")], by = "quarter")
      gb_df <- gb_df[which(gb_df$quarter < "2017-01-01"),]
      
    } else if (SPF_series == "CPROF"){
      real_data <- read.csv("data/SPF/CPATAX.csv", stringsAsFactors = FALSE)
      real_data$quarter <- as.Date(real_data$DATE, format = "%d/%m/%Y")
      real_data$variable_act <- as.numeric(str_remove(real_data$CPATAX, ","))
      
      gb_df <- merge(gb_df, real_data[,c("quarter", "variable_act")], by = "quarter")
      gb_df <- gb_df[which(gb_df$quarter < "2017-01-01"),]
      
    } else {
      gb_df$variable_act <- NA
    }
    
  }
  
  nowcast_df <- merge(gb_df, spf_df, by = "quarter", all.x = TRUE)
  nowcast_df$quarter <- as.Date(nowcast_df$quarter)
  nowcast_df$variable <- SPF_sheet
  
  # Plot to check they look sensible
  ggplot(nowcast_df) + theme_bw() + guides(color=guide_legend(title="Legend")) +
    geom_line(aes(x = quarter, y = variable_act, color = "Actual")) +
    #geom_line(aes(x = quarter, y = GB_nowcast, color = "GB nowcast"), linetype = "dashed") +
    geom_line(aes(x = quarter, y = SPF_nowcast, color = "SPF nowcast"), linetype = "dashed") +
    ylab(SPF_sheet) + xlab("Date") + 
    ggtitle(paste("Nowcasts for", SPF_sheet))
  ggsave(paste0("figures/nowcasts/", SPF_sheet, ".pdf"), width = 6, height = 3)
  
  # Append to all_nowcasts_df
  all_nowcasts_df <- rbind(
    all_nowcasts_df, nowcast_df[,c("quarter", "variable", "variable_act", "GB_nowcast", "SPF_nowcast", 
                                   "GB_forecast1", "SPF_forecast1")])
  
}
rm(nowcast_df)

# Calculate errors
all_nowcasts_df$GB_now_error <- all_nowcasts_df$GB_nowcast - all_nowcasts_df$variable_act
all_nowcasts_df$SPF_now_error <- all_nowcasts_df$SPF_nowcast - all_nowcasts_df$variable_act
all_nowcasts_df$GB_SPF_now_gap <- all_nowcasts_df$GB_nowcast - all_nowcasts_df$SPF_nowcast
all_nowcasts_df$GB_SPF_f1_gap <- all_nowcasts_df$GB_forecast1 - all_nowcasts_df$SPF_forecast1

all_nowcasts_df <- pdata.frame(all_nowcasts_df,index = c("variable", "quarter"))

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

# Standardise
nowcast_means <- all_nowcasts_df %>%
  group_by(variable) %>% 
  select(-quarter) %>%
  summarise_all(list(mean, sd), na.rm = TRUE)
nowcast_means$GB_now_error_abs_fn2[which(nowcast_means$variable == "TBOND")] <- 1
nowcast_means$GB_now_error_fn2[which(nowcast_means$variable == "TBOND")] <- 1

nowcasts_df <- merge(all_nowcasts_df, as.data.frame(nowcast_means), by = "variable")
var_names <- c("GB_now_error_abs", "SPF_now_error_abs", "GB_SPF_now_gap_abs", 
               "GB_SPF_f1_gap_abs", "GB_SPF_f1_gap_abs", "SPF_update_abs", "GB_update_abs")
for (var_name in var_names){
  nowcasts_df[,paste0(var_name,"_std")] <- 
    (nowcasts_df[,var_name] - nowcasts_df[,paste0(var_name,"_fn1")])/nowcasts_df[,paste0(var_name,"_fn2")]
  
}
nowcasts_df <- nowcasts_df[,which(!str_detect(names(nowcasts_df), "_fn1"))]
nowcasts_df <- data.frame(nowcasts_df[,which(!str_detect(names(nowcasts_df), "_fn2"))])


#### Sense check plots
nowcasts_df$quarter <- as.Date(nowcasts_df$quarter)

ggplot(nowcasts_df) + theme_bw() + 
  facet_wrap(variable~., nrow = 3, scales = "free") +
  geom_line(aes(x = quarter, y = variable_act, color = "Actual")) + 
  geom_line(aes(x = quarter, y = GB_nowcast, color = "GB nowcast"), linetype = "dashed") +
  geom_line(aes(x = quarter, y = SPF_nowcast, color = "SPF nowcast"), linetype = "dashed") +
  xlab("Quarter") + ylab("")
ggsave("figures/nowcasts/all_nowcasts.pdf", width = 8, height = 5)

ggplot(nowcasts_df) + theme_bw() + 
  facet_wrap(variable~., nrow = 4, scales = "free") +
  geom_line(aes(x = quarter, y = variable_act, color = "Actual")) + 
  geom_line(aes(x = quarter, y = GB_forecast1, color = "GB forecast"), linetype = "dashed") +
  geom_line(aes(x = quarter, y = SPF_forecast1, color = "SPF forecast"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_forecasts.pdf", width = 8, height = 5)



ggplot(nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(variable~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_now_error_abs_std, color = "GB error"), linetype = "dashed") +
  geom_line(aes(y = SPF_now_error_abs_std, color = "SPF error"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_nowcast_errors.pdf", width = 8, height = 6)


ggplot(nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(variable~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_SPF_f1_gap_abs_std, color = "Forecast gap"), linetype = "dashed") +
  geom_line(aes(y = GB_SPF_now_gap_abs_std, color = "Nowcast gap"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_gaps.pdf", width = 8, height = 6)

ggplot(nowcasts_df, aes(x=quarter)) + theme_bw() + 
  facet_wrap(variable~., nrow = 3, scales = "free") +
  geom_line(aes(y = GB_update_abs_std, color = "GB update"), linetype = "dashed") +
  geom_line(aes(y = SPF_update_abs_std, color = "SPF update"), linetype = "dashed") +
  xlab("Date") + ylab("")
ggsave("figures/nowcasts/all_updates.pdf", width = 8, height = 6)



#### Clean up and export

df <- spf_panel[,c("quarter", "variable", "dispersion", "f1_dispersion")]
df$quarter <- as.Date(df$quarter)
nowcasts_df <- nowcasts_df[,c("variable", "quarter", "variable_act", "GB_nowcast", "SPF_nowcast",
                              "GB_forecast1", "SPF_forecast1",
                              "GB_now_error_abs", "SPF_now_error_abs", "GB_now_error_abs_std", "SPF_now_error_abs_std",
                              "GB_SPF_now_gap_abs", "GB_SPF_f1_gap_abs", "GB_SPF_now_gap_abs_std", "GB_SPF_f1_gap_abs_std",
                              "SPF_update_abs", "GB_update_abs", "SPF_update_abs_std", "GB_update_abs_std"
)]
nowcasts_df$quarter <- as.Date(nowcasts_df$quarter)
df <- merge(df, nowcasts_df, by = c("variable", "quarter"), all.x = T)

# Convert to panel form
df$quarter <- as.Date(df$quarter)
df$period <- as.numeric(as.factor(df$quarter))
df <- pdata.frame(data.frame(df), index = c("variable", "period"))

cor.test(df$dispersion, df$GB_SPF_now_gap_abs)
cor.test(df$dispersion, df$GB_SPF_f1_gap_abs)
cor.test(df$dispersion, df$GB_update_abs)
cor.test(df$dispersion, df$SPF_update_abs)


write.csv(df, "data/spf_gb_panel.csv", row.names = FALSE)

df <- read.csv("data/spf_gb_panel.csv", stringsAsFactors = FALSE)


pivot_wider()
df_wide <- pivot_wider(df, id_cols = c(quarter), names_from = variable, 
                       names_glue = "{variable}_{.value}",
                       values_from = c(SPF_nowcast, GB_nowcast))
plot_df1 <- data.frame(pivot_wider(plot_df1, id_cols = age, names_from = year, names_glue = "{year}_{.value}",
                                   values_from = c(workforce, population)))


"
End of script
"  
