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


############################# Import SPF data ############################# 

# Import the fedminutes to make sure we know how much to cover
minutes_df <- read.csv("data/clean_text/fedminutes_all.csv", stringsAsFactors = FALSE)

# Initialise SPF dataframe with quarters
spf_df <- data.frame(quarter = unique(minutes_df$quarter))
# disp = 1 is levels, and 2 is growth
SPF_variables <- list("NGDP" = 2, "RGDP" = 2, "CPI" = 1, "CORECPI" = 1, "PCE" = 1,  "COREPCE" = 1,"TBILL" = 1, 
                      "EMP" = 2, "UNEMP" = 1, "CPROF" = 2, "INDPROD" = 2, "HOUSING" = 2, 
                      "RRESINV" = 2, "RNRESIN" = 2, "RCONSUM" = 2, "RFEDGOV" = 2, "RSLGOV" = 2)
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
  geom_line(aes(x = quarter, y = CORECPI_dispersion, color = "CORECPI")) + 
  geom_line(aes(x = quarter, y = PCE_dispersion, color = "PCE")) + 
  geom_line(aes(x = quarter, y = COREPCE_dispersion, color = "COREPCE")) + 
  xlab("Quarter")

ggplot(spf_df) + theme_bw() + 
  geom_line(aes(x = quarter, y = RGDP_dispersion, color = "RGDP")) + 
  geom_line(aes(x = quarter, y = NGDP_dispersion, color = "NGDP")) + 
  xlab("Quarter")

ggplot(spf_df) + theme_bw() + 
  geom_point(aes(x = CPI_dispersion, y = RGDP_dispersion)) + 
  geom_smooth(aes(x = CPI_dispersion, y = NGDP_dispersion), method = "lm")



write.csv(spf_df, "data/SPF/spf_disp_clean.csv", row.names = FALSE)

"
End of script
"  
