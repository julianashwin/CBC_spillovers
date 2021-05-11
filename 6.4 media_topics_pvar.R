####
# This file estimates panel regressions on the Fed minutes NYT articles combined/joint topics.
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
require(panelvar)


### Define the directories where raw data is stored and clean will be saved
raw_dir <- "~/Documents/DPhil/Raw_Data/"
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


import_filename = paste0(clean_dir, "CBC/topics_forecasts_panel_long.csv")
total.df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

total.panel <- pdata.frame(total.df, index = c("series", "period"))


## Panel VAR
varone <-pvargmm(
  dependent_vars = c("fed_std","news_std","dispersion_std"),
  lags = 4,
  #exog_vars = c("INDB"),
  transformation = "fd",
  data = total.df,
  panel_identifier = c("series", "period"),
  steps = c("twostep"),
  system_instruments = TRUE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = FALSE
)

summary(varone)




############################# Panel analysis on the series ############################# 


# SPF dispersion as depepndent variable
model1 <- felm(dispersion ~ plm::lag(fed, 0) + plm::lag(news, 0) | series, data = total.panel)
summary(model1)
model1_std <- felm(dispersion_std ~ plm::lag(fed_std, 0) + plm::lag(news_std, 0) | series, data = total.panel)
summary(model1_std)
model2 <- felm(dispersion ~ plm::lag(fed, 0:1) + plm::lag(news, 0:1) + plm::lag(dispersion, 1:3) | series + period, data = total.panel)
summary(model2)
model2_std <- felm(dispersion_std ~ plm::lag(fed_std, -1:1) + plm::lag(news_std, -1:1) + plm::lag(dispersion_std, 1:3) | series + period, data = total.panel)
summary(model2_std)


# Fed topics as dependent variable
model3 <- felm(fed ~ plm::lag(dispersion, 0) | series, data = total.panel)
summary(model3)
model3_std <- felm(fed_std ~ plm::lag(dispersion_std, 0) | series, data = total.panel)
summary(model3_std)
model <- felm(fed ~ plm::lag(dispersion, -1:1) + plm::lag(news, -1:1) | series, data = total.panel)
summary(model)
model <- felm(fed ~ plm::lag(dispersion, 0:1) + plm::lag(news, 0:1) | series, data = total.panel)
summary(model)
model <- felm(fed ~ plm::lag(dispersion, 0:1) + plm::lag(news, 0:1) + plm::lag(fed, 1) | series, data = total.panel)
summary(model)
model4 <- felm(fed ~ plm::lag(dispersion, -1:1)  + plm::lag(fed, 1:3) | series + quarter, data = total.panel)
summary(model4)
model4_std <- felm(fed_std ~ plm::lag(dispersion_std, -1:1) + plm::lag(fed_std, 1:3) | series + quarter, data = total.panel)
summary(model4_std)

# NYT articles as dependent variable
model5 <- felm(news ~ plm::lag(dispersion, 0) | series, data = total.panel)
summary(model5)
model5_std <- felm(news_std ~ plm::lag(dispersion_std, 0) | series, data = total.panel)
summary(model5_std)
model <- felm(news ~ plm::lag(dispersion, -1:1) + plm::lag(fed, -1:1) | series, data = total.panel)
summary(model)
model <- felm(news ~ plm::lag(dispersion, 0:1) + plm::lag(fed, 0:1) | series, data = total.panel)
summary(model)
model <- felm(news ~ plm::lag(dispersion, 0:1) + plm::lag(fed, 0:1) + plm::lag(news, 1) | series, data = total.panel)
summary(model)
model6 <- felm(news ~ plm::lag(dispersion, -1:1) + plm::lag(news, 1:3) | series + quarter, data = total.panel)
summary(model6)
model6_std <- felm(news_std ~ plm::lag(dispersion_std, -1:1) + plm::lag(news_std, 1:3) | series + quarter, data = total.panel)
summary(model6_std)


# Table
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and SPF forecast dispersion (unstandardised data)",
          label = "tab:topic_spf_results_unstand")

stargazer(model1_std, model2_std, model3_std, model4_std, model5_std, model6_std,
          table.placement = "H", df = FALSE,
          title = "Federal Reserve minutes, NYT articles and SPF forecast dispersion",
          label = "tab:topic_spf_results")







############################# End ############################# 