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
require(topicdoc)
require(urca)


standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}
odiag <- function(x) x[col(x) != row(x)]


### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/topic_data/"
##### ntopics
k <- 40
k <- 30
k <- 29



get_siglevel <- function(cor_obj){
  sig_level <- "{}"
  if (cor_obj$p.value <= 0.1){
    sig_level <- "{.}"
  } 
  if (cor_obj$p.value <= 0.05){
    sig_level <- "{..}"
  }
  if (cor_obj$p.value <= 0.01){
    sig_level <- "{...}"
  }
  return(sig_level)
}

#macro_var <- "disp_std"
#text_var <- "mins_std"
create_corr_df <- function(total_panel, text_var = "mins_std", macro_var = "disp_std", 
                           siglevel = T, pval = F){
  
  corr_df <- data.frame(Variable = as.character(unique(total_panel$variable)))
  corr_df[,as.character(unique(total_panel$variable))] <- NA 
  for (ii in 1:nrow(corr_df)){
    var_x <- as.character(corr_df$Variable[ii])
    obs_x <- which(total_panel$variable == var_x)
    if (any(!is.na(total_panel[obs_x, macro_var]))){
      # Dispersion
      for (jj in 2:ncol(corr_df)){
        var_y <- names(corr_df)[jj]
        obs_y <- which(total_panel$variable == var_y)
        
        if (any(!is.na(total_panel[obs_y, text_var]))){
          test_temp <- cor.test(total_panel[obs_x, macro_var], total_panel[obs_y, text_var])
          if (siglevel){
            corr_df[ii,jj] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
          } else {
            if (pval){
              corr_df[ii,jj] <- round(test_temp$p.value,3)
            } else {
              corr_df[ii,jj] <- round(test_temp$estimate,3)
            }
          }
        }
      }
    }
  }
  return(corr_df)
}


felm_DK_se <- function(reg_formula, df_panel){
  
  # Estimate regressions with feols and felm
  model <- feols(reg_formula, data = df_panel)
  model_felm <- felm(reg_formula, data = df_panel)
  
  stopifnot(length(model_felm$se) ==  
              length(summary(model, vcov = DK ~ period)$coeftable[,"Std. Error"]))
  model_felm$se <- summary(model, vcov = DK ~ period)$coeftable[,"Std. Error"]
  model_felm$tval <- summary(model, vcov = DK ~ period)$coeftable[,"t value"]
  model_felm$pval <- summary(model, vcov = DK ~ period)$coeftable[,"Pr(>|t|)"]
  return(model_felm)
}
#corr_df  <- create_corr_df(total_panel, "mins_std", "GB_update_abs_std")
#corr_pval_df  <- create_corr_df(total_panel, "mins_std", "GB_update_abs_std", siglevel = F,
#                           pval = T)

comp_K_df <- data.frame(K = 15:40, not_matched = NA, double_matched = NA, disp = NA, GB_update = NA,
                        SPF_update = NA, GB_error = NA, SPF_error = NA, GB_SPF_gap = NA,
                        topics_picked = NA,
                        disp_coef = NA, disp_se = NA, gap_coef = NA, gap_se = NA, 
                        GB_up_coef = NA, GB_up_se = NA, GB_err_coef = NA, GB_err_se = NA,
                        SPF_up_coef = NA, SPF_up_se = NA, SPF_err_coef = NA, SPF_err_se = NA)

comp_K_df_speech <- data.frame(K = 15:40, not_matched = NA, double_matched = NA, disp = NA, GB_update = NA,
                        SPF_update = NA, GB_error = NA, SPF_error = NA, GB_SPF_gap = NA,
                        topics_picked = NA,
                        disp_coef = NA, disp_se = NA, gap_coef = NA, gap_se = NA, 
                        GB_up_coef = NA, GB_up_se = NA, GB_err_coef = NA, GB_err_se = NA,
                        SPF_up_coef = NA, SPF_up_se = NA, SPF_err_coef = NA, SPF_err_se = NA)

comp_K_df_news <- data.frame(K = 15:40, not_matched = NA, double_matched = NA, disp = NA, GB_update = NA,
                               SPF_update = NA, GB_error = NA, SPF_error = NA, GB_SPF_gap = NA,
                               topics_picked = NA,
                               disp_coef = NA, disp_se = NA, gap_coef = NA, gap_se = NA, 
                               GB_up_coef = NA, GB_up_se = NA, GB_err_coef = NA, GB_err_se = NA,
                               SPF_up_coef = NA, SPF_up_se = NA, SPF_err_coef = NA, SPF_err_se = NA)



for (k in 15:40){
##### suffix for files 
spec <- paste0("_qly_k",k)
#spec <- "_guid_k30_qly"

##### Names of topic variables
variablenames <- paste0("T", 1:k)

# Import the topic summary for interpretation 
import_filename =  paste0("data/topic_data/short_topics_summary_k",k,".csv")
#import_filename =  "data/topic_data/joint_topics_summary_guid_k30.csv"
topic_summary <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

if (str_detect(spec,"guid")){
  variablenames <- topic_summary$Topic
}


paragraph_lda <- readRDS(file = paste0("data/topic_data/overall/short_lda_k",k,".rds"))
top_terms <- terms(paragraph_lda, 15)
topic_summary$top_terms <- NA
for (ii in 1:nrow(topic_summary)){
  topic_summary$top_terms[ii] <- paste(top_terms[,topic_summary$Topic[ii]], collapse = ",")
}


############################# Import the topic proportions ############################# 

# Minutes
import_filename = paste0(clean_dir,"minutes", spec,".csv")
minutes_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
minutes_df <- minutes_df[,c("quarter", variablenames)]

import_filename = paste0(clean_dir,"minutes_event_k",k,".csv")
minutes_mly <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
minutes_mly$month <- floor_date(as.Date(minutes_mly$meet_date), unit = "months")
minutes_mly <- minutes_mly[,c("month", variablenames)]


# Speeches
import_filename = paste0(clean_dir,"speeches", spec,".csv")
speeches_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
speeches_df[,paste0(variablenames,"_speech")] <- speeches_df[,variablenames]
speeches_df <- speeches_df[,c("quarter", paste0(variablenames,"_speech"))]

import_filename = paste0(clean_dir,"speeches_event_k",k,".csv")
speeches_mly <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
speeches_mly$month <- floor_date(as.Date(speeches_mly$date), unit = "months")
speeches_mly <- speeches_mly[,c("month", variablenames)]
speeches_mly <- aggregate(speeches_mly[,variablenames], by = list(month = speeches_mly$month), FUN = mean)

# Articles
import_filename =  paste0(clean_dir,"articles", spec,".csv")
articles_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
articles_df[,paste0(variablenames,"_news")] <- articles_df[,variablenames]
articles_df <- articles_df[,c("quarter", paste0(variablenames,"_news"))]

articles_key <- read.csv("data/clean_text/articles_key.csv", stringsAsFactors = F)

import_filename = paste0(clean_dir,"overall/article_topics_k",k,".csv")
articles_mly <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
articles_mly <- merge(articles_key, articles_mly, by = "unique_id")
articles_mly$month <- floor_date(as.Date(articles_mly$date), unit = "months")
articles_mly <- articles_mly[,c("month", variablenames)]
articles_mly <- aggregate(articles_mly[,variablenames], by = list(month = articles_mly$month), FUN = mean)


# Combine into one quarterly df
topics_df <- merge(minutes_df, speeches_df, by = "quarter", all.x = TRUE)
topics_df <- merge(topics_df, articles_df, by = "quarter", all.x = TRUE)

# Import SPF data
spf_df <- read.csv("data/SPF/spf_disp_clean.csv", stringsAsFactors = F)
SPF_variables <- unique(unlist(str_extract_all(names(spf_df), "[A-Z]+")))
SPF_variables <- SPF_variables[which(SPF_variables != "TBOND")] 


# Merge SPF with topics
spf_df$quarter <- as.Date(spf_df$quarter)
topics_df$quarter <- as.Date(topics_df$quarter)
total_df <- merge(spf_df, topics_df, by = "quarter")
total_df$quarter <- as.Date(total_df$quarter)
total_df <- total_df[which(total_df$quarter < "2018-01-01"),]

if (FALSE){
  clean_filename = "data/topics_qly_baseline.csv"
  write.csv(total_df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
}


"
Match the topics to variables
"
topic_summary$SPF_vars <- ""

top_terms <- c("invest", "capit")
SPF_var <- "RNRESIN"
match_vars <- function(topic_summary, SPF_var, top_terms){
  obs <-   which(str_detect(topic_summary$top_terms, top_terms[1]) & 
                   str_detect(topic_summary$top_terms, top_terms[2]))
  
  if (length(obs) < 1){
    print(paste("No match for", SPF_var))
  }
  if (length(obs) > 1){
    print(paste("Multiple matches for", SPF_var))
    pos_sum <- rep(NA, length(obs))
    for (ii in 1:length(obs)){
      terms_obs <- unlist(str_split(topic_summary$top_terms[obs[ii]], ","))
      top_terms <- unlist(str_split(top_terms, "\\|"))
      pos_sum[ii] <- sum(which(terms_obs %in% top_terms)[1:2])
    }
    obs <- obs[which(pos_sum == min(pos_sum))]
    
  }
  topic_summary$SPF_vars[obs] <- paste0(topic_summary$SPF_vars[obs], SPF_var, ",")
  return(topic_summary)
}
topic_summary <- match_vars(topic_summary, "NGDP", c("recess", "growth"))
topic_summary <- match_vars(topic_summary, "RGDP", c("recess", "growth"))
topic_summary <- match_vars(topic_summary, "CPI", c("price", "inflat"))
topic_summary <- match_vars(topic_summary, "EMP", c("job", "emp"))
topic_summary <- match_vars(topic_summary, "UNEMP", c("job", "emp"))
topic_summary <- match_vars(topic_summary, "CPROF", c("corpor", "profit"))
topic_summary <- match_vars(topic_summary, "INDPROD", c("industri", "manufac"))
topic_summary <- match_vars(topic_summary, "HOUSING", c("hous", "home"))
topic_summary <- match_vars(topic_summary, "RRESINV", c("hous", "home"))
topic_summary <- match_vars(topic_summary, "RNRESIN", c("invest", "capit"))
topic_summary <- match_vars(topic_summary, "RCONSUM", c("spend", "consum"))
topic_summary <- match_vars(topic_summary, "RFEDGOV", c("tax", "budget"))
topic_summary <- match_vars(topic_summary, "RSLGOV", c("tax", "budget"))

if (FALSE){
  print_tab <- topic_summary[,c("Topic", "Description", "Top.5.Words",
                                "mins", "speech", "nyt", "SPF_vars")]
  print_tab$Topic <- str_remove(print_tab$Topic, "Topic ")
  print_tab[,c("mins", "speech", "nyt")] <- round(print_tab[,c("mins", "speech", "nyt")], 3)
  stargazer(as.matrix(print_tab), table.placement = "H", column.sep.width = "0pt", 
            title = paste0("Topics for K = ",k))
  
}

# Stationarity tests 
topic_summary[,c("tau_mins", "phi1_mins", "phi2_mins","tau_speech", "phi1_speech", "phi2_speech",
                 "tau_news", "phi1_news", "phi2_news")] <- NA

ur_sig <- function(temp_test , hypoth = "tau3"){
  temp_test@cval[hypoth,]
  sig_level <- ""
  if (abs(temp_test@teststat[,hypoth]) <= abs(temp_test@cval[hypoth,"1pct"])){
    sig_level <- "{.}"
  } 
  if (abs(temp_test@teststat[,hypoth]) <= abs(temp_test@cval[hypoth,"5pct"])){
    sig_level <- "{..}"
  }
  if (abs(temp_test@teststat[,hypoth]) <= abs(temp_test@cval[hypoth,"10pct"])){
    sig_level <- "{...}"
  }
  stat_out <-  paste0(round(temp_test@teststat[,hypoth],2), sig_level)
  return(stat_out)
}

for (kk in 1:k){
  
  temp_series <- minutes_mly[,paste0("T",kk)]
  temp_test <- ur.df(temp_series, type = "trend", lags = 1,
                     selectlags = "AIC")
  temp_test@testreg
  topic_summary[kk, "tau_mins"] <- ur_sig(temp_test , hypoth = "tau3")
  topic_summary[kk, "phi1_mins"] <- ur_sig(temp_test , hypoth = "phi2")
  topic_summary[kk, "phi2_mins"] <- ur_sig(temp_test , hypoth = "phi3")
  
  temp_series <- speeches_mly[,paste0("T",kk)]
  temp_series <- temp_series[which(!is.na(temp_series))]
  temp_test <- ur.df(temp_series, type = "trend", lags = 1,
                     selectlags = "AIC")
  temp_test@testreg
  topic_summary[kk, "tau_speech"] <- ur_sig(temp_test , hypoth = "tau3")
  topic_summary[kk, "phi1_speech"] <- ur_sig(temp_test , hypoth = "phi2")
  topic_summary[kk, "phi2_speech"] <- ur_sig(temp_test , hypoth = "phi3")
  
  temp_series <- articles_mly[,paste0("T",kk)]
  temp_series <- temp_series[which(!is.na(temp_series))]
  temp_test <- ur.df(temp_series, type = "trend", lags = 1,
                     selectlags = "AIC")
  temp_test@testreg
  topic_summary[kk, "tau_news"] <- ur_sig(temp_test , hypoth = "tau3")
  topic_summary[kk, "phi1_news"] <- ur_sig(temp_test , hypoth = "phi2")
  topic_summary[kk, "phi2_news"] <- ur_sig(temp_test , hypoth = "phi3")
  
}
if (FALSE){
  print_tab <- topic_summary[,c("Topic","tau_mins", "phi1_mins", "phi2_mins",
                                "tau_speech", "phi1_speech", "phi2_speech",
                                "tau_news", "phi1_news", "phi2_news")]
  #print_tab <- print_tab[which(topic_summary$SPF_vars != ""),]
  stargazer(as.matrix(print_tab), table.placement = "H", 
            title = "Augmented Dickey-Fuller unit root test results")
}



panelnames <- c("variable", "quarter", "disp", "disp_f1", "disp_f2", "disp_f3", "disp_f4",
                "mins", "speeches", "news", "double_matched",
                "disp_std", "disp_f1_std", "disp_f2_std", "disp_f3_std", "disp_f4_std",
                "mins_std", "speeches_std", "news_std")
total_panel <- data.frame(matrix(NA, nrow = 0, ncol = length(panelnames)))

total_df$double_matched <- NA

for (spf_var in SPF_variables){
  # Pull out the relevant SPF variables
  total_df$disp <- total_df[, paste0(spf_var, "_dispersion")]
  total_df$disp_f1 <- total_df[, paste0(spf_var, "_f1_dispersion")]
  total_df$disp_f2 <- total_df[, paste0(spf_var, "_f2_dispersion")]
  total_df$disp_f3 <- total_df[, paste0(spf_var, "_f3_dispersion")]
  total_df$disp_f4 <- total_df[, paste0(spf_var, "_f4_dispersion")]
  
  # Pull out the relevant topic variables
  tnum <- which(str_detect(topic_summary$SPF_vars, paste0(spf_var,",")))
  if (length(tnum) == 1 ){
    total_df$mins <- total_df[, paste0("T", tnum)]
    total_df$speeches <- total_df[, paste0("T", tnum, "_speech")]
    total_df$news <- total_df[, paste0("T", tnum, "_news")]
    total_df$double_matched <- length(tnum)
  } else if (length(tnum) == 0){
    print(paste("No topic found for", spf_var))
    total_df$mins <- NA
    total_df$speeches <-NA
    total_df$news <- NA
    total_df$double_matched <- NA
  } else {
    print(paste("Multiple topics found for", spf_var))
    total_df$mins <- rowSums(total_df[, paste0("T", tnum)])
    total_df$speeches <- rowSums(total_df[, paste0("T", tnum, "_speech")])
    total_df$news <- rowSums(total_df[, paste0("T", tnum, "_news")])
    total_df$double_matched <- length(tnum)
  }
  
  temp_df <- total_df[,c("quarter", "disp", "disp_f1", "disp_f2", "disp_f3", "disp_f4",
                         "mins", "speeches", "news", "double_matched")]
  temp_df$disp_std <- standardise(temp_df$disp)
  temp_df$disp_f1_std <- standardise(temp_df$disp_f1)
  temp_df$disp_f2_std <- standardise(temp_df$disp_f2)
  temp_df$disp_f3_std <- standardise(temp_df$disp_f3)
  temp_df$disp_f4_std <- standardise(temp_df$disp_f4)
  
  temp_df$mins_std <- standardise(temp_df$mins)
  temp_df$speeches_std <- standardise(temp_df$speeches)
  temp_df$news_std <- standardise(temp_df$news)
  
  temp_df$variable <- spf_var
  
  temp_df[,panelnames]
  
  total_panel <- rbind(total_panel, temp_df)
}

### Merge in the other macro data

spf_gb_panel <- read.csv("data/spf_gb_panel.csv", stringsAsFactors = FALSE)

spf_gb_panel$quarter <- as.Date(spf_gb_panel$quarter)
total_panel$quarter <- as.Date(total_panel$quarter)

total_panel <- merge(total_panel, spf_gb_panel, by = c("variable", "quarter"), all.x = T)

#obs <- which(total_panel$variable == "RRESINV")
#cor.test(total_panel$disp[obs], total_panel$mins[obs])


total_panel$quarter <- as.Date(total_panel$quarter)
total_panel$period <- as.numeric(as.factor(total_panel$quarter))
total_panel <- pdata.frame(data.frame(total_panel), index = c("variable", "period"))


ggplot(total_panel) + theme_bw() + 
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) + 
  scale_color_manual("Variable", values = c("FOMC minutes" = "darkblue",
                                            "FOMC speeches" = "darkgreen",
                                            "News articles" = "grey",
                                            "SPF dispersion" = "firebrick")) + 
  facet_wrap(variable~., nrow = 3, scales = "free") +
  geom_line(aes(x = quarter, y = news_std, color = "FOMC speeches"), alpha = 0.6) +
  geom_line(aes(x = quarter, y = speeches_std, color = "News articles"), alpha = 0.6) +
  geom_line(aes(x = quarter, y = mins_std, color = "FOMC minutes"), alpha = 0.8) + 
  geom_line(aes(x = quarter, y = disp_std, color = "SPF dispersion"), alpha = 0.8) + 
  xlab("Date") + ylab("Std. Units") + scale_x_date(date_labels = "%y")
#ggsave("figures/fed_media_topics/all_topics.pdf", width = 8, height = 5)

if (FALSE){
  clean_filename = "data/topics_forecasts_panel.csv"
  write.csv(total_panel, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
}


# Lags 
total_panel$mins_std_1lag <- plm::lag(total_panel$mins_std,1)
total_panel$mins_std_2lag <- plm::lag(total_panel$mins_std,2)
total_panel$mins_std_3lag <- plm::lag(total_panel$mins_std,3)
total_panel$mins_std_4lag <- plm::lag(total_panel$mins_std,4)
total_panel$mins_std_5lag <- plm::lag(total_panel$mins_std,5)
total_panel$mins_std_6lag <- plm::lag(total_panel$mins_std,6)
total_panel$mins_std_7lag <- plm::lag(total_panel$mins_std,7)

total_panel$speeches_std_1lag <- plm::lag(total_panel$speeches_std,1)
total_panel$speeches_std_2lag <- plm::lag(total_panel$speeches_std,2)
total_panel$speeches_std_3lag <- plm::lag(total_panel$speeches_std,3)
total_panel$speeches_std_4lag <- plm::lag(total_panel$speeches_std,4)
total_panel$speeches_std_5lag <- plm::lag(total_panel$speeches_std,5)
total_panel$speeches_std_6lag <- plm::lag(total_panel$speeches_std,6)
total_panel$speeches_std_7lag <- plm::lag(total_panel$speeches_std,7)

total_panel$news_std_1lag <- plm::lag(total_panel$news_std,1)
total_panel$news_std_2lag <- plm::lag(total_panel$news_std,2)
total_panel$news_std_3lag <- plm::lag(total_panel$news_std,3)
total_panel$news_std_4lag <- plm::lag(total_panel$news_std,4)
total_panel$news_std_5lag <- plm::lag(total_panel$news_std,5)
total_panel$news_std_6lag <- plm::lag(total_panel$news_std,6)
total_panel$news_std_7lag <- plm::lag(total_panel$news_std,7)





### Fill in table 
obs <- which(comp_K_df$K == k)
comp_K_df$not_matched[obs] <- paste(unique(total_panel$variable[which(is.na(total_panel$mins))]), collapse = ",")
comp_K_df$double_matched[obs] <- paste(unique(total_panel$variable[which(total_panel$double_matched > 1)]), collapse = ",")

# Dispersion 
test_temp <- cor.test(total_panel$disp_std, total_panel$mins_std)
comp_K_df$disp[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
test_temp <- cor.test(total_panel$disp_std, total_panel$speeches_std)
comp_K_df_speech$disp[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
test_temp <- cor.test(total_panel$disp_std, total_panel$news_std)
comp_K_df_news$disp[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
# GB update
test_temp <- cor.test(total_panel$GB_update_abs_std, total_panel$mins_std)
comp_K_df$GB_update[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
test_temp <- cor.test(total_panel$GB_update_abs_std, total_panel$speeches_std)
comp_K_df_speech$GB_update[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
test_temp <- cor.test(total_panel$GB_update_abs_std, total_panel$news_std)
comp_K_df_news$GB_update[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
# GB error
test_temp <- cor.test(total_panel$GB_now_error_abs_std, total_panel$mins_std)
comp_K_df$GB_error[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
test_temp <- cor.test(total_panel$GB_now_error_abs_std, total_panel$speeches_std)
comp_K_df_speech$GB_error[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
test_temp <- cor.test(total_panel$GB_now_error_abs_std, total_panel$news_std)
comp_K_df_news$GB_error[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))


# Other 
test_temp <- cor.test(total_panel$SPF_update_abs_std, total_panel$mins_std)
comp_K_df$SPF_update[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$SPF_now_error_abs_std, total_panel$mins_std)
comp_K_df$SPF_error[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$GB_SPF_now_gap_abs_std, total_panel$mins_std)
comp_K_df$GB_SPF_gap[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

comp_K_df$topics_picked[obs] <- paste(str_replace(topic_summary$Topic[topic_summary$SPF_vars != ""], 
                                             "Topic ", ""), collapse = ",")

reg_panel <- total_panel[which(total_panel$quarter < "2017-01-01"),]
# Dispersion
reg_formula <- formula(mins_std ~ disp_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
summary(feols(reg_formula, reg_panel), vcov = DK~period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df$disp_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df$disp_se[obs] <- model$coefficients[1,"Std. Error"]
reg_formula <- formula(speeches_std ~ disp_std+speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable+period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df_speech$disp_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df_speech$disp_se[obs] <- model$coefficients[1,"Std. Error"]
reg_formula <- formula(news_std ~ disp_std+news_std_1lag+news_std_2lag+news_std_3lag|variable+period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df_news$disp_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df_news$disp_se[obs] <- model$coefficients[1,"Std. Error"]
# GB update
reg_formula <- formula(mins_std ~ GB_update_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df$GB_up_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df$GB_up_se[obs] <- model$coefficients[1,"Std. Error"]
reg_formula <- formula(speeches_std ~ GB_update_abs_std+speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable+period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df_speech$GB_up_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df_speech$GB_up_se[obs] <- model$coefficients[1,"Std. Error"]
reg_formula <- formula(news_std ~ GB_update_abs_std+news_std_1lag+news_std_2lag+news_std_3lag|variable+period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df_news$GB_up_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df_news$GB_up_se[obs] <- model$coefficients[1,"Std. Error"]
# GB error
reg_formula <- formula(mins_std ~ GB_now_error_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df$GB_err_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df$GB_err_se[obs] <- model$coefficients[1,"Std. Error"]
reg_formula <- formula(speeches_std ~ GB_now_error_abs_std+speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable+period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df_speech$GB_err_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df_speech$GB_err_se[obs] <- model$coefficients[1,"Std. Error"]
reg_formula <- formula(news_std ~ GB_now_error_abs_std+news_std_1lag+news_std_2lag+news_std_3lag|variable+period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df_news$GB_err_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df_news$GB_err_se[obs] <- model$coefficients[1,"Std. Error"]

# Gap
reg_formula <- formula(mins_std ~ GB_SPF_now_gap_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df$gap_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df$gap_se[obs] <- model$coefficients[1,"Std. Error"]
# SPF update
reg_formula <- formula(mins_std ~ SPF_update_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df$SPF_up_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df$SPF_up_se[obs] <- model$coefficients[1,"Std. Error"]
# SPF error
reg_formula <- formula(mins_std ~ SPF_now_error_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model <- summary(felm_DK_se(reg_formula, reg_panel))
comp_K_df$SPF_err_coef[obs] <- model$coefficients[1,"Estimate"]
comp_K_df$SPF_err_se[obs] <- model$coefficients[1,"Std. Error"]
#summary(felm(disp_std ~ mins_std, total_panel))



corr_df <- data.frame(Variable = unique(total_panel$variable), 
                      disp_mins = NA, GB_up_mins = NA, GB_err_mins = NA,
                      disp_speech = NA, GB_up_speech = NA, GB_err_speech = NA,
                      disp_news = NA, GB_up_news = NA, GB_err_news = NA)
for (ii in 1:nrow(corr_df)){
  varname <- as.character(corr_df$Variable[ii])
  obs <- which(total_panel$variable == varname)
  if (any(!is.na(total_panel$mins_std[obs]))){
    # Dispersion
    test_temp <- cor.test(total_panel$disp_std[obs], total_panel$mins_std[obs])
    corr_df$disp_mins[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
    test_temp <- cor.test(total_panel$disp_std[obs], total_panel$speeches_std[obs])
    corr_df$disp_speech[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
    test_temp <- cor.test(total_panel$disp_std[obs], total_panel$news_std[obs])
    corr_df$disp_news[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
    # GB update
    if (!(varname %in% c("EMP", "CPROF"))){
      test_temp <- cor.test(total_panel$GB_update_abs_std[obs], total_panel$mins_std[obs])
      corr_df$GB_up_mins[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
      test_temp <- cor.test(total_panel$GB_update_abs_std[obs], total_panel$speeches_std[obs])
      corr_df$GB_up_speech[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
      test_temp <- cor.test(total_panel$GB_update_abs_std[obs], total_panel$news_std[obs])
      corr_df$GB_up_news[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
      # GB error
      test_temp <- cor.test(total_panel$GB_now_error_abs_std[obs], total_panel$mins_std[obs])
      corr_df$GB_err_mins[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
      test_temp <- cor.test(total_panel$GB_now_error_abs_std[obs], total_panel$speeches_std[obs])
      corr_df$GB_err_speech[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
      test_temp <- cor.test(total_panel$GB_now_error_abs_std[obs], total_panel$news_std[obs])
      corr_df$GB_err_news[ii] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))
    }
  }
}
if (FALSE){
  stargazer(as.matrix(corr_df), table.placement = "H", column.sep.width = "0pt", 
            title = paste0("Correlation of uncertainty with focus"))
  
}

corr_disp  <-  create_corr_df(total_panel, text_var = "mins_std", macro_var = "disp_std",)
corr_up <- create_corr_df(total_panel, text_var = "mins_std", macro_var = "GB_update_abs_std")
corr_err <- create_corr_df(total_panel, text_var = "mins_std", macro_var = "GB_now_error_abs_std")
corr_mat <- rbind(corr_disp, corr_up, corr_err)
if (FALSE){
  stargazer(as.matrix(corr_mat), table.placement = "H", column.sep.width = "0pt", 
            title = paste0("Correlation of SPF dispersion with focus of FOMC minutes"))
  
}

# Dispersion
corr_est <- create_corr_df(total_panel, "mins_std", "disp_std", siglevel = F)
corr_est <- as.matrix(corr_est[,2:ncol(corr_est)])
corr_pvals <- create_corr_df(total_panel, "mins_std", "disp_std", siglevel = F, pval = T)
corr_pvals <- as.matrix(corr_pvals[,2:ncol(corr_pvals)])
mean(diag(corr_est), na.rm = T)
mean(odiag(corr_est), na.rm = T)
mean(diag(corr_pvals), na.rm = T)
mean(odiag(corr_pvals), na.rm = T)

# GB_update
corr_est <- create_corr_df(total_panel, "mins_std", "GB_update_abs_std", siglevel = F)
corr_est <- as.matrix(corr_est[,2:ncol(corr_est)])
corr_pvals <- create_corr_df(total_panel, "mins_std", "GB_update_abs_std", siglevel = F, pval = T)
corr_pvals <- as.matrix(corr_pvals[,2:ncol(corr_pvals)])
mean(diag(corr_est), na.rm = T)
mean(odiag(corr_est), na.rm = T)
mean(diag(corr_pvals), na.rm = T)
mean(odiag(corr_pvals), na.rm = T)

# GB_error
corr_est <- create_corr_df(total_panel, "mins_std", "GB_now_error_abs_std", siglevel = F)
corr_est <- as.matrix(corr_est[,2:ncol(corr_est)])
corr_pvals <- create_corr_df(total_panel, "mins_std", "GB_now_error_abs_std", siglevel = F, pval = T)
corr_pvals <- as.matrix(corr_pvals[,2:ncol(corr_pvals)])
mean(diag(corr_est), na.rm = T)
mean(odiag(corr_est), na.rm = T)
mean(diag(corr_pvals), na.rm = T)
mean(odiag(corr_pvals), na.rm = T)

corr_mins_disp_pvals <- create_corr_df(total_panel, "mins_std", "disp_std", siglevel = F, 
                                     pval = T)

corr_mins_disp_mat <- create_corr_df(total_panel, "mins_std", "disp_std", siglevel = F)
corr_mins_disp_mat <- as.matrix(corr_mins_disp_mat[,2:ncol(corr_mins_disp_mat)])
mean(diag(corr_mins_disp_mat), na.rm = T)
mean(odiag(corr_mins_disp_mat), na.rm = T)

corr_mins_disp_mat <- create_corr_df(total_panel, "mins_std", "GB_now_error_abs_std", siglevel = F)
corr_mins_disp_mat <- as.matrix(corr_mins_disp_mat[,2:ncol(corr_mins_disp_mat)])
mean(diag(corr_mins_disp_mat), na.rm = T)
mean(odiag(corr_mins_disp_mat), na.rm = T)



}
beep()











### Some topic model stats
total_dtm <- readRDS("data/topic_data/overall/total_dtm.rds")
short_dtm <- readRDS("data/topic_data/overall/short_dtm.rds")

temp_df <- data.frame(K = 15:40, loglik = NA, size = NA, dist_corp = NA, 
                      dist_df = NA, prominence = NA, coherence = NA, 
                      exclusivity = NA, perplexity = NA)
for (ii in 1:nrow(temp_df)){
  paragraph_lda <- readRDS(file = paste0("data/topic_data/overall/short_lda_k",temp_df$K[ii],".rds"))
  
  rel_tops <- str_split(comp_K_df$topics_picked[which(comp_K_df$K == temp_df$K[ii])], ",")[[1]]
  
  if (paragraph_lda@n == sum(short_dtm)){
    #diags <- topic_diagnostics(paragraph_lda,short_dtm)
    diags <- data.frame(topic_num = 1:temp_df$K[ii])
    diags$coherence <- topic_coherence(paragraph_lda,short_dtm, 
                                       top_n_tokens = 15)
    diags$exclusivity <- topic_exclusivity(paragraph_lda, 
                                           top_n_tokens = 15, excl_weight = 1)
    #temp_df$size[ii] <- mean(diags$topic_size)
    #temp_df$dist_corp[ii] <- mean(diags$dist_from_corpus)
    #temp_df$dist_df[ii] <- mean(diags$tf_df_dist)
    #temp_df$prominence[ii] <- mean(diags$doc_prominence)
    obs <- which(diags$topic_num %in% rel_tops)
    temp_df$coherence[ii] <- mean(diags$coherence)
    temp_df$exclusivity[ii] <- mean(diags$exclusivity)
    #temp_df$perplexity[ii] <- perplexity(paragraph_lda, newdata = total_dtm,
    #                                     estimate_theta=TRUE)
  }
  #terms(paragraph_lda,10)
  #temp_df$loglik[ii] <- paragraph_lda@loglikelihood
}


comp_K_df1 <- merge(comp_K_df, temp_df, by = "K")
comp_K_df1$`Fully matched` <- as.numeric(comp_K_df$not_matched == "")
comp_K_df1$Both <- standardise(comp_K_df1$coherence) + standardise(log(comp_K_df1$exclusivity))

ggplot(comp_K_df1, aes(x= K)) + 
  scale_color_manual("Metric", values = c("Coherence" = "blue",
                                          "Exclusivity" = "green",
                                          "Both" = "red")) + 
  geom_line(aes(y = standardise(coherence), color ="Coherence")) +
  geom_line(aes(y = standardise(exclusivity), color ="Exclusivity")) +
  #geom_line(aes(y = standardise(perplexity), color ="Perplexity"))
  geom_line(aes(y = Both, color ="Both")) + 
  geom_point(data = comp_K_df1[which(comp_K_df1$`Fully matched` == 1),],
             aes(x = K, y = Both), shape = 8) + ylab("Std. units")
ggsave("figures/fed_media_topics/topic_comp.pdf", width = 6, height = 3)




print_tab <- comp_K_df[,c("K", "not_matched", "disp", "GB_update", "GB_error")]
names(comp_K_df_speech)[3:ncol(comp_K_df_speech)] <- 
  paste0(names(comp_K_df_speech)[3:ncol(comp_K_df_speech)], "_speech")
print_tab <- merge(print_tab, by = "K", 
                   comp_K_df_speech[,c("K", "disp_speech", "GB_update_speech", "GB_error_speech")])
names(comp_K_df_speech) <- names(comp_K_df)
names(comp_K_df_news)[3:ncol(comp_K_df_news)] <- 
  paste0(names(comp_K_df_news)[3:ncol(comp_K_df_news)], "_news")
print_tab <- merge(print_tab, by = "K", 
                   comp_K_df_news[,c("K", "disp_news", "GB_update_news", "GB_error_news")])
names(comp_K_df_news) <- names(comp_K_df)



stargazer(as.matrix(print_tab), table.placement = "H", 
          title = "Correlation of focus with uncertainty across K")


comp_K_df$corpus <- "FOMC minutes"
comp_K_df_speech$corpus <- "FOMC speeches"
comp_K_df_news$corpus <- "NYT articles"  
comp_K_df_all <- rbind(comp_K_df, comp_K_df_speech, comp_K_df_news)

comp_K_df_all <- comp_K_df_all[,c("K", "corpus", "disp_coef", "disp_se", 
                                  "GB_up_coef", "GB_up_se","GB_err_coef", "GB_err_se")]
comp_K_df_all<- merge(comp_K_df_all, comp_K_df[,c("K","not_matched")], by = "K")
comp_K_df_all$not_matched[which(comp_K_df_all$not_matched!="")] <- "some"#"some"
comp_K_df_all$not_matched[which(comp_K_df_all$not_matched=="")] <- "some"

## Plot coefficients across K
ggplot(comp_K_df_all, aes(y = K)) + theme_bw() + 
  scale_shape_manual(guide="none",values = c("some" = 19, "none" = 8)) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_ribbon(aes(xmin = disp_coef-1.282*abs(disp_se), xmax = disp_coef+1.282*abs(disp_se)), 
              alpha = 0.5) + 
  geom_ribbon(aes(xmin = disp_coef-1.96*abs(disp_se), xmax = disp_coef+1.96*abs(disp_se)), 
              alpha = 0.2) + 
  geom_point(aes(x = disp_coef, shape = not_matched)) + 
  scale_x_continuous(breaks = c(-0.1,0,0.1)) + 
  facet_wrap(corpus~., nrow = 1) + 
  xlab("Coefficient on SPF dispersion")
ggsave("figures/fed_media_topics/disp_coefs.pdf", width = 4, height = 6)

ggplot(comp_K_df_all, aes(y = K)) + theme_bw() + 
  scale_shape_manual(guide="none",values = c("some" = 19, "none" = 8)) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_ribbon(aes(xmin = GB_up_coef-1.282*abs(GB_up_se), xmax = GB_up_coef+1.282*abs(GB_up_se)), 
              alpha = 0.5) + 
  geom_ribbon(aes(xmin = GB_up_coef-1.96*abs(GB_up_se), xmax = GB_up_coef+1.96*abs(GB_up_se)), 
              alpha = 0.2) + 
  geom_point(aes(x = GB_up_coef, shape = not_matched)) + 
  scale_x_continuous(breaks = c(-0.1,0,0.1)) + 
  facet_wrap(corpus~., nrow = 1) + 
  xlab("Coefficient on Tealbook update")
ggsave("figures/fed_media_topics/GB_up_coefs.pdf", width = 4, height = 6)

ggplot(comp_K_df_all, aes(y = K)) + theme_bw() + 
  scale_shape_manual(guide="none",values = c("some" = 19, "none" = 8)) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_ribbon(aes(xmin = GB_err_coef-1.282*abs(GB_err_se), xmax = GB_err_coef+1.282*abs(GB_err_se)), 
              alpha = 0.5) + 
  geom_ribbon(aes(xmin = GB_err_coef-1.96*abs(GB_err_se), xmax = GB_err_coef+1.96*abs(GB_err_se)), 
              alpha = 0.2) + 
  geom_point(aes(x = GB_err_coef, shape = not_matched)) + 
  scale_x_continuous(breaks = c(-0.1,0,0.1)) + 
  facet_wrap(corpus~., nrow = 1) + 
  xlab("Coefficient on Tealbook error")
ggsave("figures/fed_media_topics/GB_err_coefs.pdf", width = 4, height = 6)











summary(felm(mins_std ~ plm::lag(mins_std,1) | variable, total_panel))

summary(felm(mins_std ~ plm::lag(mins_std,1) + disp_std + GB_update_abs_std + SPF_update_abs_std +
               GB_now_error_abs_std + SPF_now_error_abs_std + GB_SPF_now_gap_abs_std | variable, total_panel))

summary(felm(mins_std ~ disp_std, total_panel))
summary(felm(mins_std ~ plm::lag(disp_std,0) + plm::lag(mins_std,1) | variable + period, total_panel))
summary(felm(mins_std ~ plm::lag(disp_std,0) + plm::lag(mins_std,1:8) | variable + period, total_panel))

summary(felm(mins_std ~ disp_std + plm::lag(mins_std,1:8) | variable + period, total_panel))
summary(felm(mins_std ~ GB_update_abs_std + plm::lag(mins_std,1:8) | variable + period, total_panel))
summary(felm(mins_std ~ SPF_update_abs_std + plm::lag(mins_std,1:8) | variable + period, total_panel))
summary(felm(mins_std ~ GB_now_error_abs_std + plm::lag(mins_std,1:8) | variable + period, total_panel))
summary(felm(mins_std ~ SPF_now_error_abs_std + plm::lag(mins_std,1:8) | variable + period, total_panel))
summary(felm(mins_std ~ GB_SPF_now_gap_abs_std + plm::lag(mins_std,1:8) | variable + period, total_panel))


summary(felm(speeches_std ~ disp_std + plm::lag(speeches_std,1:8) | variable + period, total_panel))
summary(felm(speeches_std ~ GB_update_abs_std + plm::lag(speeches_std,1:8) | variable + period, total_panel))
summary(felm(speeches_std ~ SPF_update_abs_std + plm::lag(speeches_std,1:8) | variable + period, total_panel))
summary(felm(speeches_std ~ GB_now_error_abs_std + plm::lag(speeches_std,1:8) | variable + period, total_panel))
summary(felm(speeches_std ~ SPF_now_error_abs_std + plm::lag(speeches_std,1:8) | variable + period, total_panel))
summary(felm(speeches_std ~ GB_SPF_now_gap_abs_std + plm::lag(speeches_std,1:8) | variable + period, total_panel))


summary(felm(news_std ~ disp_std + plm::lag(news_std,1:8) | variable + period, total_panel))
summary(felm(news_std ~ GB_update_abs_std + plm::lag(news_std,1:8) | variable + period, total_panel))
summary(felm(news_std ~ SPF_update_abs_std + plm::lag(news_std,1:8) | variable + period, total_panel))
summary(felm(news_std ~ GB_now_error_abs_std + plm::lag(news_std,1:8) | variable + period, total_panel))
summary(felm(news_std ~ SPF_now_error_abs_std + plm::lag(news_std,1:8) | variable + period, total_panel))
summary(felm(news_std ~ GB_SPF_now_gap_abs_std + plm::lag(news_std,1:8) | variable + period, total_panel))


summary(felm(news_std ~ mins_std + speeches_std + plm::lag(news_std,1:8) | variable + period, total_panel))


summary(felm(mins_std ~ GB_now_error_abs_std + GB_update_abs_std + disp_std + 
               plm::lag(mins_std,1:8) | variable + period, total_panel))

summary(felm(mins_std ~ GB_SPF_now_gap_abs_std + SPF_now_error_abs_std + 
               GB_now_error_abs_std + SPF_update_abs_std + 
               GB_update_abs_std + disp_std + 
               plm::lag(mins_std,1:8) | variable + period, total_panel))


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