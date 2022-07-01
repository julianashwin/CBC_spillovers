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

standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}


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


comp_K_df <- data.frame(K = 15:40, not_matched = NA, double_matched = NA, disp = NA, GB_update = NA,
                        SPF_update = NA, GB_error = NA, SPF_error = NA, GB_SPF_gap = NA)


for (k in 15:40){
##### suffix for files 
spec <- "_qly"
spec <- "_full_qly_k30"
spec <- "_full_qly_k40"
spec <- "_qly_k30"
spec <- paste0("_qly_k",k)
#spec <- "_guid_k30_qly"

##### Names of topic variables
variablenames <- paste0("T", 1:k)

# Import the topic summary for interpretation 
import_filename =  "data/topic_data/full_topics_summary_k30.csv"
import_filename =  "data/topic_data/full_topics_summary_k40.csv"
import_filename =  "data/topic_data/short_topics_summary_k30.csv"
import_filename =  paste0("data/topic_data/short_topics_summary_k",k,".csv")
#import_filename =  "data/topic_data/joint_topics_summary_guid_k30.csv"
topic_summary <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

if (str_detect(spec,"guid")){
  variablenames <- topic_summary$Topic
}


############################# Import the topic proportions ############################# 

# Minutes
import_filename = paste0(clean_dir,"minutes", spec,".csv")
minutes_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
minutes_df <- minutes_df[,c("quarter", variablenames)]
#topics_df <- minutes_df
# Speeches
import_filename = paste0(clean_dir,"speeches", spec,".csv")
speeches_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
speeches_df[,paste0(variablenames,"_speech")] <- speeches_df[,variablenames]
speeches_df <- speeches_df[,c("quarter", paste0(variablenames,"_speech"))]
# Articles
import_filename =  paste0(clean_dir,"articles", spec,".csv")
articles_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
articles_df[,paste0(variablenames,"_news")] <- articles_df[,variablenames]
articles_df <- articles_df[,c("quarter", paste0(variablenames,"_news"))]
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
total_df <- total_df[which(total_df$quarter < "2020-01-01"),]



"
Match the topics to variables
"
topic_summary$SPF_vars <- ""

top_terms <- c("economi", "growth")
SPF_var <- "NGDP"
match_vars <- function(topic_summary, SPF_var, top_terms){
  obs <-   which(str_detect(topic_summary$Top.5.Words, top_terms[1]) & 
                   str_detect(topic_summary$Top.5.Words, top_terms[2]))
  topic_summary$SPF_vars[obs] <- paste0(topic_summary$SPF_vars[obs], SPF_var, ",")
  
  if (length(obs) < 1){
    print(paste("No match for", SPF_var))
  }
  if (length(obs) > 1){
    print(paste("Multiple matches for", SPF_var))
  }
  return(topic_summary)
}
topic_summary <- match_vars(topic_summary, "NGDP", c("economi", "growth"))
topic_summary <- match_vars(topic_summary, "RGDP", c("economi", "growth"))
topic_summary <- match_vars(topic_summary, "CPI", c("price", "inflat"))
topic_summary <- match_vars(topic_summary, "EMP", c("job", "emp"))
topic_summary <- match_vars(topic_summary, "UNEMP", c("job", "emp"))
topic_summary <- match_vars(topic_summary, "CPROF", c("corpor|compani", "profit"))
topic_summary <- match_vars(topic_summary, "INDPROD", c("industri", "produc|manufac"))
topic_summary <- match_vars(topic_summary, "HOUSING", c("hous", "home"))
topic_summary <- match_vars(topic_summary, "RRESINV", c("hous", "home"))
topic_summary <- match_vars(topic_summary, "RNRESIN", c("invest", "capit"))
topic_summary <- match_vars(topic_summary, "RCONSUM", c("spend", "consum"))
topic_summary <- match_vars(topic_summary, "RFEDGOV", c("tax", "budget"))
topic_summary <- match_vars(topic_summary, "RSLGOV", c("tax", "budget"))



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



ggplot(total_panel) + theme_bw() + 
  facet_wrap(variable~., nrow = 4, scales = "free") +
  geom_line(aes(x = quarter, y = news_std, color = "Speeches"), alpha = 0.3) +
  geom_line(aes(x = quarter, y = speeches_std, color = "News"), alpha = 0.3) +
  geom_line(aes(x = quarter, y = mins_std, color = "Minutes")) + 
  geom_line(aes(x = quarter, y = disp_std, color = "Dispersion")) + 
  xlab("Date") + ylab("Std. Units")


### Merge in the other macro data

spf_gb_panel <- read.csv("data/spf_gb_panel.csv", stringsAsFactors = FALSE)

spf_gb_panel$quarter <- as.Date(spf_gb_panel$quarter)
total_panel$quarter <- as.Date(total_panel$quarter)

total_panel <- merge(total_panel, spf_gb_panel, by = c("variable", "quarter"), all.x = T)

obs <- which(total_panel$variable == "RRESINV")
cor.test(total_panel$disp[obs], total_panel$mins[obs])


total_panel$quarter <- as.Date(total_panel$quarter)
total_panel$period <- as.numeric(as.factor(total_panel$quarter))
total_panel <- pdata.frame(data.frame(total_panel), index = c("variable", "period"))

### Fill in table 
obs <- which(comp_K_df$K == k)
comp_K_df$not_matched[obs] <- paste(unique(total_panel$variable[which(is.na(total_panel$mins))]), collapse = ",")
comp_K_df$double_matched[obs] <- paste(unique(total_panel$variable[which(total_panel$double_matched > 1)]), collapse = ",")

test_temp <- cor.test(total_panel$disp_std, total_panel$mins_std)
comp_K_df$disp[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$GB_update_abs_std, total_panel$mins_std)
comp_K_df$GB_update[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$SPF_update_abs_std, total_panel$mins_std)
comp_K_df$SPF_update[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$GB_now_error_abs_std, total_panel$mins_std)
comp_K_df$GB_error[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$SPF_now_error_abs_std, total_panel$mins_std)
comp_K_df$SPF_error[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

test_temp <- cor.test(total_panel$GB_SPF_now_gap_abs_std, total_panel$mins_std)
comp_K_df$GB_SPF_gap[obs] <- paste0(round(test_temp$estimate,3), get_siglevel(test_temp))

#summary(felm(disp_std ~ mins_std, total_panel))

}



### Some topic model stats
total_dtm <- readRDS("data/topic_data/overall/total_dtm.rds")
short_dtm <- readRDS("data/topic_data/overall/short_dtm.rds")

temp_df <- data.frame(K = 15:40, loglik = NA, size = NA, dist_corp = NA, dist_df = NA,
                      prominence = NA, coherence = NA, exclusivity = NA)
for (ii in 1:nrow(temp_df)){
  paragraph_lda <- readRDS(file = paste0("data/topic_data/overall/short_lda_k",temp_df$K[ii],".rds"))
  
  if (paragraph_lda@n == sum(short_dtm)){
    diags <- topic_diagnostics(paragraph_lda,short_dtm)
    temp_df$size[ii] <- mean(diags$topic_size)
    temp_df$dist_corp[ii] <- mean(diags$dist_from_corpus)
    temp_df$dist_df[ii] <- mean(diags$tf_df_dist)
    temp_df$prominence[ii] <- mean(diags$doc_prominence)
    temp_df$coherence[ii] <- mean(diags$topic_coherence)
    temp_df$exclusivity[ii] <- mean(diags$topic_exclusivity)
  }
  terms(paragraph_lda,10)
  
  temp_df$loglik[ii] <- paragraph_lda@loglikelihood
}


comp_K_df1 <- merge(comp_K_df, temp_df, by = "K")

ggplot(comp_K_df1, aes(x= K)) + 
  geom_line(aes(y = standardise(coherence), color ="Coherence")) +
  geom_line(aes(y = standardise(exclusivity), color ="Exclusivity")) +
  geom_line(aes(y = (standardise(coherence) + standardise(exclusivity)), color ="Both"))


summary(felm(mins_std ~ plm::lag(mins_std,1) | variable, total_panel))

summary(felm(mins_std ~ plm::lag(mins_std,1) + disp_std + GB_update_abs_std + SPF_update_abs_std +
               GB_now_error_abs_std + SPF_now_error_abs_std + GB_SPF_now_gap_abs_std | variable, total_panel))

summary(felm(mins_std ~ disp_std, total_panel))
summary(felm(mins_std ~ disp_std + news_std + speeches_std | variable, total_panel))
summary(felm(mins_std ~ disp_std + news_std + speeches_std | variable + period, total_panel))
summary(felm(mins_std ~ plm::lag(disp_std,0) + plm::lag(mins_std,1) | variable + period, total_panel[which(total_panel$quarter < "2020-01-01"),]))

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


summary(felm(mins_std ~ GB_SPF_now_gap_abs_std + SPF_now_error_abs_std + 
               GB_now_error_abs_std + SPF_update_abs_std + 
               GB_update_abs_std + disp_std + 
               plm::lag(mins_std,1:8) | variable + period, total_panel))



names(total_panel)

summary(felm(mins_std ~ news_std + speeches_std | variable + period, total_panel))


SPF_update_abs

total_panel$SPF_update_abs


summary(lm(mins_std ~ disp_std + news_std + speeches_std, total_panel))

summary(lm(speeches_std ~ disp_std + mins_std , total_panel))



"
Create correlation matrix
"






create_corr_df <- function(total_df, variablenames, SPF_variables, suffix = "", k = 30){
  corr_df <- data.frame(matrix(NA, k,(length(SPF_variables) + 1)))
  names(corr_df) <- c("topic", SPF_variables)
  
  corr_df$topic <- paste0("T", 1:k, ".",str_replace_all(topic_summary$Top.5.Words, ", ","."))
  for(ii in 1:nrow(corr_df)){
    topic_series <- total_df[,paste0(variablenames[ii], suffix)]
    for (spf_var in SPF_variables){
      spf_series <- total_df[,paste0(spf_var, "_dispersion")]
      cor_obj <- cor.test(topic_series, spf_series)
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
      corr_df[ii, spf_var]<- paste0(round(cor_obj$estimate,3), sig_level)
    }
  }
  return(corr_df)
  
}

corr_mins1 <- create_corr_df(total_df[which(total_df$quarter < "2017-01-01"),], 
                             variablenames, SPF_variables, suffix = "", k = k)
corr_mins <- create_corr_df(total_df, variablenames, SPF_variables, suffix = "", k = k)
corr_speech <- create_corr_df(total_df, variablenames, SPF_variables, suffix = "_speech", k = k)
corr_news <- create_corr_df(total_df, variablenames, SPF_variables, suffix = "_news", k = k)

cor.test(total_df$T3, rowMeans((total_df[,c("HOUSING_dispersion", "HOUSING_f1_dispersion",
                                             "HOUSING_f2_dispersion", "HOUSING_f3_dispersion",
                                             "HOUSING_f4_dispersion")])))
cor.test(total_df$T3, rowMeans((total_df[,c("CPI_dispersion", "CPI_f1_dispersion",
                                             "CPI_f2_dispersion", "CPI_f3_dispersion",
                                             "CPI_f4_dispersion")])))

cor.test(total_df$T21, total_df$RRESINV_dispersion)
cor.test(total_df$T21, total_df$HOUSING_dispersion)
cor.test(total_df$T3, total_df$CPI_dispersion)



ggplot(total_df[which(total_df$quarter < "2017-01-01"),]) + theme_bw() +
  geom_line(aes(x = quarter, y = standardise(T30_speech), color = "speech"), alpha = 0.4) +
  geom_line(aes(x = quarter, y = standardise(T30_news), color = "news"), alpha = 0.4) + 
  geom_line(aes(x = quarter, y = standardise(T30), color = "mins")) + 
  geom_line(aes(x = quarter, y = standardise(EMP_dispersion), color = "spf"))


ggplot(total_df) + theme_bw() +
  geom_line(aes(x = quarter, y = standardise(T21_speech), color = "speech"), alpha = 0.4) +
  geom_line(aes(x = quarter, y = standardise(T21_news), color = "news"), alpha = 0.4) + 
  geom_line(aes(x = quarter, y = standardise(T21), color = "mins")) + 
  geom_line(aes(x = quarter, y = standardise(HOUSING_dispersion), color = "spf"))
cor.test(total_df$T30, total_df$EMP_dispersion)
summary(lm(T11 ~ EMP_dispersion + lag(T11-1), total_df))
summary(lm(T13 ~ CPI_dispersion + lag(T13-1), total_df))

ggplot(total_df) + theme_bw() +
  geom_line(aes(x = quarter, y = standardise(T3_speech), color = "speech")) +
  geom_line(aes(x = quarter, y = standardise(T3_news), color = "news")) + 
  geom_line(aes(x = quarter, y = standardise(T3), color = "mins")) + 
  geom_line(aes(x = quarter, y = standardise(RGDP_f1_dispersion), color = "spf"))
cor.test(total_df$T3, total_df$RGDP_dispersion)



ggplot(spf_df) + theme_bw() +
  geom_line(aes(x = quarter, y = log(NGDP_dispersion), color = "0")) + 
  geom_line(aes(x = quarter, y = log(NGDP_f1_dispersion), color = "1")) + 
  geom_line(aes(x = quarter, y = log(NGDP_f2_dispersion), color = "2")) + 
  geom_line(aes(x = quarter, y = log(NGDP_f3_dispersion), color = "3")) +
  geom_line(aes(x = quarter, y = log(NGDP_f4_dispersion), color = "4"))
  
ggplot() + theme_bw() +
  geom_line(data = total_df, aes(x = quarter, y = standardise(T13 + T27), color = "mins")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(NGDP_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(NGDP_f1_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(NGDP_f2_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(NGDP_f3_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(NGDP_f4_dispersion), color = "SPF"))
cor.test(total_df$T27+total_df$T13, total_df$NGDP_dispersion)  

ggplot() + theme_bw() +
  geom_line(data = total_df, aes(x = quarter, y = standardise(T9), color = "mins")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f1_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f2_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f3_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f4_dispersion), color = "SPF"))

ggplot() + theme_bw() +
  geom_line(data = total_df, aes(x = quarter, y = standardise(T8), color = "mins")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f1_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f2_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f3_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(CPI_f4_dispersion), color = "SPF"))
cor.test(total_df$T8, total_df$CPI_dispersion)  

ggplot() + theme_bw() +
  geom_line(data = total_df, aes(x = quarter, y = standardise(T5_news), color = "mins")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f1_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f2_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f3_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f4_dispersion), color = "SPF"))
cor.test(total_df$T5_news, total_df$HOUSING_dispersion)  



  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f1_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f2_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f3_dispersion), color = "SPF")) + 
  geom_line(data = spf_df, aes(x = quarter, y = standardise(HOUSING_f4_dispersion), color = "SPF"))
cor.test(total_df$T5_news, total_df$HOUSING_dispersion)  

# Define a function to import and plot the SPF dispersion data alongside the relevant topic





############################# Create panel df with topics and SPF disp ############################# 

series <- c("CPI", "NGDP", "RGDP", "EMP", "UNEMP", "CPROF", "INDPROD", "HOUSING", "TBILL",
            "RCONSUM", "RNRESIN", "RRESINV", "RFEDGOV", "RSLGOV")

### Keep only the topics which are matched to an SPF series
command <- paste0("total.panel <- select(total.df, quarter, ", 
                  paste(paste0("starts_with(\"", series, "\")"), collapse = ", "), ")")
eval(parse(text=command))

### Gather into panel 
total.panel <- gather(total.panel, topic, value, -quarter)

total.panel$series <- NA
for (s in series){
  total.panel[which(str_detect(total.panel$topic, s)),"series"] <- s
}

total.panel$type <- NA
for (t in c("dispersion", "f1_dispersion", "fed", "news")){
  total.panel[which(str_detect(total.panel$topic, t)),"type"] <- t
}

### Spread back to get dispersion, fed and news measures for each series
total.panel <- spread(total.panel, type, value)
total.panel <- select(total.panel, -topic)

total.panel <- total.panel %>%
  group_by(quarter, series) %>% 
  summarise_all(funs(mean), na.rm = TRUE)

### Normalise by series
total_means <- total.panel %>%
  group_by(series) %>% 
  select(-quarter) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)

total.panel <- merge(total.panel, total_means, by = "series")
total.panel$dispersion_std <- (total.panel$dispersion - total.panel$dispersion_mean)/total.panel$dispersion_sd
total.panel$f1_dispersion_std <- (total.panel$f1_dispersion - total.panel$f1_dispersion_mean)/total.panel$f1_dispersion_sd
total.panel$fed_std <- (total.panel$fed - total.panel$fed_mean)/total.panel$fed_sd
total.panel$news_std <- (total.panel$news - total.panel$news_mean)/total.panel$news_sd



clean_filename = "data/topics_forecasts_panel.csv"
write.csv(total.panel, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
total.panel <- read.csv("data/topics_forecasts_panel.csv", stringsAsFactors = FALSE)

### Convert to 
total.panel$quarter <- as.Date(total.panel$quarter)
total.panel$period <- as.numeric(as.factor(total.panel$quarter))
total.panel <- pdata.frame(data.frame(total.panel), index = c("series", "period"))


############################# Cross-correlation matrix for the series ############################# 
test1 <- cor.test(total.df$NGDP_dispersion, total.df$CPI_topic_fed)
test1$estimate
test1 <- cor.test(total.df$CPI_topic_fed, total.df$NGDP_dispersion)
test1$estimate
cor.test(total.df$CPI_topic_news, total.df$NGDP_dispersion)
cor.test(total.df$NGDP_topic_fed, total.df$CPI_dispersion)
cor.test(total.df$NGDP_topic_news, total.df$CPI_dispersion)
cor.test(total.df$NGDP_topic_news, total.df$CPI_topic_news)
cor.test(total.df$CPROF_topic_fed, total.df$CPROF_dispersion)
cor.test(total.df$RCONSUM_topic_fed, total.df$RCONSUM_dispersion)
cor.test(total.df$HOUSING_topic_news, total.df$HOUSING_dispersion)
cor.test(total.df$INDPROD_topic_news, total.df$INDPROD_dispersion)




series <- as.character(unique(total.panel$series))


### Correlation matrix for Fed topics
fed.cor.matrix <- data.frame(matrix(NA, nrow = 3*length(series), ncol = (1+length(series))))
fed.cor.matrix.est <- matrix(NA, nrow = length(series), ncol = (length(series)))
fed.cor.matrix.pval <- matrix(NA, nrow = length(series), ncol = (length(series)))
split_rows <- paste(as.vector(paste(series, "x n", "x n x")))
split_rows <- paste(split_rows, collapse = "")
split_rows <- (str_split(split_rows, "x"))[[1]]
colnames(fed.cor.matrix) <- c("Series", series)
fed.cor.matrix$Series <- str_replace_all(split_rows[1:42], "n", "")

i <- 1
k <- 1
s <- series[1]
t <- series[1]
for (s in series){
  
  # Dispersion series
  disp_series <- total.panel[which(total.panel$series == s), "dispersion"]
  j <- 2
  for (t in series){
    
    
    fed_series <- total.panel[which(total.panel$series == t), "fed"]
    
    correlation <- cor.test(disp_series, fed_series)
    
    est <- round(correlation$estimate, 3)
    p_val <- round(correlation$p.value, 3)
    
    fed.cor.matrix.est[k,(j-1)] <- est
    fed.cor.matrix.pval[k,(j-1)] <- p_val
    
    if (p_val <= 0.01){
      stars <- "$^{***}$"
    } else if (p_val <= 0.05){
      stars <- "$^{**}$"
    } else if (p_val <= 0.1){
      stars <- "$^{*}$"
    } else {
      stars <- ""
    }
    
    entry <- paste0(est, stars)
    p_val <- paste0("(", p_val, ")")
    
    fed.cor.matrix[i,j] <- entry
    fed.cor.matrix[i+1,j] <- p_val
    
    print(paste(s,t, i, j, est))
    
    j <- j + 1
  }
  
  i <- i + 3
  k <- k + 1
}

stargazer(as.matrix(fed.cor.matrix), title = "SPF dispersion and FOMC topic correlation matrix")

N <- nrow(fed.cor.matrix.est)*nrow(fed.cor.matrix.est)
N_offdiag <- N - nrow(fed.cor.matrix.est)
N_diag <- nrow(fed.cor.matrix.est)


est_diag <- sum(diag(fed.cor.matrix.est))/N_diag
est_offdiag <- (sum(fed.cor.matrix.est) - sum(diag(fed.cor.matrix.est)))/N_offdiag
pval_diag <- sum(diag(fed.cor.matrix.pval))/N_diag
pval_offdiag <- (sum(fed.cor.matrix.pval) - sum(diag(fed.cor.matrix.pval)))/N_offdiag









### Correlation matrix for NYT topics
nyt.cor.matrix <- data.frame(matrix(NA, nrow = 3*length(series), ncol = (1+length(series))))
nyt.cor.matrix.est <- matrix(NA, nrow = length(series), ncol = (length(series)))
nyt.cor.matrix.pval <- matrix(NA, nrow = length(series), ncol = (length(series)))
split_rows <- paste(as.vector(paste(series, "x n", "x n x")))
split_rows <- paste(split_rows, collapse = "")
split_rows <- (str_split(split_rows, "x"))[[1]]
colnames(nyt.cor.matrix) <- c("Series", series)
nyt.cor.matrix$Series <- str_replace_all(split_rows[1:42], "n", "")

i <- 1
k <- 1
s <- series[1]
t <- series[1]
for (s in series){
  
  # 
  disp_series <- total.panel[which(total.panel$series == s), "dispersion"]
  j <- 2
  for (t in series){
    
    
    nyt_series <- total.panel[which(total.panel$series == t), "news"]
    
    correlation <- cor.test(disp_series, nyt_series)
    
    est <- round(correlation$estimate, 3)
    p_val <- round(correlation$p.value, 3)
    
    nyt.cor.matrix.est[k,(j-1)] <- est
    nyt.cor.matrix.pval[k,(j-1)] <- p_val
    
    if (p_val <= 0.01){
      stars <- "$^{***}$"
    } else if (p_val <= 0.05){
      stars <- "$^{**}$"
    } else if (p_val <= 0.1){
      stars <- "$^{*}$"
    } else {
      stars <- ""
    }
    
    entry <- paste0(est, stars)
    p_val <- paste0("(", p_val, ")")
    
    nyt.cor.matrix[i,j] <- entry
    nyt.cor.matrix[i+1,j] <- p_val
    
    print(paste(s,t, i, j, est))
    
    j <- j + 1
  }
  
  i <- i + 3
  k <- k + 1
}

stargazer(as.matrix(nyt.cor.matrix), title = "SPF dispersion and NYT topic correlation matrix")

N <- nrow(nyt.cor.matrix.est)*nrow(nyt.cor.matrix.est)
N_offdiag <- N - nrow(nyt.cor.matrix.est)
N_diag <- nrow(nyt.cor.matrix.est)


est_diag <- sum(diag(nyt.cor.matrix.est))/N_diag
est_offdiag <- (sum(nyt.cor.matrix.est) - sum(diag(nyt.cor.matrix.est)))/N_offdiag
pval_diag <- sum(diag(nyt.cor.matrix.pval))/N_diag
pval_offdiag <- (sum(nyt.cor.matrix.pval) - sum(diag(nyt.cor.matrix.pval)))/N_offdiag





############################# Panel analysis on the series ############################# 


# SPF dispersion as depepndent variable
model1 <- felm(dispersion ~ plm::lag(fed, 0) + plm::lag(news, 0) | series, data = total.panel)
summary(model1)
model1_std <- felm(dispersion_std ~ plm::lag(fed_std, 0) + plm::lag(news_std, 0) | series, data = total.panel)
summary(model1_std)
model2 <- felm(dispersion ~ plm::lag(fed, 0:1) + plm::lag(news, 0:1) + plm::lag(dispersion, 1:3) | series + period, data = total.panel)
summary(model2)
model2_std <- felm(dispersion_std ~ plm::lag(fed_std, 0:1) + plm::lag(news_std, 0:1) + plm::lag(dispersion_std, 1:3) | series + period, data = total.panel)
summary(model2_std)


# Fed topics as dependent variable
model3 <- felm(fed ~ plm::lag(dispersion, 0) | series, data = total.panel)
summary(model3)
model3_std <- felm(fed_std ~ plm::lag(dispersion_std, 0) + plm::lag(news_std, 0) | series, data = total.panel)
summary(model3_std)
model4 <- felm(fed ~ plm::lag(dispersion, 0:1)  + plm::lag(fed, 1:3) | series + quarter, data = total.panel)
summary(model4)
model4_std <- felm(fed_std ~ plm::lag(dispersion_std, -1:1) +  plm::lag(news_std, -1:1) +
                     plm::lag(fed_std, 1:3) | series + quarter, data = total.panel)
summary(model4_std)

# NYT articles as dependent variable
model5 <- felm(news ~ plm::lag(dispersion, 0) | series, data = total.panel)
summary(model5)
model5_std <- felm(news_std ~ plm::lag(dispersion_std, 0) | series, data = total.panel)
summary(model5_std)
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