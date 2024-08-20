####
# This file estimates topics on the Federal Reserve and NYT articles combined
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

library(stringr)
library(ggplot2)
library(ggwordcloud)
library(lfe)
library(plm)
library(fixest)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(stargazer)

standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
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


cor_mat_plot <- function(df, qual_vars){
  # Create correlation matrix
  cormat_hh <- rcorr(as.matrix(df[,qual_vars]))
  # Coefs
  cormat_hh_coef <- cormat_hh$r
  cormat_hh_coef[upper.tri(cormat_hh_coef)] <- NA
  diag(cormat_hh_coef) <- NA
  cormat_hh_coef <- tibble(melt(cormat_hh_coef, na.rm = T, value.name = "coef")) %>%
    filter(Var1 %in% qual_vars & Var2 %in% qual_vars) %>%
    mutate(coef = round(coef,2))
  # P-values
  cormat_hh_pval <- cormat_hh$P
  cormat_hh_pval[upper.tri(cormat_hh_pval)] <- NA
  diag(cormat_hh_pval) <- NA
  cormat_hh_pval <- tibble(melt(cormat_hh_pval, na.rm = T, value.name = "pval")) %>%
    filter(Var1 %in% qual_vars & Var2 %in% qual_vars) %>%
    mutate(alpha = case_when(pval < 0.1 ~ "sig", TRUE ~ "not")) %>%
    mutate(stars = case_when(pval < 0.01 ~ "***", pval < 0.05 ~ "**", pval < 0.1 ~ "*", TRUE ~ ""))
  # Merge back 
  cormat_hh <- left_join(cormat_hh_coef, cormat_hh_pval, by = c("Var1", "Var2"))
  # Plot matrix
  corplot_hh <- ggplot(cormat_hh, aes(x = (Var2), y = fct_rev(Var1), fill = coef)) + theme_minimal() + 
    geom_tile(color = "white") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1)) + 
    labs(x = "", y = "", fill = "Correlation") + 
    geom_text(aes(label = str_c(signif(coef,3),stars), alpha = alpha), color = "black", size = 2.3) + 
    scale_alpha_manual(values = c("sig" = 1, "not" = 0.5), guide = 'none') +
    ggtitle("")
  
  return(corplot_hh)
  
}


col_theme <- scale_color_manual("Legend", values = c("FOMC minutes" = "darkblue",
                                                       "FOMC speeches" = "darkgreen",
                                                       "News articles" = "darkgrey"))
col_theme2 <- scale_color_manual("Legend", values = c("FOMC minutes" = "darkblue",
                                                       "SPF dispersion" = "firebrick",
                                                       "Tealbook update" = "darkcyan"))
                                                       #,"Tealbook error" = "darkgoldenrod3"))

# Import the minutes data estimated at the meeting level
import_filename <- "data/topics_forecasts_panel.csv"
panel_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
panel_df <- pdata.frame(panel_df, index = c("variable", "period"))
# Lags of mins
panel_df$mins_1lag <- plm::lag(panel_df$mins,1)
panel_df$mins_2lag <- plm::lag(panel_df$mins,2)
panel_df$mins_3lag <- plm::lag(panel_df$mins,3)
# Lags of speeches
panel_df$speeches_1lag <- plm::lag(panel_df$speeches,1)
panel_df$speeches_2lag <- plm::lag(panel_df$speeches,2)
panel_df$speeches_3lag <- plm::lag(panel_df$speeches,3)
# Lags of media 
panel_df$news_1lag <- plm::lag(panel_df$news,1)
panel_df$news_2lag <- plm::lag(panel_df$news,2)
panel_df$news_3lag <- plm::lag(panel_df$news,3)
# Cut-off observations that don't have GB data
panel_df$quarter <- as.Date(panel_df$quarter)
panel_df <- panel_df[which(panel_df$quarter < "2017-01-01"),]


# Import the topic betas
import_filename <- "data/topic_data/overall/short_paragraph_topics_k29.csv"
topicwords_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import the topic betas
import_filename <- "data/topic_data/short_topics_summary_k29.csv"
topicsummary_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

import_filename = "data/topics_qly_baseline.csv"
qly_series_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
qly_series_df$quarter <- as.Date(qly_series_df$quarter)

spf_gb_df <- read.csv("data/spf_gb_panel.csv", stringsAsFactors = FALSE)
spf_gb_df$quarter <- as.Date(spf_gb_df$quarter)
spf_gb_df <- pivot_wider(spf_gb_df, id_cols = c(quarter), names_from = variable, 
                            names_glue = "{variable}_{.value}",
                            values_from = c(variable_act, dispersion, GB_update_abs_std,
                                            GB_now_error_abs, GB_SPF_now_gap_abs_std))
qly_series_df <- merge(qly_series_df[,which(!str_detect(names(qly_series_df), "disper"))], 
                       spf_gb_df, by = "quarter")
qly_series_df$quarter <- as.Date(qly_series_df$quarter)

variables <- as.character(unique(panel_df$variable))
variables_gb <- as.character(unique(filter(panel_df, !is.na(GB_update_abs_std))$variable))

"
Check correlations
"


# Are errors correlated across variables?
# Residualise errors against a time fixed effect
resids_df <- panel_df[which(!is.na(panel_df$GB_update_abs_std)),]
model <- feols(GB_update_abs_std ~ 0 | variable + period, resids_df)
resids_df$GB_update_abs_resids <- model$residuals
update_df <- resids_df %>%
  pivot_wider(id_cols = quarter, names_from = variable, values_from = GB_update_abs_resids)

cor_mat_plot(update_df, variables_gb)


x <- mutate(panel_df, variable = "Overall")
cor.test(x$mins_std, x$GB_update_abs_std)

# Estimates
est_df <- panel_df %>%
  mutate(GB_update_abs_std = replace_na(GB_update_abs_std,0),
         GB_SPF_error_diff_std = replace_na(GB_SPF_error_diff_std,0),
         GB_SPF_now_gap_abs_std = replace_na(GB_SPF_now_gap_abs_std,0)) %>%
  rbind(mutate(panel_df, variable = "Overall")) %>%
  pivot_longer(cols = c(mins_std, speeches_std, news_std), names_to = "text", values_to = "focus") %>%
  mutate(text = case_when(str_detect(text, "mins") ~ "FOMC Minutes", str_detect(text, "speech") ~ "FOMC Speeches",
                          str_detect(text, "news") ~ "NYT articles")) %>%
  group_by(variable, text) %>%
  summarise(`disp` = cor.test(focus, disp_std, na.rm = T)$estimate,
            `s` = case_when(any(!is.na(GB_nowcast)) ~ cor.test(focus, GB_update_abs_std, na.rm = T)$estimate, TRUE ~ NA_real_),
            `nu` = case_when(any(!is.na(GB_nowcast)) ~ cor.test(focus, GB_SPF_error_diff_std, na.rm = T)$estimate, TRUE ~ NA_real_),
            `varepsilon` = case_when(any(!is.na(GB_nowcast)) ~ cor.test(focus, GB_SPF_now_gap_abs_std, na.rm = T)$estimate, TRUE ~ NA_real_)) %>%
  pivot_longer(cols = c(-variable, -text), values_to = "coef", names_to = "correlation")
# p-vals
pval_df <- panel_df %>%
  mutate(GB_update_abs_std = replace_na(GB_update_abs_std,0),
         GB_SPF_error_diff_std = replace_na(GB_SPF_error_diff_std,0),
         GB_SPF_now_gap_abs_std = replace_na(GB_SPF_now_gap_abs_std,0)) %>%
  rbind(mutate(panel_df, variable = "Overall")) %>%
  pivot_longer(cols = c(mins_std, speeches_std, news_std), names_to = "text", values_to = "focus") %>%
  mutate(text = case_when(str_detect(text, "mins") ~ "FOMC Minutes", str_detect(text, "speech") ~ "FOMC Speeches",
                          str_detect(text, "news") ~ "NYT articles")) %>%
  group_by(variable, text) %>%
  summarise(`disp` = cor.test(focus, disp_std, na.rm = T)$p.value,
            `s` = case_when(any(!is.na(GB_nowcast)) ~ cor.test(focus, GB_update_abs_std, na.rm = T)$p.value, TRUE ~ NA_real_),
            `nu` = case_when(any(!is.na(GB_nowcast)) ~ cor.test(focus, GB_SPF_error_diff_std, na.rm = T)$p.value, TRUE ~ NA_real_),
            `varepsilon` = case_when(any(!is.na(GB_nowcast)) ~ cor.test(focus, GB_SPF_now_gap_abs_std, na.rm = T)$p.value, TRUE ~ NA_real_)) %>%
  pivot_longer(cols = c(-variable, -text), values_to = "pval", names_to = "correlation")
# Create correlation matrix
cormat_df <- left_join(est_df, pval_df, by = c("variable", "text", "correlation")) %>%
  mutate(variable = factor(variable, levels = c(variables, "Overall"), ordered = T)) %>%
  mutate(correlation = factor(correlation, levels = c("disp", "s", "varepsilon", "nu"), ordered = T))
# Plot matrix
cormat_df %>%
  filter(!is.na(coef)) %>%
  mutate(alpha = case_when(pval < 0.1 ~ "sig", TRUE ~ "not")) %>%
  mutate(stars = case_when(pval < 0.01 ~ "***", pval < 0.05 ~ "**", pval < 0.1 ~ "*", TRUE ~ "")) %>%
  ggplot(aes(x = correlation, y = fct_rev(variable), fill = coef)) + theme_minimal() + 
  facet_wrap(~text) +
  scale_x_discrete(labels=c(expression(disp[kt]^SPF), 
                            expression(paste("|",s[kt],"|")), 
                            expression(paste("|",s[kt]-q[kt],"|")), 
                            expression(paste("|",sigma^CB-sigma^PS,"|")))) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1)) + 
  labs(x = "", y = "", fill = "Correlation") + 
  geom_text(aes(label = str_c(signif(coef,3),stars), alpha = alpha), color = "black", size = 2.3) + 
  scale_alpha_manual(values = c("sig" = 1, "not" = 0.5), guide = 'none')
ggsave(paste0("figures/fed_media_topics/correlations.pdf"), width = 9, height = 4)



series_df <- panel_df[which(panel_df$variable == "NGDP"),]
cor.test(series_df$mins_std, series_df$GB_SPF_now_gap_abs_std)

cor.test(panel_df$mins_std, panel_df$GB_SPF_now_gap_abs_std)

"
Manually do all the word clouds like a loser (need to adjust size so that all the words fit)
"
ss <- variables[1]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 3
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 5, height = 5)

ggplot(series_df, aes(x = quarter)) + theme_bw() + col_theme2 + 
  geom_line(aes(y = mins_std, color = "FOMC minutes"), alpha = 0.8) +
  geom_line(aes(y = disp_std, color = "SPF dispersion"), alpha = 0.8) +
  geom_line(aes(y = GB_update_abs_std, color = "Tealbook update"), alpha = 0.8) +
  #geom_line(aes(y = GB_SPF_error_diff_std, color = "Tealbook error")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/CPI_series.pdf"), width = 8, height = 3)


ss <- variables[2]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 21
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 5.5, height = 5.5)

ss <- variables[3]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 29
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6.5, height = 6.5)


ss <- variables[4]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 8
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6, height = 6)

ggplot(qly_series_df, aes(x = quarter)) + theme_bw() + 
  scale_color_manual("Legend", values = c("FOMC minutes" = "darkblue",
                                                        "SPF dispersion" = "firebrick")) +
  geom_line(aes(y = standardise(T8), color = "FOMC minutes"), alpha = 0.8) +
  geom_line(aes(y = standardise(RRESINV_dispersion), color = "SPF dispersion"), alpha = 0.8) +
  #geom_line(aes(y = standardise(CPI_GB_now_error_abs), color = "Tealbook error")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/RRESINV_series.pdf"), width = 8, height = 3)




ss <- variables[5]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 12
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 7., height = 7.)


ss <- variables[6]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 26
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6., height = 6.)


ggplot(qly_series_df, aes(x = quarter)) + theme_bw() + col_theme2 + 
  geom_line(aes(y = standardise(T26), color = "FOMC minutes"), alpha = 0.8) +
  geom_line(aes(y = standardise(RGDP_dispersion), color = "SPF dispersion"), alpha = 0.8) +
  geom_line(aes(y = standardise(RGDP_GB_update_abs_std), color = "Tealbook update"), alpha = 0.8) +
  #geom_line(aes(y = standardise(RGDP_GB_now_error_abs), color = "Tealbook error")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/RGDP_series.pdf"), width = 8, height = 3)




ss <- variables[7]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 18
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 7.0, height = 7.0)


ss <- variables[8]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 13
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6., height = 6.)


ss <- variables[10]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 11
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6., height = 6.)



"
Finance topic
"
kk <- 22
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 5., height = 5.)

ggplot(qly_series_df, aes(x = quarter)) + theme_bw() + col_theme + 
  geom_line(aes(y = standardise(T22), color = "FOMC minutes")) +
  geom_line(aes(y = standardise(T22_speech), color = "FOMC speeches")) +
  geom_line(aes(y = standardise(T22_news), color = "News articles")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/finance_series.pdf"), width = 8, height = 3)


#vix_df <- read.csv("data/SPF/VIXCLS.csv", stringsAsFactors = F)
#vix_df$VIX <- as.numeric(vix_df$VIXCLS)
#vix_df <- vix_df[complete.cases(vix_df),]
#vix_df$DATE <- as.Date(vix_df$DATE)
#vix_df$quarter <- floor_date(vix_df$DATE, unit = "quarters")
#vix_df <- aggregate(vix_df[,c("VIX", "VIXCLS")], by = list(quarter = vix_df$quarter), FUN = mean)

#plot_df <- merge(qly_series_df, vix_df, by = "quarter")
#plot_df$quarter <- as.Date(plot_df$quarter)





"
Euro crisis topic
"
kk <- 20
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:30,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 7.5, height = 7.5)

ggplot(qly_series_df, aes(x = quarter)) + theme_bw() + col_theme + 
  geom_line(aes(y = standardise(T20), color = "FOMC minutes")) +
  geom_line(aes(y = standardise(T20_speech), color = "FOMC speeches")) +
  geom_line(aes(y = standardise(T20_news), color = "News articles")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/euro_series.pdf"), width = 8, height = 3)






cor.test(panel_df$disp_std, panel_df$GB_now_error_abs_std)



panel_df <- panel_df %>%
  group_by(variable) %>%
  mutate(disp_1lag = lag(disp_std, order_by = quarter), 
         disp_1lead = lead(disp_std, order_by = quarter))



panel_df$disp_1lag

"
Regression tables
"



####### SPF dispersion ####### 
# To test for autocorrelation in residuals
model <- model2
summary(lm(model$residuals[-length(model$residuals)] ~ model$residuals[-1]) )
# Mins just topic FE
reg_formula <- formula(mins_std ~ disp_std |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
# Mins period FE and lags
reg_formula <- formula(mins_std ~  disp_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
# Speeches just topic FE
reg_formula <- formula(speeches_std ~ disp_std |variable)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
# Speeches period FE and lags
reg_formula <- formula(speeches_std ~ disp_std + speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable + period)
model4 <- felm_DK_se(reg_formula, panel_df)
summary(model4)
# News just topic FE
reg_formula <- formula(news_std ~ disp_std |variable)
model5 <- felm_DK_se(reg_formula, panel_df)
summary(model5)
# News period FE and lags
reg_formula <- formula(news_std ~ disp_std + news_std_1lag+news_std_2lag+news_std_3lag|variable + period)
model6 <- felm_DK_se(reg_formula, panel_df)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "Focus and SPF forecast dispersion",
          label = "tab:topic_spf_results")

####### GB update ####### 
# Mins just topic FE
reg_formula <- formula(mins_std ~ GB_update_abs_std |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
# Mins period FE and lags
reg_formula <- formula(mins_std ~ GB_update_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
# Speeches just topic FE
reg_formula <- formula(speeches_std ~ GB_update_abs_std |variable)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
# Speeches period FE and lags
reg_formula <- formula(speeches_std ~ GB_update_abs_std + speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable + period)
model4 <- felm_DK_se(reg_formula, panel_df)
summary(model4)
# News just topic FE
reg_formula <- formula(news_std ~ GB_update_abs_std |variable)
model5 <- felm_DK_se(reg_formula, panel_df)
summary(model5)
# News period FE and lags
reg_formula <- formula(news_std ~ GB_update_abs_std + news_std_1lag+news_std_2lag+news_std_3lag|variable + period)
model6 <- felm_DK_se(reg_formula, panel_df)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "Focus and Tealbook updates",
          label = "tab:topic_gb_update_results")


panel_df$GB_SPF_error_ratio <- log(1+panel_df$GB_now_error_abs) - log(1+panel_df$SPF_now_error_abs)
panel_df <- panel_df %>%
  group_by(variable) %>%
  mutate(GB_SPF_error_ratio_std =standardise(GB_SPF_error_ratio))

####### Error diff ####### 
# Mins just topic FE
reg_formula <- formula(mins_std ~ GB_SPF_error_diff_std |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
# Mins period FE and lags
reg_formula <- formula(mins_std ~ GB_SPF_error_diff_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
# Speeches just topic FE
reg_formula <- formula(speeches_std ~ GB_SPF_error_diff_std |variable)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
# Speeches period FE and lags
reg_formula <- formula(speeches_std ~ GB_SPF_error_diff_std + speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable + period)
model4 <- felm_DK_se(reg_formula, panel_df)
summary(model4)
# News just topic FE
reg_formula <- formula(news_std ~ GB_SPF_error_diff_std |variable)
model5 <- felm_DK_se(reg_formula, panel_df)
summary(model5)
# News period FE and lags
reg_formula <- formula(news_std ~ GB_SPF_error_diff_std + news_std_1lag+news_std_2lag+news_std_3lag|variable + period)
model6 <- felm_DK_se(reg_formula, panel_df)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "Focus and Tealbook SPF error diff",
          label = "tab:topic_gb_error_results")



####### GB-SPF gap update ####### 
# Mins just topic FE
reg_formula <- formula(mins_std ~ GB_SPF_now_gap_abs_std |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
# Mins period FE and lags
reg_formula <- formula(mins_std ~ GB_SPF_now_gap_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
# Speeches just topic FE
reg_formula <- formula(speeches_std ~ GB_SPF_now_gap_abs_std |variable)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
# Speeches period FE and lags
reg_formula <- formula(speeches_std ~ GB_SPF_now_gap_abs_std + speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable + period)
model4 <- felm_DK_se(reg_formula, panel_df)
summary(model4)
# News just topic FE
reg_formula <- formula(news_std ~ GB_SPF_now_gap_abs_std |variable)
model5 <- felm_DK_se(reg_formula, panel_df)
summary(model5)
# News period FE and lags
reg_formula <- formula(news_std ~ GB_SPF_now_gap_abs_std + news_std_1lag+news_std_2lag+news_std_3lag|variable + period)
model6 <- felm_DK_se(reg_formula, panel_df)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "Focus and Tealbook nowcast gap",
          label = "tab:topic_spf_gb_now_gap_results")


### Including all three
summary(lm(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std + GB_SPF_now_gap_abs_std, panel_df))
reg_formula <- formula(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std + GB_SPF_now_gap_abs_std
                         |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
reg_formula <- formula(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std + GB_SPF_now_gap_abs_std+ 
                         mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
reg_formula <- formula(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std  + GB_SPF_now_gap_abs_std + news_std + 
                         mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
stargazer(model1, model2, model3, table.placement = "H", df = FALSE,
          title = "Focus and four uncertainty measures",
          label = "tab:topic_four_results")




####### Robustness ####### 
# Dispersion
model1 <- felm(mins_std ~ disp_std + mins_1lag+mins_2lag+mins_3lag | variable + period, data = panel_df)
summary(model1)
form_topspec <- formula(mins_std ~ disp_std + variable*mins_std_1lag-variable + 
                   variable*mins_std_2lag-variable + variable*mins_std_3lag-variable | 
                   variable + period)
model2 <- felm_DK_se(form_topspec, panel_df)
summary(model2)
form_unstd <- formula(mins ~ disp_std + mins_1lag+mins_2lag+mins_3lag | variable + period)
model3 <- felm_DK_se(form_unstd, panel_df)
summary(model3)
# Update
model4 <- felm(mins_std ~ GB_update_abs_std + mins_1lag+mins_2lag+mins_3lag | variable + period, data = panel_df)
summary(model4)
form_topspec <- formula(mins_std ~ GB_update_abs_std + variable*mins_std_1lag-variable + 
                          variable*mins_std_2lag-variable + variable*mins_std_3lag-variable | 
                          variable + period)
model5 <- felm_DK_se(form_topspec, panel_df)
summary(model5)
form_unstd <- formula(mins ~ GB_update_abs_std + mins_1lag+mins_2lag+mins_3lag | variable + period)
model6 <- felm_DK_se(form_unstd, panel_df)
summary(model6)
# Error diff
model7 <- felm(mins_std ~ GB_SPF_error_diff_std + mins_1lag+mins_2lag+mins_3lag | variable + period, data = panel_df)
summary(model7)
form_topspec <- formula(mins_std ~ GB_SPF_error_diff_std + variable*mins_std_1lag-variable + 
                          variable*mins_std_2lag-variable + variable*mins_std_3lag-variable | 
                          variable + period)
model8 <- felm_DK_se(form_topspec, panel_df)
summary(model8)
form_unstd <- formula(mins ~ GB_SPF_error_diff_std + mins_1lag+mins_2lag+mins_3lag | variable + period)
model9 <- felm_DK_se(form_unstd, panel_df)
summary(model9)



stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes and uncertainty robustness",
          label = "tab:mins_uncert_robust")
stargazer(model7, model8, model9,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes and uncertainty robustness II",
          label = "tab:mins_uncert_robust")











####### GB error ####### 
# Mins just topic FE
reg_formula <- formula(mins_std ~ GB_now_error_abs_std |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
# Mins period FE and lags
reg_formula <- formula(mins_std ~ GB_now_error_abs_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
# Speeches just topic FE
reg_formula <- formula(speeches_std ~ GB_now_error_abs_std |variable)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
# Speeches period FE and lags
reg_formula <- formula(speeches_std ~ GB_now_error_abs_std + speeches_std_1lag+speeches_std_2lag+speeches_std_3lag|variable + period)
model4 <- felm_DK_se(reg_formula, panel_df)
summary(model4)
# News just topic FE
reg_formula <- formula(news_std ~ GB_now_error_abs_std |variable)
model5 <- felm_DK_se(reg_formula, panel_df)
summary(model5)
# News period FE and lags
reg_formula <- formula(news_std ~ GB_now_error_abs_std/ + news_std_1lag+news_std_2lag+news_std_3lag|variable + period)
model6 <- felm_DK_se(reg_formula, panel_df)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "Focus and Tealbook error",
          label = "tab:topic_gb_error_results")



