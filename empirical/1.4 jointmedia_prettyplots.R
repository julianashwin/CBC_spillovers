####
# This file estimates topics on the Federal Reserve and NYT articles combined
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(ggplot2)
require(ggwordcloud)
require(lfe)
require(plm)

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
                                            GB_now_error_abs))
qly_series_df <- merge(qly_series_df[,which(!str_detect(names(qly_series_df), "disper"))], 
                       spf_gb_df, by = "quarter")
qly_series_df$quarter <- as.Date(qly_series_df$quarter)

variables <- as.character(unique(panel_df$variable))

"
Check correlations
"
series_df <- panel_df[which(panel_df$variable == "NGDP"),]
cor.test(series_df$mins_std, series_df$GB_update_abs_std)



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
reg_formula <- formula(mins_std ~ disp_std + mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
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


### Including all three
summary(lm(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std, panel_df))
reg_formula <- formula(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std
                         |variable)
model1 <- felm_DK_se(reg_formula, panel_df)
summary(model1)
reg_formula <- formula(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std + 
                         mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model2 <- felm_DK_se(reg_formula, panel_df)
summary(model2)
reg_formula <- formula(mins_std ~ disp_std + GB_update_abs_std + GB_SPF_error_diff_std + news_std + 
                         mins_std_1lag+mins_std_2lag+mins_std_3lag|variable + period)
model3 <- felm_DK_se(reg_formula, panel_df)
summary(model3)
stargazer(model1, model2, model3, table.placement = "H", df = FALSE,
          title = "Focus and three uncertainty measures",
          label = "tab:topic_three_results")




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



