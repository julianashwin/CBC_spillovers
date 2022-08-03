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


col_theme <- scale_color_manual("Legend", values = c("FOMC minutes" = "darkblue",
                                                       "FOMC speeches" = "darkgreen",
                                                       "News articles" = "darkgrey"))
col_theme2 <- scale_color_manual("Legend", values = c("FOMC minutes" = "darkblue",
                                                       "SPF dispersion" = "firebrick",
                                                       "Tealbook update" = "darkcyan"))
                                                       #",Tealbook error" = "darkgoldenrod3"))

# Import the minutes data estimated at the meeting level
import_filename <- "data/topics_forecasts_panel.csv"
panel_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
panel_df <- pdata.frame(panel_df, index = c("variable", "period"))
panel_df$quarter <- as.Date(panel_df$quarter)

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
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 5.3, height = 5.3)

ggplot(qly_series_df, aes(x = quarter)) + theme_bw() + col_theme2 + 
  geom_line(aes(y = standardise(T3), color = "FOMC minutes"), alpha = 0.8) +
  geom_line(aes(y = standardise(CPI_dispersion), color = "SPF dispersion"), alpha = 0.8) +
  geom_line(aes(y = standardise(CPI_GB_update_abs_std), color = "Tealbook update"), alpha = 0.8) +
  #geom_line(aes(y = standardise(CPI_GB_now_error_abs), color = "Tealbook error")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/CPI_series.pdf"), width = 8, height = 3)


ss <- variables[2]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 21
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
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
topick_df <- topick_df[1:50,]
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
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6.5, height = 6.5)

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
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 8.5, height = 8.5)


ss <- variables[6]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 26
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6.8, height = 6.8)


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
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 8.0, height = 8.0)


ss <- variables[8]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 13
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6.8, height = 6.8)


ss <- variables[10]
series_df <- panel_df[which(panel_df$variable == ss),]
kk <- 11
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 6.9, height = 6.9)



"
Finance topic
"
kk <- 22
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 5.6, height = 5.6)

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
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/baseline_clouds/cloud_", kk,".pdf"), width = 7.9, height = 7.9)

ggplot(qly_series_df, aes(x = quarter)) + theme_bw() + col_theme + 
  geom_line(aes(y = standardise(T20), color = "FOMC minutes")) +
  geom_line(aes(y = standardise(T20_speech), color = "FOMC speeches")) +
  geom_line(aes(y = standardise(T20_news), color = "News articles")) +
  ylab("Std. units") + xlab("Quarter") 
ggsave(paste0("figures/fed_media_topics/series/euro_series.pdf"), width = 8, height = 3)





"
Plot an example topic for illustration
"

topics_quarterly_df$quarter <- as.Date(topics_quarterly_df$quarter)

topick_df <- topicwords_df[which(topicwords_df$topic == 10),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", 10,".pdf"), width = 4.5, height = 4.5)




ggplot(topics_quarterly_df, aes(x = quarter)) + theme_minimal() + col_theme + 
  geom_line(aes(y = T10, color = "Fed minutes")) +
  geom_line(aes(y = T10_news, color = "NYT articles")) +
  #geom_line(aes(y = dispersion_std, color = "SPF dispersion")) +
  #geom_line(aes(y = GB_update_abs_std, color = "GB update")) + 
  ylab("Topic proportion") + xlab("Quarter") + 
  ggtitle("Topic 10 proportions over time")

ggsave(paste0("figures/clouds/ts/fednyt_props_topic10.pdf"), width = 8, height = 4)










"
Regression tables
"

####### SPF dispersion ####### 
model1 <- felm(mins_std ~ plm::lag(disp_std, 0) | variable, data = panel_df)
summary(model1)
model2 <- felm(mins_std ~ plm::lag(disp_std, 0)  + plm::lag(mins_std, 1:7)| variable + period, data = panel_df)
summary(model2)
model3 <- felm(speeches_std ~ plm::lag(disp_std, 0) | variable, data = panel_df)
summary(model3)
model4 <- felm(speeches_std ~ plm::lag(disp_std, 0)  + plm::lag(news_std, 1:7)| variable + period, data = panel_df)
summary(model4)
model5 <- felm(news_std ~ plm::lag(disp_std, 0) | variable, data = panel_df)
summary(model5)
model6 <- felm(news_std ~ plm::lag(disp_std, 0)  + plm::lag(news_std, 1:7)| variable + period, data = panel_df)
summary(model6)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "Focus and SPF forecast dispersion",
          label = "tab:topic_spf_results")

####### GB update ####### 
model1 <- felm(fed_std ~ plm::lag(GB_update_abs_std, 0) | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ plm::lag(GB_update_abs_std, 0:1)  + plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ plm::lag(GB_update_abs_std, 0) | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ plm::lag(GB_update_abs_std, 0:1)  + plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and GB update",
          label = "tab:topic_gb_update_results")


####### SPF update ####### 
model1 <- felm(fed_std ~ plm::lag(SPF_update_abs_std, 0) | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ plm::lag(SPF_update_abs_std, 0:1)  + plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ plm::lag(SPF_update_abs_std, 0) | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ plm::lag(SPF_update_abs_std, 0:1)  + plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and SPF update",
          label = "tab:topic_spf_update_results")


####### GB error ####### 
model1 <- felm(fed_std ~ plm::lag(GB_now_error_abs_std, 0) | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ plm::lag(GB_now_error_abs_std, 0:1)  + plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ plm::lag(GB_now_error_abs_std, 0) | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ plm::lag(GB_now_error_abs_std, 0:1)  + plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and GB error",
          label = "tab:topic_gb_error_results")



####### SPF error ####### 
model1 <- felm(fed_std ~ plm::lag(SPF_now_error_abs_std, 0) | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ plm::lag(SPF_now_error_abs_std, 0:1)  + plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ plm::lag(SPF_now_error_abs_std, 0) | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ plm::lag(SPF_now_error_abs_std, 0:1)  + plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and SPF error",
          label = "tab:topic_spf_error_results")



####### SPF error ####### 
model1 <- felm(fed_std ~ plm::lag(GB_SPF_now_gap_abs_std, 0) | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ plm::lag(GB_SPF_now_gap_abs_std, 0:1)  + plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ plm::lag(GB_SPF_now_gap_abs_std, 0) | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ plm::lag(GB_SPF_now_gap_abs_std, 0:1)  + plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and GB-SPF gap",
          label = "tab:topic_gb_spf_gap_results")



####### SPF dispersion vs GB update ####### 
model1 <- felm(fed_std ~ dispersion_std + GB_update_abs_std | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ dispersion_std + GB_update_abs_std + 
                 plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ dispersion_std + GB_update_abs_std | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ dispersion_std + GB_update_abs_std + 
                 plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles, dispersion and update",
          label = "tab:topic_gb_spf_gap_results")





####### Throwing shit and seeing what sticks ####### 
model1 <- felm(fed_std ~ dispersion_std + GB_update_abs_std + SPF_update_abs_std + 
                 GB_now_error_abs_std + SPF_now_error_abs_std + GB_SPF_now_gap_abs_std 
               | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ dispersion_std + GB_update_abs_std + SPF_update_abs_std + 
                 GB_now_error_abs_std + SPF_now_error_abs_std + GB_SPF_now_gap_abs_std + 
                 plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ dispersion_std + GB_update_abs_std + SPF_update_abs_std + 
                 GB_now_error_abs_std + SPF_now_error_abs_std + GB_SPF_now_gap_abs_std 
               | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ dispersion_std + GB_update_abs_std + SPF_update_abs_std + 
                 GB_now_error_abs_std + SPF_now_error_abs_std + GB_SPF_now_gap_abs_std+ 
                 plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and a buncha stuff",
          label = "tab:topic_gb_spf_gap_results")





