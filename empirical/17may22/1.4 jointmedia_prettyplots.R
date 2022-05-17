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



col_theme <- scale_color_manual("Legend", 
                                values = c("SPF dispersion" = "darkturquoise", 
                                           "Fed minutes" = "blue3", 
                                           "GB update" = "forestgreen",
                                           "NYT articles" = "dimgray"))

# Import the minutes data estimated at the meeting level
import_filename <- "data/jointmedia_spf_gb_panel.csv"
panel_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import the topic betas
import_filename <- "data/joint_paragraph_topics.csv"
topicwords_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


import_filename = "data/joint_topics_quarterly.csv"
topics_quarterly_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


"
Add topwords to panel data
"

summary_df <- data.frame(topic = unique(topicwords_df$topic), topwords = "")
for (kk in unique(topicwords_df$topic)){
  topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
  topick_df <- topick_df[order(-topick_df$beta),]
  topick_df <- topick_df[1:5,]
  summary_df$topwords[which(summary_df$topic == kk)] <- paste(topick_df$term, collapse = ", ")
}

panel_df <- merge(panel_df, summary_df, by = "topic", all.x = TRUE)
panel_df <- pdata.frame(panel_df, index = c("series", "period"))
panel_df$quarter <- as.Date(panel_df$quarter)

series <- as.character(unique(panel_df$series))

"
Check correlations
"
series_df <- panel_df[which(panel_df$series == "NGDP"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "RGDP"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "CPI"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "EMP"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "UNEMP"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "INDPROD"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "HOUSING"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "RRESINV"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "RCONSUM"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "RNRESIN"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "RFEDGOV"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)
series_df <- panel_df[which(panel_df$series == "RSLGOV"),]
cor.test(series_df$fed_std, series_df$GB_update_abs_std)



"
Manually do all the word clouds like a loser (need to adjust size so that all the words fit)
"
ss <- series[1]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 5, height = 5)

ss <- series[2]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 5.1, height = 5.1)

ss <- series[3]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 5.6, height = 5.6)


ss <- series[4]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 7.1, height = 7.1)


ss <- series[5]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 7.1, height = 7.1)


ss <- series[6]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 5.5, height = 5.5)


ss <- series[7]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 7.6, height = 7.6)


ss <- series[8]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 6, height = 6)


ss <- series[10]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 6.5, height = 6.5)


ss <- series[11]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 6, height = 6)


ss <- series[11]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 6, height = 6)


ss <- series[13]
series_df <- panel_df[which(panel_df$series == ss),]
kk <- unique(series_df$topic)
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:50,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_margin = 0.5,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/clouds/ts/joint_paragraph_topics", kk,".pdf"), width = 5, height = 5)





for (ss in series){
  series_df <- panel_df[which(panel_df$series == ss),]
  kk <- unique(series_df$topic)
  ggplot(series_df, aes(x = quarter)) + theme_minimal() + col_theme + 
    geom_line(aes(y = fed_std, color = "Fed minutes")) +
    #geom_line(aes(y = news_std, color = "NYT articles")) +
    geom_line(aes(y = dispersion_std, color = "SPF dispersion")) +
    geom_smooth(aes(y = GB_update_abs_std, color = "GB update"), method = "loess") + 
    ylab("Std. units") + xlab("Quarter") + 
    ggtitle(paste0(ss, " and Topic ", kk, " (", series_df$topwords[1] ,")"))
  ggsave(paste0("figures/clouds/ts/joint_ts_", ss,".pdf"), width = 8, height = 4)
  
}




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
model1 <- felm(fed_std ~ plm::lag(dispersion_std, 0) | series, data = panel_df)
summary(model1)
model2 <- felm(fed_std ~ plm::lag(dispersion_std, 0:1)  + plm::lag(fed_std, 1:3)| series + period, data = panel_df)
summary(model2)
model3 <- felm(news_std ~ plm::lag(dispersion_std, 0) | series, data = panel_df)
summary(model3)
model4 <- felm(news_std ~ plm::lag(dispersion_std, 0:1)  + plm::lag(news_std, 1:3)| series + period, data = panel_df)
summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and SPF forecast dispersion",
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





