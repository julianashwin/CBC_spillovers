####
# This file estimates topics on the Federal Reserve and NYT articles combined
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(ggplot2)
require(ggwordcloud)



col_theme <- scale_color_manual("Legend", 
                                values = c("SPF dispersion" = "darkturquoise", 
                                           "Fed minutes" = "blue3", 
                                           "NYT articles" = "dimgray"))

# Import the minutes data estimated at the meeting level
import_filename <- "data/jointmedia_spf_gb_panel.csv"
panel_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import the topic betas
import_filename <- "data/joint_paragraph_topics.csv"
topicwords_df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


summary_df <- data.frame(topic = unique(topicwords_df$topic), topwords = "")
for (kk in unique(topicwords_df$topic)){
  topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
  topick_df <- topick_df[order(-topick_df$beta),]
  topick_df <- topick_df[1:5,]
  summary_df$topwords[which(summary_df$topic == kk)] <- paste(topick_df$term, collapse = ", ")
}

kk <- 16
topick_df <- topicwords_df[which(topicwords_df$topic == kk),]
topick_df <- topick_df[order(-topick_df$beta),]
topick_df <- topick_df[1:100,]
topick_df$beta <- 100000*topick_df$beta/sum(topick_df$beta)
ggplot(topick_df, aes(label = term, size = beta, color = beta)) + theme_minimal() +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9) + 
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkblue", high = "blue")
ggsave(paste0("figures/ts/joint_paragraph_topics", kk,".pdf"))


