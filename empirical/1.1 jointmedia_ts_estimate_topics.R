####
# This file estimates topics on the Federal Reserve and NYT articles combined
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

### Define the directories where raw data is stored and clean will be saved
import_dir <- "data/clean_text/"
export_dir <- "data/topic_data/"
fig_dir <- "figures/fed_media_topics/"


### Import the text data
clean_filename = paste0(import_dir, "fedminutes_clean.csv")
fedminutes_df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

clean_filename = paste0(import_dir, "fedspeeches_clean.csv")
fedspeeches_df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

clean_filename = paste0(import_dir, "NYT_clean_90s.csv")
nyt_df1 <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
clean_filename = paste0(import_dir, "NYT_clean_00s.csv")
nyt_df2 <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
clean_filename = paste0(import_dir, "NYT_clean_10s.csv")
nyt_df3 <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

nyt_df <- rbind(nyt_df1, nyt_df2, nyt_df3)
rm(nyt_df1,nyt_df2,nyt_df3)



### Combine articles into a corpus with minutes, speeches and some articles
total_df <- nyt_df[which(!is.na(nyt_df$subsequent_meeting) | !is.na(nyt_df$recent_meeting) | 
                     !is.na(nyt_df$recent_speech) | !is.na(nyt_df$subsequent_speech)),
                         c("unique_id", "paragraph_clean", "sentiment")]
total_df <- rbind(total_df, fedminutes_df[,c("unique_id", "paragraph_clean", "sentiment")])
total_df <- rbind(total_df, fedspeeches_df[,c("unique_id", "paragraph_clean", "sentiment")])






############################# Convert to labelled DTM ############################# 

total_corpus <- Corpus(VectorSource(unlist(total_df[, "paragraph_clean"])))
total_dtm <- DocumentTermMatrix(total_corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of total_dtm are", dim(total_dtm)[1], "documents and", 
            dim(total_dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
total_dtm$dimnames$Docs <- total_df$unique_id
fed_dtm <- total_dtm[str_detect(total_dtm$dimnames$Docs, "FEDp"),]


# Calculate the tfidf score for each term
term_tfidf <-tapply(total_dtm$v/row_sums(total_dtm)[total_dtm$i], total_dtm$j, mean) *
  log2(nDocs(total_dtm)/col_sums(total_dtm > 0))
summary(term_tfidf)
quantile(term_tfidf, c(.01, .5, .99)) 

tfidf_df <- data.frame(term = total_dtm$dimnames$Terms, tfidf = term_tfidf,
                       tf = col_sums(total_dtm), df = col_sums(total_dtm > 0))

low_terms <- total_dtm[,which(tfidf_df$tf > 2 & tfidf_df$df < )]$dimnames$Terms
print(low_terms)

# Remove terms in the bottom 1% of the tf-idf ranking
total_dtm <- total_dtm[,term_tfidf >= 0.0122]

# Remove all empty documents
total_dtm <- total_dtm[row_sums(total_dtm) > 0,]
print(paste("After removing low tf-idf terms, the dimensions of total_dtm are now", 
            dim(total_dtm)[1], "documents and", dim(total_dtm)[2], "words in vocab"))


fed_dtm <- total_dtm[str_detect(total_dtm$dimnames$Docs, "FEDp"),]
print(paste("After removing low tf-idf terms, the dimensions of fed_dtm are now", 
            dim(fed_dtm)[1], "documents and", dim(fed_dtm)[2], "words in vocab"))




############################# Estimate the topics on the paragraphs and articles ############################# 

k <- 30
paragraph.lda <- LDA(total_dtm, k = k, method = "Gibbs", 
                     control = list(verbose = 100, burnin = 1000, thin = 100, iter = 20000))
paragraph_lda <- LDA(total_dtm, k = k, method = "VEM")

# paragraph.lda <- LDA(paragraph.dtm, k = k, control = list( verbose = 1))
paragraph_lda

saveRDS(paragraph_lda, file = paste0(export_dir, "joint_paragraph_lda.rds"))
paragraph_lda1 <- readRDS(file = paste0(export_dir, "joint_paragraph_lda.rds"))
### Store the topic beta vectors
paragraph_topics <- tidy(paragraph_lda, matrix = "beta")
paragraph_topics


# Write the topic vectors to file
export_filename = paste0(export_dir, "joint_paragraph_topics.csv")
write.csv(paragraph_topics, export_filename, fileEncoding = "utf-8", row.names = FALSE)
# nyt_relevant <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)



# Identify the top ten terms for each topic
top_terms <- paragraph_topics %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot these top ten terms for each topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
ggsave(paste0(fig_dir, "all_topics.pdf"), width = 12, height = 9)

### Store the topic proportion gamma/theta vectors
paragraph_gamma <- tidy(paragraph_lda, matrix = "gamma")
paragraph_gamma


### Store the topic assignment of each word 
paragraph_assignments <- augment(paragraph_lda, data = total_dtm)




############################# Separate out again ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
paragraph_posterior <- posterior(paragraph_lda)

# Extract the article topics with unique_id
individual_topics <- as.data.frame(paragraph_posterior$topic)
colnames(individual_topics) <- paste0("T", colnames(paragraph_posterior$topics))
individual_topics[,paste0("T", 1:k, "_sent")] <- individual_topics[,paste0("T", 1:k)]
individual_topics$unique_id <- rownames(individual_topics)


### Split back into NYT and minutes 
speechtopics <- individual_topics[which(str_detect(individual_topics$unique_id, "SPEECHp")),]
articletopics <- individual_topics[which(str_detect(individual_topics$unique_id, "nyt")),]



############################# Find the means of the meeting paragraphs ############################# 

meetingtopics <- merge(fedminutes_df[,c("unique_id", "meeting_id", "meet_date","pub_date", 
                                        "quarter", "sentiment")], 
                       individual_topics[which(str_detect(individual_topics$unique_id, "FEDp")),], 
                       by = "unique_id")
sent_topics <- which(str_detect(names(meetingtopics), "_sent"))
meetingtopics[,sent_topics] <- meetingtopics[,sent_topics]*meetingtopics$sentiment


meetinglevel_means <- meetingtopics %>%
  select(-c(unique_id, sentiment)) %>%
  group_by(meeting_id,meet_date,pub_date, quarter) %>%
  summarise_all(funs(mean))
meetinglevel_means <- meetinglevel_means[order(meetinglevel_means$meet_date),]


ggplot(meetinglevel_means) + theme_bw() + 
  geom_line(aes(x = as.Date(meet_date), y = T18))




############################# Find the means of the weekly newspaper ############################# 

# Create a data-frame with the key info for each meeting
articletopics <- merge(nyt_df[], articletopics, by = "unique_id")

articletopics <- subset(articletopics, select=-c(paragraph_clean))
articlelevel.means <- articlelevel.short %>%
  group_by(meeting) %>%
  summarise_all(funs(mean))

articlemeeting_key <- unique(articles_key[,c("meeting", "subsequent_meeting", "recent_meeting")])

articlelevel.means <- merge(articlelevel.means, articlemeeting_key, by = c("meeting"), all.x = TRUE)







############################# Write the average topic props ############################# 

# Write the meeting topic distributions to file
clean_filename = paste(clean_dir, "CBC/fedmeetingmeans_jointtopics.csv", sep = "/")
write.csv(meetinglevel.means, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# meeting.topics <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Write the meeting topic distributions to file
clean_filename = paste(clean_dir, "CBC/articlemeans_jointtopics.csv", sep = "/")
write.csv(articlelevel.means, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# meeting.topics <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)










############################# Combine the paragraphs into documents ############################# 

# Collapse paragraphs into whole meeting documents
meetinglevel.df <- fedminutes.df %>%
  group_by(meeting_id) %>%
  summarise(text=paste(paragraph_clean,collapse=' ')) %>%
  rename(meeting = meeting_id)

articles_combined <- nyt_relevant %>%
  group_by(meeting) %>%
  summarise(text=paste(text_clean,collapse=' '))

total_combined <- rbind(meetinglevel.df, articles_combined)



combined.corpus <- Corpus(VectorSource(unlist(total_combined[, "text"])))
combined.dtm <- DocumentTermMatrix(combined.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of combined.dtm are", dim(combined.dtm)[1], "documents and", 
            dim(combined.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later
combined.dtm$dimnames$Docs <- total_combined$meeting



############################# Estimate paragraph-topic props at meeting and weekly level ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
combined.posterior <- posterior(paragraph.lda, newdata = combined.dtm)

combined.topics <- as.data.frame(combined.posterior$topic)
colnames(combined.topics) <- paste0("T", colnames(combined.posterior$topics))
combined.topics$meeting <- rownames(combined.topics)

meetingtopics.combined <- combined.topics[which(!str_detect(combined.topics$meeting, "p")),]
articletopics.combined <- combined.topics[which(str_detect(combined.topics$meeting, "p")),]


articlelevel.combined <- merge(articletopics.combined, articlemeeting_key, by = c("meeting"), all.x = TRUE)






############################# Write the combined document topic props ############################# 

# Write the meeting topic distributions to file
clean_filename = paste(clean_dir, "CBC/fedmeetingcombined_jointtopics.csv", sep = "/")
write.csv(meetingtopics.combined, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# meeting.topics <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Write the meeting topic distributions to file
clean_filename = paste(clean_dir, "CBC/articlecombined_jointtopics.csv", sep = "/")
write.csv(articlelevel.combined, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# meeting.topics <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)





meetinglevel.means <- meetinglevel.means[order(meetinglevel.means$meeting_id),]
meetingtopics.combined <- meetingtopics.combined[order(meetinglevel.means$meeting_id),]
articlelevel.means <- articlelevel.means[order(articlelevel.means$meeting),]
articlelevel.combined <- articlelevel.combined[order(articlelevel.combined$meeting),]

cor.test(meetinglevel.means$T1, meetingtopics.combined$T1)
cor.test(meetinglevel.means$T3, meetingtopics.combined$T3)
cor.test(meetinglevel.means$T4, meetingtopics.combined$T4)
cor.test(meetinglevel.means$T8, meetingtopics.combined$T8)
cor.test(meetinglevel.means$T10, meetingtopics.combined$T10)



cor.test(articlelevel.means$T1, articlelevel.combined$T1)
cor.test(articlelevel.means$T3, articlelevel.combined$T3)
cor.test(articlelevel.means$T4, articlelevel.combined$T4)
cor.test(articlelevel.means$T8, articlelevel.combined$T8)
cor.test(articlelevel.means$T10, articlelevel.combined$T10)






############################# Transformed topic proportions #############################

### Fed minutes
# Calculate log value for each topic proportion
for (i in 1:k){
  command <- paste0("meeting.df$lT", i, " <- log(meeting.df$T",i ,")")
  eval(parse(text=command))
}

topicnames <- paste0("T", 1:k)
l_topicnames <- paste0("lT", 1:k)
Tsums <- rowMeans(meeting.df[,topicnames])
summary(Tsums)
lTsums <- rowMeans(meeting.df[,l_topicnames])
summary(lTsums)

meeting.df$lTsum <- lTsums

# Calculate y_{d,k} = log(\theta_{d,k}) - 1/K \sum_m log(\theta_{d,m})
for (i in 1:k){
  command <- paste0("meeting.df$y", i, " <- meeting.df$lT",i ," - meeting.df$lTsum")
  eval(parse(text=command))
}


# Write the meeting topic distributions to file
clean_filename = paste(clean_dir, "CBC/fedmeeting_topics.csv", sep = "/")
write.csv(meeting.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# meeting.topics <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)



### Combined articles 
# Calculate log value for each topic proportion
for (i in 1:k){
  command <- paste0("combined_articles.topics$lT", i, " <- log(combined_articles.topics$T",i ,")")
  eval(parse(text=command))
}

topicnames <- paste0("T", 1:k)
l_topicnames <- paste0("lT", 1:k)
Tsums <- rowMeans(combined_articles.topics[,topicnames])
summary(Tsums)
lTsums <- rowMeans(combined_articles.topics[,l_topicnames])
summary(lTsums)

combined_articles.topics$lTsum <- lTsums

# Calculate y_{d,k} = log(\theta_{d,k}) - 1/K \sum_m log(\theta_{d,m})
for (i in 1:k){
  command <- paste0("combined_articles.topics$y", i, " <- combined_articles.topics$lT",i ," - combined_articles.topics$lTsum")
  eval(parse(text=command))
}

clean_filename = paste(clean_dir, "CBC/combinedarticle_topics.csv", sep = "/")
write.csv(combined_articles.topics, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# combined_articles.topics <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)




### Individual articles 
# Calculate log value for each topic proportion
for (i in 1:k){
  command <- paste0("articlelevel.df$lT", i, " <- log(articlelevel.df$T",i ,")")
  eval(parse(text=command))
}

topicnames <- paste0("T", 1:k)
l_topicnames <- paste0("lT", 1:k)
Tsums <- rowMeans(articlelevel.df[,topicnames])
summary(Tsums)
lTsums <- rowMeans(articlelevel.df[,l_topicnames])
summary(lTsums)

articlelevel.df$lTsum <- lTsums

# Calculate y_{d,k} = log(\theta_{d,k}) - 1/K \sum_m log(\theta_{d,m})
for (i in 1:k){
  command <- paste0("articlelevel.df$y", i, " <- articlelevel.df$lT",i ," - articlelevel.df$lTsum")
  eval(parse(text=command))
}

clean_filename = paste(clean_dir, "CBC/NYT_relevant_articleleveltopics.csv", sep = "/")
write.csv(articlelevel.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# articlelevel.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)


articlelevel.short <- subset(articlelevel.df, select=-c(article_id, subsequent_meeting, recent_meeting))
articlelevel.means <- articlelevel.short %>%
  group_by(meeting) %>%
  summarise_all(funs(mean))
articlelevel.means <- merge(articlelevel.means, article_key, by = c("meeting"), all.x = TRUE)

clean_filename = paste(clean_dir, "CBC/article_meantopics", sep = "/")
write.csv(articlelevel.means, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# articlelevel.means <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)


############################# Word clouds ############################# 

terms(paragraph_lda, 5)

# The beta contains the log transformation of the word probabilities
sum(paragraph_lda@beta[1,])
sum(exp(paragraph_lda@beta[1,]))

# opar <- par()  
for (ii in 1:k){
  # Filename for exported graphic
  file_name <- paste0(fig_dir, "clouds/topic", ii, "_cloud.png")
  print(paste0("Writing ", file_name))
  
  # Create a term distribution df
  topic.df <- data.frame(term = paragraph_lda@terms, p = exp(paragraph_lda@beta[ii,]))
  topic.df <- topic.df[order(-topic.df$p),]
  
  # Cut off only the top 50 words
  topic.df <- topic.df[1:50,]
  
  # Plot the wordclouds
  par(mar = rep(0, 4))
  png(file_name)
  wordcloud(words = topic.df$term,
            freq = topic.df$p,
            max.words = 50,
            random.order = FALSE,
            rot.per = 0.35,
            colors=brewer.pal(8, "Dark2"),
            scale=c(5,.5))
  dev.off()
}
#par(opar)



############################# Proportions for each corpus ############################# 

fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date)
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date)


meeting.key <- unique(fedminutes.df[,c("meeting_id", "pub_date", "meet_date")])


meetinglevel.means <- merge(meetinglevel.means, meeting.key, by = "meeting_id", all.x = TRUE)


# Separate data frame for each central bank (easier than using melt() for now as need to loop over topics)
pre_articles.df <- articlelevel.means[which(!is.na(articlelevel.means$subsequent_meeting)),]
pre_articles.df$meeting_id <- pre_articles.df$subsequent_meeting
pre_articles.df <- merge(pre_articles.df, meeting.key, by = "meeting_id")

post_articles.df <- articlelevel.means[which(!is.na(articlelevel.means$recent_meeting)),]
post_articles.df$meeting_id <- post_articles.df$recent_meeting
post_articles.df <- merge(post_articles.df, meeting.key, by = "meeting_id")

ggplot() + 
  scale_color_manual("Source",
                     values = c("Pre meeting" = "black", "Fed" = "blue3", "Post publication" = "darkgoldenrod2")) +
  geom_line(data = meetinglevel.means, aes(x = meet_date, y = T9, color = "Fed")) +
  geom_line(data = pre_articles.df, aes(x = meet_date, y = T9, color = "Pre meeting")) +
  geom_line(data = post_articles.df, aes(x = meet_date, y = T9, color = "Post publication")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("Topic 7 over time")

# Plot the topic proportions over time 
for (i in 1:k){
  file_name <- paste0(export_dir, "joint_LDA/topic", i, "_graph.png")
  print(paste0("Writing ", file_name))
  
  command <- paste0("
                    ggplot() + 
                    scale_color_manual(\"Source\", values = c(\"Pre meeting\" = \"black\", \"Fed\" = \"blue3\", \"Post publication\" = \"darkgoldenrod2\")) +
                    geom_line(data = meetinglevel.means, aes(x = meet_date, y = T",i,", color = \"Fed\")) +
                    geom_line(data = pre_articles.df, aes(x = meet_date, y = T",i,", color = \"Pre meeting\")) +
                    geom_line(data = post_articles.df, aes(x = meet_date, y = T",i,", color = \"Post publication\")) +
                    xlab(\"Meeting date\") +
                    ylab(expression(theta[bt])) + 
                    ggtitle(\"Topic ", i, " over time\") +
                    ggsave(\"", file_name, "\")")
  eval(parse(text=command))
}



############################# Tex files for figures ############################# 

texoutput <- vector()
beginfigure <- "\\begin{figure}[H]\n\t\\centering\n\t\\caption{LDA Topic "
begincontentsub <- " (transformed)} \n \t \\begin{subfigure}{.45\\textwidth} 
\t\t\\centering 
\t\t\\includegraphics[width=1.1\\textwidth]{figures/joint_LDA/topic"
endcontentsub <- "_cloud} \n\t\\end{subfigure} \n"
begintimesub <- "\t\\begin{subfigure}{.45\\textwidth}
\t\t\\centering\n\t\t
\t\t\\includegraphics[width=1\\textwidth]{figures/joint_LDA/topic"
endfigure <- "_graph}\n\t\\end{subfigure}\n\\end{figure}"

# Loop over each topic and combine strings to create the .tex code
for (i in 1:k){
  x <- paste(beginfigure, i, begincontentsub, i, endcontentsub, 
             begintimesub, i, endfigure, sep = "")
  texoutput[i] <- x
}
# loop to write each topic to a .tex
for (h in 1:k){
  filename_specific <- paste(export_dir, "joint_LDA/topic",h,"_fig.tex",sep="")
  write(texoutput[h], filename_specific)
}

filename_all <- paste(export_dir, "joint_LDA/all_fig.tex",sep="")
write(paste(texoutput, collapse = "\n\n"), filename_all)












############################# End ############################# 