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
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


# Import the text data
clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

clean_filename = paste(clean_dir, "CBC/NYT_relevant_clean.csv", sep = "/")
nyt_relevant <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

############################# Unique identifier for each pre and post meeting period ############################# 

# Separate out pre/post meeting/publication articles an
pre_articles.df <- nyt_relevant[which(!is.na(nyt_relevant$subsequent_meeting)),]
post_articles.df <- nyt_relevant[which(!is.na(nyt_relevant$recent_meeting)),]

# edit each id to show whether this is in pre or post sample
pre_articles.df$meeting <- paste0(pre_articles.df$subsequent_meeting, "_pre")
pre_articles.df$article_id <- paste0(pre_articles.df$unique_id, "_pre")
pre_articles.df$recent_meeting <- NA
post_articles.df$meeting <- paste0(post_articles.df$recent_meeting, "_post")
post_articles.df$article_id <- paste0(post_articles.df$unique_id, "_pre")
post_articles.df$subsequent_meeting <- NA

nyt_relevant <- rbind(pre_articles.df, post_articles.df)
nyt_relevant <- nyt_relevant %>% arrange(meeting, article_id)


fedminutes.df$text_clean <- fedminutes.df$paragraph_clean
fedminutes.df$total_id <- fedminutes.df$unique_id
nyt_relevant$total_id <- nyt_relevant$article_id
total.df <- nyt_relevant[,c("total_id", "text_clean")]
total.df <- rbind(total.df, fedminutes.df[,c("total_id", "text_clean")])

articles_key <- unique(nyt_relevant[,c("total_id", "article_id","meeting", "subsequent_meeting", "recent_meeting")])





############################# Convert to labelled DTM ############################# 

total.corpus <- Corpus(VectorSource(unlist(total.df[, "text_clean"])))
total.dtm <- DocumentTermMatrix(total.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of total.dtm are", dim(total.dtm)[1], "documents and", 
            dim(total.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
total.dtm$dimnames$Docs <- total.df$total_id


# Calculate the tfidf score for each term
term_tfidf <-tapply(total.dtm$v/row_sums(total.dtm)[total.dtm$i], total.dtm$j, mean) *
  log2(nDocs(total.dtm)/col_sums(total.dtm > 0))
summary(term_tfidf)
quantile(term_tfidf, c(.01, .5, .99)) 

low_terms <- total.dtm[,term_tfidf < 0.01]$dimnames$Terms
print(low_terms)



######## Also remove the word "said" because it is fucking everywhere ######## 

# Remove terms in the bottom 1% of the tf-idf ranking
total.dtm <- total.dtm[,term_tfidf >= 0.01]

# Remove all empty documents
total.dtm <- total.dtm[row_sums(total.dtm) > 0,]
print(paste("After removing low tf-idf terms, the dimensions of total.dtm are now", 
            dim(total.dtm)[1], "documents and", dim(total.dtm)[2], "words in vocab"))





############################# Estimate the topics on the paragraphs and articles ############################# 

k <- 30
paragraph.lda <- LDA(total.dtm, k = k, method = "Gibbs", 
                     control = list(verbose = 100, burnin = 1000, thin = 100, iter = 20000))
# paragraph.lda <- LDA(paragraph.dtm, k = k, control = list( verbose = 1))
paragraph.lda

### Store the topic beta vectors
paragraph.topics <- tidy(paragraph.lda, matrix = "beta")
paragraph.topics


# Write the topic vectors to file
clean_filename = paste(clean_dir, "CBC/joint_paragraph_topics.csv", sep = "/")
write.csv(paragraph.topics, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
# nyt_relevant <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)



# Identify the top ten terms for each topic
top.terms <- paragraph.topics %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot these top ten terms for each topic
top.terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


### Store the topic proportion gamma/theta vectors
paragraph.gamma <- tidy(paragraph.lda, matrix = "gamma")
paragraph.gamma


### Store the topic assignment of each word 
paragraph.assignments <- augment(paragraph.lda, data = total.dtm)




############################# Separate out again ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
paragraph.posterior <- posterior(paragraph.lda)

# Extract the article topics with unique_id
individual.topics <- as.data.frame(paragraph.posterior$topic)
colnames(individual.topics) <- paste0("T", colnames(paragraph.posterior$topics))
individual.topics$total_id <- rownames(individual.topics)


### Split back into NYT and minutes 
meetingtopics <- individual.topics[which(str_detect(individual.topics$total_id, "FED")),]
articletopics <- individual.topics[which(str_detect(individual.topics$total_id, "nyt")),]






############################# Find the means of the meeting paragraphs ############################# 

meetingtopics <- merge(meetingtopics, fedminutes.df[c("total_id", "unique_id", "meeting_id")], by = "total_id", all.x = TRUE)
meetingtopics <- subset(meetingtopics, select=-c(unique_id, total_id))

meetinglevel.means <- meetingtopics %>%
  group_by(meeting_id) %>%
  summarise_all(funs(mean))


############################# Find the means of the weekly newspaper ############################# 

# Create a data-frame with the key info for each meeting
articletopics <- merge(articles_key, articletopics, by = "total_id", all.x = TRUE)

articlelevel.short <- subset(articletopics, select=-c(total_id, article_id, subsequent_meeting, recent_meeting))
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

terms(paragraph.lda, 5)

# The beta contains the log transformation of the word probabilities
sum(paragraph.lda@beta[1,])
sum(exp(paragraph.lda@beta[1,]))

# opar <- par()  
for (i in 1:k){
  # Filename for exported graphic
  file_name <- paste0(export_dir, "joint_LDA/topic", i, "_cloud.png")
  print(paste0("Writing ", file_name))
  
  # Create a term distribution df
  topic.df <- data.frame(term = paragraph.lda@terms, p = exp(paragraph.lda@beta[i,]))
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