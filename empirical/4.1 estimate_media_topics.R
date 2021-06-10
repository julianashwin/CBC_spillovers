####
# This file estimates topics on the Federal Reserve minutes and then queries them on the minutes and the NYT articles
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(tm)
require(slam)
require(topicmodels)
require(tidyverse)
require(tidytext)

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

articles_key <- unique(nyt_relevant[,c("article_id","meeting", "subsequent_meeting", "recent_meeting")])






############################# Convert to labelled DTM ############################# 

fed.corpus <- Corpus(VectorSource(unlist(fedminutes.df[, "paragraph_clean"])))
paragraph.dtm <- DocumentTermMatrix(fed.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of paragraph.dtm are", dim(paragraph.dtm)[1], "documents and", 
            dim(paragraph.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
paragraph.dtm$dimnames$Docs <- fedminutes.df$unique_id


# Calculate the tfidf score for each term
term_tfidf <-tapply(paragraph.dtm$v/row_sums(paragraph.dtm)[paragraph.dtm$i], paragraph.dtm$j, mean) *
  log2(nDocs(paragraph.dtm)/col_sums(paragraph.dtm > 0))
summary(term_tfidf)
quantile(term_tfidf, c(.01, .5, .99)) 

low_terms <- paragraph.dtm[,term_tfidf < 0.03579]$dimnames$Terms
print(low_terms)

# Remove terms in the bottom 1% of the tf-idf ranking
paragraph.dtm <- paragraph.dtm[,term_tfidf >= 0.03579]

# Remove all empty documents
paragraph.dtm <- paragraph.dtm[row_sums(paragraph.dtm) > 0,]
print(paste("After removing low tf-idf terms, the dimensions of paragraph.dtm are now", 
            dim(paragraph.dtm)[1], "documents and", dim(paragraph.dtm)[2], "words in vocab"))


article.corpus <- Corpus(VectorSource(unlist(nyt_relevant[, "text_clean"])))
article.corpus <- tm_map(article.corpus, removeWords, low_terms)
article.dtm <- DocumentTermMatrix(article.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of article.dtm are", dim(article.dtm)[1], "documents and", 
            dim(article.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
article.dtm$dimnames$Docs <- nyt_relevant$article_id




############################# Estimate the topics on the paragraphs ############################# 

k <- 30
paragraph.lda <- LDA(paragraph.dtm, k = k, method = "Gibbs", 
                     control = list(verbose = 100, burnin = 10000, thin = 100, iter = 200000))
# paragraph.lda <- LDA(paragraph.dtm, k = k, control = list( verbose = 1))
paragraph.lda

### Store the topic beta vectors
paragraph.topics <- tidy(paragraph.lda, matrix = "beta")
paragraph.topics


# Write the topic vectors to file
clean_filename = paste(clean_dir, "CBC/Fed_paragraph_topics.csv", sep = "/")
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
paragraph.assignments <- augment(paragraph.lda, data = paragraph.dtm)



############################# Combine the paragraphs into documents ############################# 

# Collapse paragraphs into whole meeting documents
meetinglevel.df <- fedminutes.df %>%
  group_by(meeting_id) %>%
  summarise(text=paste(paragraph_clean,collapse=' '))

meeting.corpus <- Corpus(VectorSource(unlist(meetinglevel.df[, "text"])))
meeting.dtm <- DocumentTermMatrix(meeting.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of meeting.dtm are", dim(meeting.dtm)[1], "documents and", 
            dim(meeting.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later
meeting.dtm$dimnames$Docs <- meetinglevel.df$meeting_id



############################# Estimate paragraph-topic props at Fed meeting level ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
meeting.posterior <- posterior(paragraph.lda, newdata = meeting.dtm)

meeting.topics <- as.data.frame(meeting.posterior$topic)
colnames(meeting.topics) <- paste0("T", colnames(meeting.posterior$topics))
meeting.topics$meeting_id <- rownames(meeting.topics)

# Create a data-frame with the key info for each meeting
meeting.df <- unique(fedminutes.df[, c("meeting_id", "pub_date", "meet_date")])
meeting.df <- merge(meeting.df, meeting.topics, by = "meeting_id", all.x = TRUE)



### Check that these are at least similar to the mean proportion across paragraphs for each document
paragraph.posterior <- posterior(paragraph.lda)
paragraph.topics <- as.data.frame(paragraph.posterior$topics)
colnames(paragraph.topics) <- paste0("T", colnames(paragraph.posterior$topics))
paragraph.topics$unique_id <- rownames(paragraph.topics)

paragraph.topics <- merge(paragraph.topics, fedminutes.df[c("unique_id", "meeting_id")], by = "unique_id", all.x = TRUE)
paragraph.topics <- subset(paragraph.topics, select=-c(unique_id))

meetinglevel.means <- paragraph.topics %>%
  group_by(meeting_id) %>%
  summarise_all(funs(mean))

cor.test(meeting.topics$T1, meetinglevel.means$T1)
cor.test(meeting.topics$T3, meetinglevel.means$T3)
cor.test(meeting.topics$T4, meetinglevel.means$T4)
cor.test(meeting.topics$T8, meetinglevel.means$T8)
cor.test(meeting.topics$T10, meetinglevel.means$T10)



meeting.key <- unique(fedminutes.df[,c("meeting_id", "pub_date", "meet_date")])
meeting.df <- merge(meeting.topics, meeting.key, by = "meeting_id")
meeting.df <- meeting.df[order(meeting.df$meet_date),]






############################# Estimate minutes-topics over individual media articles ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
article.posterior <- posterior(paragraph.lda, newdata = article.dtm)

# Extract the article topics with unique_id
article.topics <- as.data.frame(article.posterior$topic)
colnames(article.topics) <- paste0("T", colnames(article.posterior$topics))
article.topics$article_id <- rownames(article.topics)

# Create a data-frame with the key info for each meeting
articlelevel.df <- merge(articles_key, article.topics, by = "article_id", all.x = TRUE)

article_key <- unique(articlelevel.df[,c("meeting", "subsequent_meeting", "recent_meeting")])

articlelevel.short <- subset(articlelevel.df, select=-c(article_id, subsequent_meeting, recent_meeting))
articlelevel.means <- articlelevel.short %>%
  group_by(meeting) %>%
  summarise_all(funs(mean))
articlelevel.means <- merge(articlelevel.means, article_key, by = c("meeting"), all.x = TRUE)




############################# Combine the articles into pre and post documents ############################# 

articles_combined <- nyt_relevant %>%
  group_by(meeting) %>%
  summarise(text=paste(text_clean,collapse=' '))

articles_combined.corpus <- Corpus(VectorSource(unlist(articles_combined[, "text"])))
articles_combined.dtm <- DocumentTermMatrix(articles_combined.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of articles_combined.dtm are", dim(articles_combined.dtm)[1], "documents and", 
            dim(articles_combined.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later
articles_combined.dtm$dimnames$Docs <- articles_combined$meeting




############################# Estimate minutes-topics over combined media articles ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
combined_articles.posterior <- posterior(paragraph.lda, newdata = articles_combined.dtm)

combined_articles.topics <- as.data.frame(combined_articles.posterior$topic)
colnames(combined_articles.topics) <- paste0("T", colnames(combined_articles.posterior$topics))
combined_articles.topics$meeting <- rownames(combined_articles.topics)

# Create a data-frame with the key info for each meeting
combined_articles.topics <- merge(combined_articles.topics, 
                                  unique(articles_key[,c("meeting", "subsequent_meeting", "recent_meeting")]), 
                                  by = "meeting", all.x = TRUE)




### Check that these are at least similar to the mean proportion across paragraphs for each document

combined_articles.topics <- combined_articles.topics[order(combined_articles.topics$meeting),]
articlelevel.means <- articlelevel.means[order(articlelevel.means$meeting),]

cor.test(combined_articles.topics$T1, articlelevel.means$T1)
cor.test(combined_articles.topics$T3, articlelevel.means$T3)
cor.test(combined_articles.topics$T4, articlelevel.means$T4)
cor.test(combined_articles.topics$T7, articlelevel.means$T7)
cor.test(combined_articles.topics$T10, articlelevel.means$T10)




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
  file_name <- paste0(export_dir, "media_LDA/topic", i, "_cloud.png")
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

meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df$pub_date <- as.Date(meeting.df$pub_date)

meeting.key <- unique(meeting.df[,c("meeting_id", "pub_date", "meet_date")])

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
  geom_line(data = meeting.df, aes(x = meet_date, y = T9, color = "Fed")) +
  geom_line(data = pre_articles.df, aes(x = meet_date, y = T9, color = "Pre meeting")) +
  geom_line(data = post_articles.df, aes(x = meet_date, y = T9, color = "Post publication")) +
  xlab('Meeting date') +
  ylab(expression(theta[bt])) + 
  ggtitle("Topic 7 over time")

# Plot the topic proportions over time 
for (i in 1:k){
  file_name <- paste0(export_dir, "media_LDA/topic", i, "_graph.png")
  print(paste0("Writing ", file_name))
  
  command <- paste0("
                    ggplot() + 
                    scale_color_manual(\"Source\", values = c(\"Pre meeting\" = \"black\", \"Fed\" = \"blue3\", \"Post publication\" = \"darkgoldenrod2\")) +
                    geom_line(data = meeting.df, aes(x = meet_date, y = T",i,", color = \"Fed\")) +
                    geom_line(data = pre_articles.df, aes(x = meet_date, y = T",i,", color = \"Pre meeting\")) +
                    geom_line(data = post_articles.df, aes(x = meet_date, y = T",i,", color = \"Post publication\")) +
                    xlab(\"Meeting date\") +
                    ylab(expression(theta[bt])) + 
                    ggtitle(\"Topic ", i, " over time\") +
                    ggsave(\"", file_name, "\")")
  eval(parse(text=command))
}

# Plot the transformed topic variable over time 
for (i in 1:k){
  file_name <- paste0(export_dir, "media_LDA/topic", i, "_transformed_graph.png")
  print(paste0("Writing ", file_name))
  
  command <- paste0("
                    ggplot() + 
                    scale_color_manual(\"Source\", values = c(\"Pre meeting\" = \"black\", \"Fed\" = \"blue3\", \"Post publication\" = \"darkgoldenrod2\")) +
                    geom_line(data = meeting.df, aes(x = meet_date, y = y",i,", color = \"Fed\")) +
                    geom_line(data = pre_articles.df, aes(x = meet_date, y = y",i,", color = \"Pre meeting\")) +
                    geom_line(data = post_articles.df, aes(x = meet_date, y = y",i,", color = \"Post publication\")) +
                    xlab(\"Meeting date\") +
                    ylab(expression(theta[bt])) + 
                    ggtitle(\"Topic ", i, " (transformed) over time\") +
                    ggsave(\"", file_name, "\")")
  eval(parse(text=command))
}


############################# Tex files for figures ############################# 

texoutput <- vector()
beginfigure <- "\\begin{figure}[H]\n\t\\centering\n\t\\caption{LDA Topic "
begincontentsub <- " (transformed)} \n \t \\begin{subfigure}{.45\\textwidth} 
\t\t\\centering 
\t\t\\includegraphics[width=1.1\\textwidth]{figures/media_LDA/topic"
endcontentsub <- "_cloud} \n\t\\end{subfigure} \n"
begintimesub <- "\t\\begin{subfigure}{.45\\textwidth}
\t\t\\centering\n\t\t
\t\t\\includegraphics[width=1\\textwidth]{figures/media_LDA/topic"
endfigure <- "_transformed_graph}\n\t\\end{subfigure}\n\\end{figure}"

# Loop over each topic and combine strings to create the .tex code
for (i in 1:k){
  x <- paste(beginfigure, i, begincontentsub, i, endcontentsub, 
             begintimesub, i, endfigure, sep = "")
  texoutput[i] <- x
}
# loop to write each topic to a .tex
for (h in 1:k){
  filename_specific <- paste(export_dir, "media_LDA/topic",h,"_fig.tex",sep="")
  write(texoutput[h], filename_specific)
}

filename_all <- paste(export_dir, "media_LDA/all_fig.tex",sep="")
write(paste(texoutput, collapse = "\n\n"), filename_all)












############################# End ############################# 