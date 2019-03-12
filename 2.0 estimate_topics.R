setwd("~/Documents/GitHub/CBC_spillovers")
rm(list=ls())

require(tm)
require(stringr)
require(tidytext)
require(tidyr)
require(slam)
require(topicmodels)
library(ggplot2)
library(dplyr)
require(wordcloud)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


### Import the Federal Reserve data
paragraph.df <- read.csv(paste0(clean_dir, "CBC/allbanksdata_clean.csv"), encoding = "utf-8", stringsAsFactors = FALSE)


############################# Convert to labelled DTM ############################# 

paragraph.corpus <- Corpus(VectorSource(unlist(paragraph.df[, "paragraph_clean"])))
paragraph.dtm <- DocumentTermMatrix(paragraph.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of paragraph.dtm are", dim(paragraph.dtm)[1], "documents and", 
            dim(paragraph.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
paragraph.dtm$dimnames$Docs <- paragraph.df$unique_id


# Calculate the tfidf score for each term
term_tfidf <-tapply(paragraph.dtm$v/row_sums(paragraph.dtm)[paragraph.dtm$i], paragraph.dtm$j, mean) *
  log2(nDocs(paragraph.dtm)/col_sums(paragraph.dtm > 0))
summary(term_tfidf)
quantile(term_tfidf, c(.025, .5, .975)) 

low_terms <- paragraph.dtm[,term_tfidf < 0.05]$dimnames$Terms
print(low_terms)

# Remove terms in the bottom 1% of the tf-idf ranking
paragraph.dtm <- paragraph.dtm[,term_tfidf >= 0.05]

# Remove all empty documents
paragraph.dtm <- paragraph.dtm[row_sums(paragraph.dtm) > 0,]

print(paste("After removing low tf-idf terms, the dimensions of paragraph.dtm are now", 
            dim(paragraph.dtm)[1], "documents and", dim(paragraph.dtm)[2], "words in vocab"))




############################# Estimate the topics on the paragraphs ############################# 


k <- 30
paragraph.lda <- LDA(paragraph.dtm, k = k, method = "Gibbs", 
                      control = list(verbose = 100, burnin = 1000, thin = 100, iter = 10000))
# paragraph.lda <- LDA(paragraph.dtm, k = k, control = list( verbose = 1))
paragraph.lda

### Store the topic beta vectors
paragraph.topics <- tidy(paragraph.lda, matrix = "beta")
paragraph.topics

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
meetinglevel.df <- paragraph.df %>%
  group_by(meeting_id) %>%
  summarise(text=paste(paragraph_clean,collapse=' '))

meeting.corpus <- Corpus(VectorSource(unlist(meetinglevel.df[, "text"])))
meeting.dtm <- DocumentTermMatrix(meeting.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of meeting.dtm are", dim(meeting.dtm)[1], "documents and", 
            dim(meeting.dtm)[2], "words in vocab"))

# Make sure each document is labelled with a unique identifier, in order to merge back later
meeting.dtm$dimnames$Docs <- meetinglevel.df$meeting_id





############################# Estimate paragraph-topic props over documents ############################# 

# Calculate the topic distribution of the documents, given the term distributions estimated on the paragraphs
meeting.posterior <- posterior(paragraph.lda, newdata = meeting.dtm)

meeting.topics <- as.data.frame(meeting.posterior$topic)
colnames(meeting.topics) <- paste0("T", colnames(meeting.posterior$topics))
meeting.topics$meeting_id <- rownames(meeting.topics)

# Create a data-frame with the key info for each meeting
meeting.df <- unique(paragraph.df[, c("meeting_id", "year", "month", "central_bank", "pub_date", "meet_date")])
meeting.df <- merge(meeting.df, meeting.topics, by = "meeting_id", all.x = TRUE)




### Check that these are at least similar to the mean proportion across paragraphs for each document
paragraph.posterior <- posterior(paragraph.lda)
paragraph.topics <- as.data.frame(paragraph.posterior$topics)
colnames(paragraph.topics) <- paste0("T", colnames(paragraph.posterior$topics))
paragraph.topics$unique_id <- rownames(paragraph.topics)

paragraph.topics <- merge(paragraph.topics, paragraph.df, by = "unique_id", all.x = TRUE)
paragraph.topics <- subset(paragraph.topics, select=-c(unique_id, paragraph_clean))

meetinglevel.means <- paragraph.topics %>%
  group_by(meeting_id) %>%
  summarise_all(funs(mean))

cor.test(meeting.topics$T1, meetinglevel.means$T1)
cor.test(meeting.topics$T3, meetinglevel.means$T3)
cor.test(meeting.topics$T4, meetinglevel.means$T4)
cor.test(meeting.topics$T7, meetinglevel.means$T7)
cor.test(meeting.topics$T10, meetinglevel.means$T10)



############################# Transformed topic proportions ############################# 

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



### Write the clean data to csv
write.csv(meeting.df, file = paste0(clean_dir, "CBC/meetingtopicprops.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)


############################# Word clouds ############################# 

terms(paragraph.lda, 5)

# The beta contains the log transformation of the word probabilities
sum(paragraph.lda@beta[1,])
sum(exp(paragraph.lda@beta[1,]))

# opar <- par()  
for (i in 1:k){
  # Filename for exported graphic
  file_name <- paste0(export_dir, "LDA/topic", i, "_cloud.png")
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

# Separate data frame for each central bank (easier than using melt() for now as need to loop over topics)
bank.df <- meeting.df[which(meeting.df$central_bank == "Bank of England"),]
rownames(bank.df) <- bank.df$meeting_id
fed.df <- meeting.df[which(meeting.df$central_bank == "Federal Reserve"),]
rownames(fed.df) <- fed.df$meeting_id
ecb.df <- meeting.df[which(meeting.df$central_bank == "European Central Bank"),]
rownames(ecb.df) <- ecb.df$meeting_id

ggplot() + 
  scale_color_manual("Central Bank",
                     values = c("BoE" = "black", "Fed" = "blue3", "ECB" = "darkgoldenrod2")) +
  geom_line(data = bank.df, aes(x = meet_date, y = T2, color = "BoE")) +
            geom_line(data = fed.df, aes(x = meet_date, y = T2, color = "Fed")) +
            geom_line(data = ecb.df, aes(x = meet_date, y = T2, color = "ECB")) +
            xlab('Meeting date') +
            ylab(expression(theta[bt])) + 
            ggtitle("Topic 2 over time")

# Plot the topic proportions over time 
for (i in 1:k){
  file_name <- paste0(export_dir, "LDA/topic", i, "_graph.png")
  print(paste0("Writing ", file_name))
  
  command <- paste0("
  ggplot() + 
    scale_color_manual(\"Central Bank\", values = c(\"BoE\" = \"black\", \"Fed\" = \"blue3\", \"ECB\" = \"darkgoldenrod2\")) +
    geom_line(data = bank.df, aes(x = meet_date, y = T",i,", color = \"BoE\")) +
    geom_line(data = fed.df, aes(x = meet_date, y = T",i,", color = \"Fed\")) +
    geom_line(data = ecb.df, aes(x = meet_date, y = T",i,", color = \"ECB\")) +
    xlab(\"Meeting date\") +
    ylab(expression(theta[bt])) + 
    ggtitle(\"Topic ", i, " over time\") +
    ggsave(\"", file_name, "\")")
  eval(parse(text=command))
}

# Plot the transformed topic variable over time 
for (i in 1:k){
  file_name <- paste0(export_dir, "LDA/topic", i, "_transformed_graph.png")
  print(paste0("Writing ", file_name))
  
  command <- paste0("
  ggplot() + 
    scale_color_manual(\"Central Bank\", values = c(\"BoE\" = \"black\", \"Fed\" = \"blue3\", \"ECB\" = \"darkgoldenrod2\")) +
    geom_line(data = bank.df, aes(x = meet_date, y = y",i,", color = \"BoE\")) +
    geom_line(data = fed.df, aes(x = meet_date, y = y",i,", color = \"Fed\")) +
    geom_line(data = ecb.df, aes(x = meet_date, y = y",i,", color = \"ECB\")) +
    xlab('Meeting date') +
    ylab(expression(y[bt])) + 
    ggtitle(\"Topic ", i, " (transformed) over time\")
    ggsave(\"", file_name, "\")")
  eval(parse(text=command))
}




############################# Tex files for figures ############################# 

texoutput <- vector()
beginfigure <- "\\begin{figure}[H]\n\t\\centering\n\t\\caption{LDA Topic "
begincontentsub <- " (transformed)} \n \t \\begin{subfigure}{.45\\textwidth} 
\t\t\\centering 
\t\t\\includegraphics[width=1.1\\textwidth]{figures/LDA/topic"
endcontentsub <- "_cloud} \n\t\\end{subfigure} \n"
begintimesub <- "\t\\begin{subfigure}{.45\\textwidth}
\t\t\\centering\n\t\t
\t\t\\includegraphics[width=1\\textwidth]{figures/LDA/topic"
endfigure <- "_transformed_graph}\n\t\\end{subfigure}\n\\end{figure}"
  
# Loop over each topic and combine strings to create the .tex code
for (i in 1:k){
  x <- paste(beginfigure, i, begincontentsub, i, endcontentsub, 
             begintimesub, i, endfigure, sep = "")
  texoutput[i] <- x
}
# loop to write each topic to a .tex
for (h in 1:k){
  filename_specific <- paste(export_dir, "LDA/topic",h,"_fig.tex",sep="")
  write(texoutput[h], filename_specific)
}
  
filename_all <- paste(export_dir, "LDA/all_fig.tex",sep="")
write(paste(texoutput, collapse = "\n\n"), filename_all)





############################# End ############################# 