setwd("~/Documents/GitHub/CBC_spillovers")
rm(list=ls())

require(tm)
require(stringr)
require(tidytext)
require(tidyr)
require(slam)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"



############################# Import and extract vocab of each corpus ############################# 

### Import the Federal Reserve data
fedminutes.df <- read.csv(paste0(clean_dir, "CBC/fedminutes_clean.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
# The removePunctuation function seems to be unreliable, so use gsub to remove some problems first
fedminutes.df$paragraph <- gsub("-", " ", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("/'", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("’", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("‘", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("“", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("”", "", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub("€", " ", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub(",", " ", fedminutes.df$paragraph)
fedminutes.df$paragraph <- gsub(":", " ", fedminutes.df$paragraph)

fedminutes.corpus <- Corpus(VectorSource(unlist(fedminutes.df[, "paragraph"])))
# Preliminary cleaning
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, stripWhitespace)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, removeNumbers)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, removePunctuation)
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, content_transformer(tolower))
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <- tm_map(fedminutes.corpus, removeWords, stopwords("english"))
inspect(fedminutes.corpus[[250]])
fedminutes.corpus <-  tm_map(fedminutes.corpus, stemDocument)
inspect(fedminutes.corpus[[250]])
#fedminutes.corpus.tokenized <-  lapply(fedminutes.corpus, scan_tokenizer)
#fedminutes.corpus.tokenized[[4713]]
#fedminutes.corpus.tokenized.stemcompleted <- lapply(fedminutes.corpus.tokenized, stemCompletion, dictCorpus)


# The convert each corpus to a DTM in order to extract the complete vocab
fedminutes.dtm <- DocumentTermMatrix(fedminutes.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of fedminutes.dtm are", dim(fedminutes.dtm)[1], "documents and", 
            dim(fedminutes.dtm)[2], "words in vocab"))
fedminutes.vocab <- fedminutes.dtm$dimnames$Terms

# Total wordcount
fedminutes.df$wordcount <- rowSums(as.matrix(fedminutes.dtm))
summary(fedminutes.df$wordcount)
sum(fedminutes.df$wordcount)
length(unique(fedminutes.df$meeting_id))


### Import the Bank of England data

bankminutes.df <- read.csv(paste0(clean_dir, "CBC/bankminutes_clean.csv"), encoding = "utf-8", stringsAsFactors = FALSE)

# Toggle the "exclude annex" option
exclude_annex <- TRUE
if (exclude_annex){
  bankminutes.df <- bankminutes.df[which(bankminutes.df$annex == 0),]
}

bankminutes.df$paragraph <- gsub("-", " ", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub("/'", "", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub("’", "", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub("‘", "", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub("“", "", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub("”", "", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub("€", " ", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub(",", " ", bankminutes.df$paragraph)
bankminutes.df$paragraph <- gsub(":", " ", bankminutes.df$paragraph)

bankminutes.corpus <- Corpus(VectorSource(unlist(bankminutes.df[, "paragraph"])))
# Preliminary cleaning
bankminutes.corpus <- tm_map(bankminutes.corpus, stripWhitespace)
bankminutes.corpus <- tm_map(bankminutes.corpus, removeNumbers)
bankminutes.corpus <- tm_map(bankminutes.corpus, removePunctuation)
bankminutes.corpus <- tm_map(bankminutes.corpus, content_transformer(tolower))
bankminutes.corpus <- tm_map(bankminutes.corpus, removeWords, stopwords("english"))
bankminutes.corpus <- tm_map(bankminutes.corpus, stemDocument)

# The convert each corpus to a DTM in order to extract the complete vocab
bankminutes.dtm <- DocumentTermMatrix(bankminutes.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of bankminutes.dtm are", dim(bankminutes.dtm)[1], "documents and", 
            dim(bankminutes.dtm)[2], "words in vocab"))
bankminutes.vocab <- bankminutes.dtm$dimnames$Terms

# Total wordcount
bankminutes.df$wordcount <- rowSums(as.matrix(bankminutes.dtm))
summary(bankminutes.df$wordcount)
sum(bankminutes.df$wordcount)
length(unique(bankminutes.df$meeting_id))



### Import the ECB data
ecbstatements.df <- read.csv(paste0(clean_dir, "CBC/ecbstatements_clean.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
ecbstatements.df$paragraph <- gsub("-", " ", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub("/'", "", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub("’", "", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub("‘", "", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub("“", "", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub("”", "", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub("€", " ", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub(",", " ", ecbstatements.df$paragraph)
ecbstatements.df$paragraph <- gsub(":", " ", ecbstatements.df$paragraph)

ecbstatements.corpus <- Corpus(VectorSource(unlist(ecbstatements.df[, "paragraph"])))
# Preliminary cleaning
ecbstatements.corpus <- tm_map(ecbstatements.corpus, stripWhitespace)
ecbstatements.corpus <- tm_map(ecbstatements.corpus, removeNumbers)
ecbstatements.corpus <- tm_map(ecbstatements.corpus, removePunctuation)
ecbstatements.corpus <- tm_map(ecbstatements.corpus, content_transformer(tolower))
ecbstatements.corpus <- tm_map(ecbstatements.corpus, removeWords, stopwords("english"))
ecbstatements.corpus <- tm_map(ecbstatements.corpus, stemDocument)

# The convert each corpus to a DTM in order to extract the complete vocab
ecbstatements.dtm <- DocumentTermMatrix(ecbstatements.corpus, control = list(minWordLength = 3))
print(paste("Dimensions of ecbstatements.dtm are", dim(ecbstatements.dtm)[1], "documents and", 
            dim(ecbstatements.dtm)[2], "words in vocab"))
ecbstatements.vocab <- ecbstatements.dtm$dimnames$Terms

# Total wordcount
ecbstatements.df$wordcount <- rowSums(as.matrix(ecbstatements.dtm))
summary(ecbstatements.df$wordcount)
sum(ecbstatements.df$wordcount)
length(unique(ecbstatements.df$meeting_id))



#############################  Identify terms that only appear in one corpus ############################# 

# First a little example to demonstrate the outersect methodology
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
a1 <- c("a","b","c","d","e")
a2 <- c("a","b","c","f","g")
a3 <- c("a","b","c","g","h")

# So all elements that appear in at least two lists should be given by 
atleasttwo <- union(union(intersect(a1,a2), intersect(a1,a3)), intersect(a2,a3))
print(atleasttwo)
# And thus elements that do not appear in at least two should be given by 
total <- union(union(a1,a2),a3)
print(total)
outersect(total, atleasttwo)


# Now the same with the vocabularies
fedbank.vocab <- intersect(fedminutes.vocab, bankminutes.vocab)
fedecb.vocab <- intersect(fedminutes.vocab, ecbstatements.vocab)
bankecb.vocab <- intersect(bankminutes.vocab, ecbstatements.vocab)
atleasttwo <- union(union(fedbank.vocab, fedecb.vocab), bankecb.vocab)

allterms <- union(union(fedminutes.vocab, bankminutes.vocab), ecbstatements.vocab)
onecorpusterms <- outersect(allterms, atleasttwo)







############################# Combine the corpora #############################  


# Now combine all three corpora into one
alldata.df <- rbind(fedminutes.df, bankminutes.df)
alldata.df <- rbind(alldata.df, ecbstatements.df)

alldata.corpus <- Corpus(VectorSource(unlist(alldata.df[, "paragraph"])))

# Label with some important metadata
meta(alldata.corpus, type = "indexed", tag = "unique_id") <- alldata.df$unique_id
meta(alldata.corpus, type = "indexed", tag = "central_bank") <- alldata.df$central_bank
meta(alldata.corpus, type = "indexed", tag = "meet_date") <- alldata.df$meet_date
meta(alldata.corpus, type = "indexed", tag = "pub_date") <- alldata.df$pub_date

# Preliminary cleaning 
alldata.corpus <- tm_map(alldata.corpus, stripWhitespace)
alldata.corpus <- tm_map(alldata.corpus, removeNumbers)
alldata.corpus <- tm_map(alldata.corpus, removePunctuation)
alldata.corpus <- tm_map(alldata.corpus, content_transformer(tolower))
alldata.corpus <- tm_map(alldata.corpus, removeWords, stopwords("english"))
alldata.corpus <- tm_map(alldata.corpus, stemDocument)

# Then, crucially, remove the terms which only appear in one corpus
alldata.corpus <- tm_map(alldata.corpus, removeWords, onecorpusterms)

# Remove months and seasons as they might introduce artificial co-movement
months <- c("januari", "februari", "march", "april", "may", "june", "july", "juli", "julyaugust", "august",
            "septemb", "octob", "novemb", "decemb")
alldata.corpus <- tm_map(alldata.corpus, removeWords, months)
seasons <- c("summer", "autumn", "spring", "winter")
alldata.corpus <- tm_map(alldata.corpus, removeWords, seasons)


alldata.df$paragraph_clean <- sapply(alldata.corpus, as.character)
alldata.df$paragraph_clean <- str_trim(alldata.df$paragraph_clean)
alldata.df[250, c("paragraph", "paragraph_clean")]

# Remove the empty documents
alldata.df <-  alldata.df[which(alldata.df$paragraph_clean !=""),]


### Write the clean data to csv
write.csv(alldata.df, file = paste0(clean_dir, "CBC/allbanksdata_clean.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)



# The convert each corpus to a DTM in order to extract the complete vocab
alldata.dtm <- DocumentTermMatrix(alldata.corpus, control = list(minsWordLength = 3))
print(paste("Dimensions of alldata.dtm are", dim(alldata.dtm)[1], "documents and", 
            dim(alldata.dtm)[2], "words in vocab"))
alldata.vocab <- alldata.dtm$dimnames$Terms     


############################# Some summary stats ############################# 

summary(col_sums(alldata.dtm))
findFreqTerms(alldata.dtm, 5000)

# The rows are documents and the columns are terms
summary(col_sums(alldata.dtm))
summary(row_sums(alldata.dtm))
N_d <- row_sums(alldata.dtm)
quantile(N_d, c(0.01, 0.5, 0.9999))

dim(alldata.dtm)




############################# End ############################# 