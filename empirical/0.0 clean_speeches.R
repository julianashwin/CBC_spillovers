####
# This file imports and cleans FOMC speeches from html files
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(stringr)
require(tm)
require(slam)
require(htm2txt)
require(lubridate)



############### Speeches ############### 


### Define the directories where raw data is stored and clean will be saved
html_old_dir <- "data/raw_text/pre06_html/"

# Identify html files to read
html_old_files <- dir(html_old_dir)
# empty dataframe to fill
speeches_pre06_df <- data.frame(matrix(NA, nrow = length(html_old_files), ncol = 5))
names(speeches_pre06_df) <- c("date", "speaker", "title", "text", "filename")


pb = txtProgressBar(min = 1, max = length(html_old_files), initial = 1) 
for (ii in 1:length(html_old_files)){
  filename <- html_old_files[ii]
  text <- gettxt(paste0(html_old_dir, filename), encoding = "latin1")
  filename <- str_remove(filename, "Revisited")
  filename <- str_replace(filename, "Business-Cycle", "Business Cycle")
  filename <- str_replace(filename, "FRB_ Speech, Greenspan -- FRB_ Speech, Greenspan", "FRB_ Speech, Greenspan ")
  filename <- str_replace(filename, "education, April 3, 2003", "education -- April 3, 2003")
  filename <- str_replace(filename, "Janurary", "January")
  filename <- str_replace(filename, "Januray", "January")
  filename <- str_replace(filename, "Febraury", "February")
  filename <- str_replace(filename, "Nevember", "November")
  
  
  metadata <- str_split(filename, "----|---|--")[[1]]
  if (length(metadata) > 3){
    # If there are too many remove the shortest one
    print(paste("Removing", metadata[which(nchar(metadata) == min(nchar(metadata)))], 
                "from document", ii))
    metadata <- metadata[which(nchar(metadata) != min(nchar(metadata)))]
  }
  if (length(metadata) != 3){
    metadata <- str_split(filename, "—")[[1]]
    if (length(metadata) != 3){
      metadata <- str_split(filename, "- -")[[1]]
      if (length(metadata) != 3){
        metadata <- str_split(filename, "--|-|—|–")[[1]]
        if (length(metadata) != 3){
          print(ii)
        }
      }
    }
  }
  speeches_pre06_df$filename[ii] <- filename
  speeches_pre06_df$speaker[ii] <- str_squish(str_remove(metadata[1], 
                                                   "FRB speech_|FRB_ Speech,|FRB Speech,|FRB_Speech,|Speech,"))
  speeches_pre06_df$title[ii] <- str_squish(metadata[2])
  date <- str_split(str_squish(str_remove(metadata[3], ".html")), " ")[[1]]
  day <- str_squish(removePunctuation(date[2]))
  month <- removePunctuation(date[1])
  year <- date[3]
  
  # One date is specified incorrectly so fix here
  if (month == "November" & day == "31"){
    month <- "October"
  }
  
  speeches_pre06_df$date[ii] <- as.character(as.Date(paste(day,month,year,sep = "-"), format = "%d-%B-%Y"))
  if (is.na(speeches_pre06_df$date[ii])){
    print(paste("No date for document", ii))
  }
  
  
  
  split_text <- str_split(text, "\n\n----------\n\n")[[1]]
  if (length(split_text) < 2){
    print(paste("Problem with document", ii))
  }
  text <- split_text[2]
  
  speeches_pre06_df$text[ii] <- text
  
  setTxtProgressBar(pb,ii)
  
}

speeches_pre06_df <- speeches_pre06_df[order(speeches_pre06_df$date),]



## Clean the post-2005 speeches
html_new_dir <- "data/raw_text/06onward_html/"
# Identify html files to read
html_new_files <- dir(html_new_dir)
# empty dataframe to fill
speeches_06onward_df <- data.frame(matrix(NA, nrow = length(html_new_files), ncol = 5))
names(speeches_06onward_df) <- c("date", "speaker", "title", "text", "filename")


pb = txtProgressBar(min = 1, max = length(html_new_files), initial = 1) 
for (ii in 1:length(html_new_files)){
  filename <- html_new_files[ii]
  text <- gettxt(paste0(html_new_dir, filename), encoding = "latin1")
  
  split_text <- str_split(text, 
             "Please enable JavaScript if it is disabled in your browser or access the information through the links provided below")[[1]]
  if (length(split_text) != 2){
    print(paste("Problem with document", ii))
  }
  split_text <- str_split(split_text[2], 
                          "\n\n• Share\n\n•\n•\n•")[[1]]
  if (length(split_text) != 2){
    print(paste("Problem with document", ii))
  }
  
  # The first part should be metadata and the second the actual speech
  text <- trimws(split_text[2])
  metadata <- str_split(trimws(split_text[1]), "\n\n")[[1]]
  metadata <- metadata[which(str_detect(metadata, "[a-z]"))]
  
  
  if (length(metadata) != 4){
    print(paste("Problem with document", ii))
  }
   
  speeches_06onward_df$filename[ii] <- filename
  speeches_06onward_df$speaker[ii] <- str_squish(metadata[3])
  speeches_06onward_df$title[ii] <- str_squish(metadata[2])
  date <- str_split(str_squish(metadata[1]), " ")[[1]]
  day <- str_squish(removePunctuation(date[2]))
  month <- removePunctuation(date[1])
  year <- date[3]
  
  
  speeches_06onward_df$date[ii] <- as.character(as.Date(paste(day,month,year,sep = "-"), format = "%d-%B-%Y"))
  if (is.na(speeches_06onward_df$date[ii])){
    print(paste("No date for document", ii))
  }
  
  
  
  # Cut of the bit at the end of the speech
  split_text <- str_split(text, "\n\nLast Update:")[[1]]
  if (length(split_text) != 2){
    print(paste("Problem with document", ii))
  }
  
  split_text <- str_split(text, "\n\n----------\n\n")[[1]]
  if (length(split_text) < 2){
    text <- text
  } else {
    text <- split_text[1]
  }
  
  

  text <- split_text[1]
  
  speeches_06onward_df$text[ii] <- text
  
  setTxtProgressBar(pb,ii)
}





# Combine new and old

speeches_df <- rbind(speeches_pre06_df, speeches_06onward_df)
rownames(speeches_df) <- NULL


### Separate into paragraphs
speeches_para_df <- data.frame(matrix(NA, nrow = 0, ncol = 5))
names(speeches_para_df) <- c("unique_id", "date", "filename", "paragraph", "nchar")

pb = txtProgressBar(min = 1, max = nrow(speeches_df), initial = 1) 
for (ii in 1:nrow(speeches_df)){
  
  row <- speeches_df[ii,]
  paras <- str_split(row$text, "\n\n")[[1]]
  paras <- paras[which(nchar(paras) > 1)]
  N <- length(paras)
  
  speech_df <- data.frame(unique_id = NA, speech_id = NA, date = rep(row$date,N),
                          filename = rep(row$filename,N), paragraph = paras, nchar = NA)
  
  # Remove extra whitespace
  speech_df$paragraph <- str_squish(speech_df$paragraph)
  speech_df$nchar <- nchar(speech_df$paragraph)
  speech_df <- speech_df[which(nchar(speech_df$paragraph) > 1),]
  
  # Remove content below the "Return to top" point
  end_point <- which(str_detect(speech_df$paragraph, "Return to top|Return to text"  ))
  if (length(end_point) == 1 ){
    if (nrow(speech_df) < end_point){
      print(paste("Problem with", ii))
    } else {
      speech_df <- speech_df[which(1:nrow(speech_df) < end_point),]
    }
  }
  # remove any paragraphs that don't include alphabet characters
  speech_df <- speech_df[which(str_detect(speech_df$paragraph, "[a-z]") ),]
  # remove paragraphs with three or fewer words
  speech_df <- speech_df[which(str_count(speech_df$paragraph, " ") >= 4 ),]
  # Append
  speeches_para_df <- rbind(speeches_para_df, speech_df)
  
  setTxtProgressBar(pb,ii)
}

# Add a unique id per paragraph
speeches_para_df$unique_id <- paste0("SPEECHp_", 1:nrow(speeches_para_df))
# Add a unique id per speech
speeches_para_df$speech_id <- paste0("SPEECH_", as.numeric(as.factor(speeches_para_df$filename)))
# Add quarter
speeches_para_df$date <- as.Date(speeches_para_df$date)
speeches_para_df$quarter <- floor_date(speeches_para_df$date, "quarter")

## Add an LM sentiment measure (loop else vector memory is exhausted)
speeches_para_df$sentiment <- NA
pb = txtProgressBar(min = 1, max = nrow(speeches_para_df), initial = 1) 
for (ii in 1:nrow(speeches_para_df)){
  para <- speeches_para_df$paragraph[ii]
  sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
  speeches_para_df$sentiment[ii] <- sentiment[1,1]
  setTxtProgressBar(pb,ii)
}

speeches_para_df$sentiment[which(is.na(speeches_para_df$sentiment))] <- 0

sent_df <- aggregate(speeches_para_df[,c("sentiment")], FUN = mean, by = 
                       list(quarter = speeches_para_df$quarter))
sent_df$quarter <- as.Date(sent_df$quarter)
ggplot(sent_df) + theme_bw() + 
  geom_line(aes(x = quarter, y = x))



speeches_para_df <- speeches_para_df[order(speeches_para_df$date),
                                     c("unique_id","speech_id", "date", "quarter", "filename", 
                                       "paragraph", "nchar", "sentiment")]


write.csv(speeches_para_df, "data/clean_text/fedspeeches_all.csv", row.names = F)
#speeches_para_df <- read.csv( "data/clean_text/fedspeeches_all.csv", stringsAsFactors = F)



############### End ############### 


