####
# This file imports and cleans FOMC minutes from txt files
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(readtext)
require(utils)
require(stringr)
require(tm)
require(slam)
require(SentimentAnalysis)
require(lubridate)
require(ggplot2)
require(htm2txt)
require(tidyverse)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/clean_text/"
raw_dir <- "data/raw_text/"


meeting_dates <- read.csv(paste0(raw_dir, "Fed_meeting_dates.csv"),
                          stringsAsFactors = FALSE, encoding = "utf-8")

############### Minutes ############### 

import_filename <- paste0(raw_dir, "fedstatements_pre2014.txt")
fedstatements_all <- readtext(import_filename, encoding = "utf-8")
#fedminutes_all <- read.table(import_filename, sep="\t", header=TRUE, encoding = "utf-8")

# Create dataframe for the paragraphs
variables <- c( "year", "month", "document", "paragraph","seq")
fedstatements.df <- data.frame(matrix(NA,0,length(variables)))
colnames(fedstatements.df) <- variables

para.locs <- as.data.frame(str_locate_all(fedstatements_all, "[0-9]+\t"))
para.num <- length(para.locs[,1])

# Loop over the paragraphs
pb = txtProgressBar(min = 1, max = (para.num-1), initial = 1) 
for (i in 1:(para.num-1)){
  
  # Create empty dataframe to fill
  temp.df <- data.frame(matrix(NA,0,length(variables)))
  colnames(temp.df) <- variables
  
  #print(i)
  # Pull out an individual article
  full.para <- str_sub(fedstatements_all, para.locs[i,1], (para.locs[(i+1),1]-1))
  
  temp <- as.data.frame(str_locate(full.para, "\t"))
  
  docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
  text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
  
  #temp <- as.data.frame(str_locate_all(text, "\t"))
  #assert_that(nrow(temp) == 1)
  
  temp.df[1, "document"] <- as.character(docid)
  temp.df[1, "paragraph"] <- text
  
  # Append to dataframe for that file
  fedstatements.df <- rbind(fedstatements.df, temp.df)
  setTxtProgressBar(pb,i)
}
temp.df <- data.frame(matrix(NA,0,length(variables)))
colnames(temp.df) <- variables
full.para <- str_sub(fedstatements_all, para.locs[para.num,1])
temp <- as.data.frame(str_locate(full.para, "\t"))
docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
temp.df[1, "document"] <- as.character(docid)
temp.df[1, "paragraph"] <- text
fedstatements.df <- rbind(fedstatements.df, temp.df)


# Edit some paragraph names as meeting sometimes starts in a different month to when it finishes
fedstatements.df[which(fedstatements.df$document == "199807"), "document"] <- "199806"
fedstatements.df[which(fedstatements.df$document == "201208"), "document"] <- "201207"
fedstatements.df[which(fedstatements.df$document == "201305"), "document"] <- "201304"

# Extract date information
year <- as.array(str_sub(fedstatements.df$document, 1, 4))
month <- as.array(as.numeric(str_sub(fedstatements.df$document, 5,6)))

# Functions to clean the year and date terms from the fedminutes file
clean_month <- function(month_in){
  month_in <- as.numeric(month_in)
  if(month_in == 1){
    month_out <- "January"
  } else if(month_in == 2){
    month_out <- "February"
  } else if(month_in == 3){
    month_out <- "March"
  } else if(month_in == 4){
    month_out <- "April"
  } else if(month_in == 5){
    month_out <- "May"
  } else if(month_in == 6){
    month_out <- "June"
  } else if(month_in == 7){
    month_out <- "July"
  } else if(month_in == 8){
    month_out <- "August"
  } else if(month_in == 9){
    month_out <- "September"
  } else if(month_in == 10){
    month_out <- "October"
  } else if(month_in == 11){
    month_out <- "November"
  } else if(month_in == 12){
    month_out <- "December"
  }
  return(month_out)
}

month <- apply(month, 1, clean_month)

# Fill in the date values of dataframe
fedstatements.df$year <- as.numeric(year)
fedstatements.df$month <- month



fedstatement_paras_df <- tibble(left_join(fedstatements.df, meeting_dates, by = c("year", "month"))) %>%
  mutate(meet_date = as.Date(meet_date, format = "%d/%m/%Y")) %>%
  mutate(pub_date = as.Date(pub_date, format = "%d/%m/%Y")) %>%
  dplyr::select(meet_date, pub_date, document, paragraph)


########### Add post 2014 data ################


extra_files <- dir(paste0(raw_dir,"statements_2014onward/"))
extra_dates <- as.Date(str_remove_all(extra_files, ".html"), format = "%B %d, %Y")

statements_14onward_df <- data.frame(matrix(NA, nrow = length(extra_files), ncol = 4))
names(statements_14onward_df) <- c("date", "title", "text", "filename")


pb = txtProgressBar(min = 1, max = length(extra_files), initial = 1) 
for (ii in 1:length(extra_files)){
  
  docid <- extra_files[ii]
  import_filename <- paste0(raw_dir,"statements_2014onward/", docid)
  text <- gettxt(import_filename, encoding = "latin1")
  split_text <- str_split(text, 
                          "Please enable JavaScript if it is disabled in your browser or access the information through the links provided below")[[1]]
  if (length(split_text) != 2){
    print(paste("First problem with document", ii))
  }
  split_text <- str_split(split_text[2], 
                          "\n\n• Share\n\n•\n•\n•")[[1]]
  if (length(split_text) != 2){
    print(paste("Second problem with document", ii))
  }
  
  # The first part should be metadata and the second the actual speech
  text <- trimws(split_text[2])
  metadata <- str_split(trimws(split_text[1]), "\n\n")[[1]]
  metadata <- metadata[which(str_detect(metadata, "[a-z]"))]
  
  
  if (length(metadata) != 3){
    print(paste("Third problem with document", ii))
  }
  
  statements_14onward_df$filename[ii] <- docid
  statements_14onward_df$title[ii] <- str_squish(metadata[2])
  date <- str_split(str_squish(metadata[1]), " ")[[1]]
  day <- str_squish(removePunctuation(date[2]))
  month <- removePunctuation(date[1])
  year <- date[3]
  extracted_date <-  as.character(as.Date(paste(day,month,year,sep = "-"), format = "%d-%B-%Y"))
  
  if (extra_dates[ii] != extracted_date){
    print(paste("Fourth problem with document", ii))
  }

  statements_14onward_df$date[ii] <- extracted_date
  if (is.na(statements_14onward_df$date[ii])){
    print(paste("No date for document", ii))
  }
  # Remove any bullet point at start 
  text <- str_remove(text,"^•\n\n")
  
  # Cut of the bit at the end of the speech
  split_text <- str_split(text, "\n\nLast Update:")[[1]]
  if (length(split_text) != 2){
    print(paste("Fifth roblem with document", ii))
  }
  
  text <- split_text[1]
  
  statements_14onward_df$text[ii] <- text
  
  setTxtProgressBar(pb,ii)
}




### Separate into paragraphs
statments_new_para_df <- data.frame(matrix(NA, nrow = 0, ncol = 5))
names(statments_new_para_df) <- c("date", "document", "paragraph")

pb = txtProgressBar(min = 1, max = nrow(statements_14onward_df), initial = 1) 
for (ii in 1:nrow(statements_14onward_df)){
  
  row <- statements_14onward_df[ii,]
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








########### Merge together with meeting info and bit more cleaning ########### 



# Import the meeting date data
meeting_dates <- read.csv(paste0(raw_dir, "Fed_meeting_dates.csv"),
                          stringsAsFactors = FALSE, encoding = "utf-8")

fedminutes.df <- merge(fedminutes.df, meeting_dates, by = c("year", "month"), all.x = TRUE)

fedminutes.df <- fedminutes.df[which(fedminutes.df$paragraph != "?"),]
#View(fedminutes.df[which(is.na(fedminutes.df$meet_date)),])

# Order by meeting date
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date, format = "%d/%m/%Y")
fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date, format = "%d/%m/%Y")
fedminutes.df <- fedminutes.df[order(fedminutes.df$meet_date),]

fedminutes.df <- fedminutes.df[which(fedminutes.df$meet_date >= "1990-01-01"),]
table(is.na(fedminutes.df$pub_date))
table(is.na(fedminutes.df$meet_date))
table(is.na(fedminutes.df$paragraph))
length(unique(fedminutes.df$document))

summary(nchar(fedminutes.df$paragraph))
fedminutes.df$nchar <- nchar(fedminutes.df$paragraph)
fedminutes.df <- fedminutes.df[which(fedminutes.df$nchar > 10),]


#View(fedminutes.df[which((str_detect(fedminutes.df$paragraph, "Votes for"))),])
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Votes against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Vote against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Votes for"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Voting against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Voting for"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Absent and"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "^The vote encompassed approval"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "^The votes encompassed approval"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "^The vote also encompassed approval"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "voted as alternate member"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "voted as the alternate"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "voted as an alternate"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Return to top"))), "paragraph"] <- NA




# Remove all the deleted paragraphs from the dataframe
fedminutes.df <- fedminutes.df[which(!is.na(fedminutes.df$paragraph)),]


plot(table(fedminutes.df$meet_date))


# Add unique paragraph identifier
unique_id <- paste0("FEDp_", 1:nrow(fedminutes.df))
fedminutes.df$unique_id <- unique_id


# Add a unique meeting identifier
meeting.df <- unique(fedminutes.df[, c("year", "month", "pub_date", "meet_date")])
meeting.df <- meeting.df[order(meeting.df$meet_date),]
table(duplicated(meeting.df[,c("year", "month")]))
meeting.df$meeting_id <- paste0("FEDm_", 1:nrow(meeting.df))
fedminutes.df <- merge(fedminutes.df, meeting.df, by = c("year", "month", "pub_date", "meet_date"), all.x = TRUE)
fedminutes.df <- fedminutes.df[order(fedminutes.df$meet_date),]

fedminutes.df$quarter <- floor_date(as.Date(fedminutes.df$meet_date), "quarter")



## Add an LM sentiment measure (loop else vector memory is exhausted)
fedminutes.df$sentiment <- NA
pb = txtProgressBar(min = 1, max = nrow(fedminutes.df), initial = 1) 
for (ii in 1:nrow(fedminutes.df)){
  para <- fedminutes.df$paragraph[ii]
  sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
  fedminutes.df$sentiment[ii] <- sentiment[1,1]
  setTxtProgressBar(pb,ii)
}

fedminutes.df$sentiment[which(is.na(fedminutes.df$sentiment))] <- 0

sent_df <- aggregate(fedminutes.df[,c("sentiment")], FUN = mean, by = 
                       list(quarter = fedminutes.df$quarter))
sent_df$quarter <- as.Date(sent_df$quarter)
ggplot(sent_df) + theme_bw() + 
  geom_line(aes(x = quarter, y = x))





fedminutes.df <- fedminutes.df[, c("unique_id", "meeting_id", "year", "month", "quarter", "document", 
                                   "pub_date", "meet_date", "source", "paragraph", "sentiment")]


# Write the clean Federal Reserve minutes to a file
clean_filename = paste0(clean_dir, "fedminutes_all.csv")
write.csv(fedminutes.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)



############### End ############### 
