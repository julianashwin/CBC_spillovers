setwd("~/Documents/GitHub/CBC_spillovers")
rm(list=ls())
require(readtext)
require(stringr)
require(assertthat)
require(dplyr)


clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"

# Toggle to run the part which combines all the separate ECB statement files
assemble_ecb_files <- FALSE

# Combine the ECB statement files into one.
if (assemble_ecb_files){
  all.files <- dir(paste0(raw_dir, "CBC/ECB_precleanup"))
  # Create variable to fill with all ECB statements
  ecb_text <- ""
  
  # Merge all of the files together so that they can be cleaned and spellchecked at once.
  for(f in all.files){
    print(paste("File", iter, "out of", length(all.files), ":", f))
    
    file <- readtext(paste0(raw_dir, "CBC/ECB_precleanup/", f))
    
    full_content <- (paste(file$doc_id, file$text, sep = "\t\t\n\n"))
    
    ecb_text <- (paste(ecb_text, full_content, sep = "\t\t\n\n"))
  }
  
  fileConn<-file(paste0(raw_dir, "CBC/ecb_statements.txt"))
  writeLines(ecb_text, fileConn)
  close(fileConn)
  
  # The spellchecked and clean file is now saved as ecbstatements_clean.txt
}





############################# Bank of England ############################# 


### Import the bank minutes text and convert to clean csv file

import_filename <- paste0(raw_dir, "CBC/boeminutes_clean.txt")
bankminutes_all <- readtext(import_filename, encoding = "utf-8")

# Create dataframe for the paragraphs
variables <- c( "year", "month", "document", "paragraph")
bankminutes.df <- data.frame(matrix(NA,0,length(variables)))
colnames(bankminutes.df) <- variables

para.locs <- as.data.frame(str_locate_all(bankminutes_all, "mpc[0-9]+.txt"))
para.num <- length(para.locs[,1])

# Loop over the paragraphs
for (i in 1:(para.num-1)){
  
  # Create empty dataframe to fill
  temp.df <- data.frame(matrix(NA,0,length(variables)))
  colnames(temp.df) <- variables
  
  #print(i)
  # Pull out an individual article
  full.para <- str_sub(bankminutes_all, para.locs[i,1], (para.locs[(i+1),1]-1))
  
  temp <- as.data.frame(str_locate_all(full.para, ".txt"))
  assert_that(nrow(temp) == 1)
  
  docid <- str_trim(str_sub(full.para, 1, (temp[,"end"])))
  text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
  
  temp.df[1, "document"] <- docid
  temp.df[1, "paragraph"] <- text
  
  # Append to dataframe for that file
  bankminutes.df <- rbind(bankminutes.df, temp.df)
}
temp.df <- data.frame(matrix(NA,0,length(variables)))
colnames(temp.df) <- variables
full.para <- str_sub(bankminutes_all, para.locs[para.num,1])
temp <- as.data.frame(str_locate_all(full.para, "\t"))
assert_that(nrow(temp) == 1)
docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
temp.df[1, "document"] <- docid
temp.df[1, "paragraph"] <- text
bankminutes.df <- rbind(bankminutes.df, temp.df)


# Extract date information
year <- as.array(str_sub(bankminutes.df$document, 4,5))
month <- as.array(as.numeric(str_sub(bankminutes.df$document, 6,7)))

# Functions to clean the year and date terms from the bankminutes file
clean_year <- function(year_in){
  if(str_sub(year_in, 1,1) == "9"){
    year_out <- paste0("19", year_in)
  } else{
    year_out <- paste0("20", year_in)
  }
  return(year_out)
}
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

year <- apply(year, 1, clean_year)
month <- apply(month, 1, clean_month)

# Fill in the date values of dataframe
bankminutes.df$year <- as.numeric(year)
bankminutes.df$month <- month
table(is.na(bankminutes.df$year))
table(is.na(bankminutes.df$month))

bankminutes.df$paragraph <- str_trim(bankminutes.df$paragraph)


# Import the meeting date data
meeting_dates <- read.csv(paste0(raw_dir, "CBC/boe_meeting_dates.csv"),
                          stringsAsFactors = FALSE)
# Order by meeting date
meeting_dates$pub_date <- as.Date(meeting_dates$pub_date, format = "%d/%m/%Y")
meeting_dates$meet_date <- as.Date(meeting_dates$meet_date, format = "%d/%m/%Y")
meeting_dates <- meeting_dates[order(meeting_dates$meet_date),]

meeting_dates$difftime <- as.numeric(difftime(meeting_dates$pub_date, meeting_dates$meet_date, units = "days"))
plot(meeting_dates$meet_date, meeting_dates$difftime, type = "l")



bankminutes.df <- merge(bankminutes.df, meeting_dates, by = c("year", "month"), all.x = TRUE)
bankminutes.df <- bankminutes.df[which(bankminutes.df$paragraph != "?"),]
table(is.na(bankminutes.df$pub_date))
table(is.na(bankminutes.df$meet_date))

# Order by meeting date
bankminutes.df$pub_date <- as.Date(bankminutes.df$pub_date, format = "%d/%m/%Y")
bankminutes.df$meet_date <- as.Date(bankminutes.df$meet_date, format = "%d/%m/%Y")
bankminutes.df <- bankminutes.df[order(bankminutes.df$meet_date),]


# First remove the first paragraph of each document
#View(bankminutes.df[which(bankminutes.df$meet_date != dplyr::lag(bankminutes.df$meet_date)),])
bankminutes.df[which(str_detect(bankminutes.df$paragraph, "MPC MEETING")), "paragraph"] <- NA
bankminutes.df[which(str_detect(bankminutes.df$paragraph, "MINUTES OF THE MPC")), "paragraph"] <- NA
bankminutes.df[which(str_detect(bankminutes.df$paragraph, "MONETARY POLICY MEETING")), "paragraph"] <- NA
bankminutes.df[which(str_detect(bankminutes.df$paragraph, "MEETING OF THE MPC")), "paragraph"] <- NA
bankminutes.df[which(str_detect(bankminutes.df$paragraph, "6-7 AUGUST 1997")), "paragraph"] <- NA
#bankminutes.df[which(bankminutes.df$paragraph == "ANNEX: SUMMARY OF DATA PRESENTED BY BANK STAFF"), "paragraph"] <- NA
bankminutes.df[which(bankminutes.df$paragraph == ""), "paragraph"] <- NA


# Now do the same for the last of each document
#View(bankminutes.df[which(bankminutes.df$meet_date != dplyr::lead(bankminutes.df$meet_date)),])
bankminutes.df[which((str_detect(bankminutes.df$paragraph, 
                                 "The following members"))), "paragraph"] <- NA

# Remove all the deleted paragraphs from the dataframe
bankminutes.df <- bankminutes.df[which(!is.na(bankminutes.df$paragraph)),]



# Create a variable to separate out the paragraphs which are part of the annex
bankminutes.df$annex <- 0 
bankminutes.df[which(str_detect(bankminutes.df$paragraph, "ANNEX:")),"annex"] <- 1

# All rows in the same meeting but after the annex selected
for (i in 2:nrow(bankminutes.df)){
  if ((bankminutes.df$annex[i] != bankminutes.df$annex[i-1]) & 
      (bankminutes.df$meet_date[i] == bankminutes.df$meet_date[i-1])){
        bankminutes.df$annex[i] <- 1
      }
}

plot(table(bankminutes.df$meet_date))
summary(nchar(bankminutes.df$paragraph))

# Add unique paragraph identifier
unique_id <- paste0("boe_", 1:nrow(bankminutes.df))
bankminutes.df$unique_id <- unique_id
bankminutes.df$central_bank <- "Bank of England"

# Add a unique meeting identifier
meeting.df <- unique(bankminutes.df[, c("year", "month", "central_bank", "pub_date", "meet_date")])
meeting.df <- meeting.df[order(meeting.df$meet_date),]
table(duplicated(meeting.df[,c("year", "month")]))
meeting.df$meeting_id <- paste0("B_", 1:nrow(meeting.df))
bankminutes.df <- merge(bankminutes.df, meeting.df, by = c("year", "month", "central_bank", 
                                                            "pub_date", "meet_date"), all.x = TRUE)
bankminutes.df <- bankminutes.df[order(bankminutes.df$meet_date),]

bankminutes.df <- bankminutes.df[, c("unique_id", "meeting_id", "year", "month", "document", 
                                     "central_bank", "pub_date", "meet_date", "annex", "source", "paragraph")]

# Write the clean BoE minutes to file
write.csv(bankminutes.df, file = paste0(clean_dir, "CBC/bankminutes_clean.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)







############################# Federal Reserve ############################# 

### Import the Federal Reserve minutes data
import_filename <- paste0(raw_dir, "CBC/fedminutes_clean_short.txt")
fedminutes_all <- readtext(import_filename, encoding = "utf-8")
#fedminutes_all <- read.table(import_filename, sep="\t", header=TRUE, encoding = "utf-8")

# Create dataframe for the paragraphs
variables <- c( "year", "month", "document", "paragraph","seq")
fedminutes.df <- data.frame(matrix(NA,0,length(variables)))
colnames(fedminutes.df) <- variables

para.locs <- as.data.frame(str_locate_all(fedminutes_all, "[0-9]+\t"))
para.num <- length(para.locs[,1])

# Loop over the paragraphs
for (i in 1:(para.num-1)){
  
  # Create empty dataframe to fill
  temp.df <- data.frame(matrix(NA,0,length(variables)))
  colnames(temp.df) <- variables
  
  #print(i)
  # Pull out an individual article
  full.para <- str_sub(fedminutes_all, para.locs[i,1], (para.locs[(i+1),1]-1))
  
  temp <- as.data.frame(str_locate(full.para, "\t"))
  
  docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
  text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
  
  #temp <- as.data.frame(str_locate_all(text, "\t"))
  #assert_that(nrow(temp) == 1)
  
  #seq <- str_trim(str_sub(text, (temp[,"end"]+1)))
  #text <- str_trim(str_sub(text, 1, (temp[,"end"]-1)))
  
  temp.df[1, "document"] <- as.character(docid)
  temp.df[1, "paragraph"] <- text
   
  # Append to dataframe for that file
  fedminutes.df <- rbind(fedminutes.df, temp.df)
}
temp.df <- data.frame(matrix(NA,0,length(variables)))
colnames(temp.df) <- variables
full.para <- str_sub(fedminutes_all, para.locs[para.num,1])
temp <- as.data.frame(str_locate(full.para, "\t"))
docid <- str_trim(str_sub(full.para, 1, (temp[,"end"]-1)))
text <- str_trim(str_sub(full.para, (temp[,"end"]+1)))
temp.df[1, "document"] <- as.character(docid)
temp.df[1, "paragraph"] <- text
fedminutes.df <- rbind(fedminutes.df, temp.df)


# Edit some paragraph names as meeting sometimes starts in a different month to when it finishes
fedminutes.df[which(fedminutes.df$document == "199807"), "document"] <- "199806"
fedminutes.df[which(fedminutes.df$document == "201208"), "document"] <- "201207"
fedminutes.df[which(fedminutes.df$document == "201305"), "document"] <- "201304"

# Extract date information
year <- as.array(str_sub(fedminutes.df$document, 1, 4))
month <- as.array(as.numeric(str_sub(fedminutes.df$document, 5,6)))

# Functions to clean the year and date terms from the bankminutes file
month <- apply(month, 1, clean_month)

# Fill in the date values of dataframe
fedminutes.df$year <- as.numeric(year)
fedminutes.df$month <- month


# Import the meeting date data
meeting_dates <- read.csv(paste0(raw_dir, "CBC/Fed_meeting_dates.csv"),
                          stringsAsFactors = FALSE, encoding = "utf-8")

fedminutes.df <- merge(fedminutes.df, meeting_dates, by = c("year", "month"), all.x = TRUE)

fedminutes.df <- fedminutes.df[which(fedminutes.df$paragraph != "?"),]
table(is.na(fedminutes.df$pub_date))
table(is.na(fedminutes.df$meet_date))
#View(fedminutes.df[which(is.na(fedminutes.df$meet_date)),])


# Order by meeting date
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date, format = "%d/%m/%Y")
fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date, format = "%d/%m/%Y")
fedminutes.df <- fedminutes.df[order(fedminutes.df$meet_date),]

fedminutes.df$difftime <- as.numeric(difftime(fedminutes.df$pub_date, fedminutes.df$meet_date, units = "days"))
plot(fedminutes.df$meet_date, fedminutes.df$difftime, type = "l")

# First remove the first paragraph of each document
#View(fedminutes.df[which(fedminutes.df$meet_date != dplyr::lag(fedminutes.df$meet_date)),])


# Now do the same for the last of each document
#View(fedminutes.df[which(fedminutes.df$meet_date != dplyr::lead(fedminutes.df$meet_date)),])
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                 "Votes against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Votes for"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Voting against"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Voting for"))), "paragraph"] <- NA
fedminutes.df[which((str_detect(fedminutes.df$paragraph, 
                                "Absent and"))), "paragraph"] <- NA

# Remove all the deleted paragraphs from the dataframe
fedminutes.df <- fedminutes.df[which(!is.na(fedminutes.df$paragraph)),]


plot(table(fedminutes.df$meet_date))
summary(nchar(fedminutes.df$paragraph))
fedminutes.df$nchar <- nchar(fedminutes.df$paragraph)

# Add unique paragraph identifier
unique_id <- paste0("fed_", 1:nrow(fedminutes.df))
fedminutes.df$unique_id <- unique_id
fedminutes.df$central_bank <- "Federal Reserve"

plot(table(bankminutes.df$meet_date))
summary(nchar(bankminutes.df$paragraph))
fedminutes.df$annex <- 0 

# Add a unique meeting identifier
meeting.df <- unique(fedminutes.df[, c("year", "month", "central_bank", "pub_date", "meet_date")])
meeting.df <- meeting.df[order(meeting.df$meet_date),]
table(duplicated(meeting.df[,c("year", "month")]))
meeting.df$meeting_id <- paste0("F_", 1:nrow(meeting.df))
fedminutes.df <- merge(fedminutes.df, meeting.df, by = c("year", "month", "central_bank", 
                                                           "pub_date", "meet_date"), all.x = TRUE)
fedminutes.df <- fedminutes.df[order(fedminutes.df$meet_date),]

fedminutes.df <- fedminutes.df[, c("unique_id", "meeting_id", "year", "month", "document", 
                                     "central_bank", "pub_date", "meet_date", "annex", "source", "paragraph")]

# Write the clean Federal Reserve minutes to a file
write.csv(fedminutes.df, file = paste0(clean_dir, "CBC/fedminutes_clean.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)





############################# ECB ############################# 

### Import the ECB statement data
import_filename <- paste0(raw_dir, "CBC/ecbstatements_clean.txt")
ecbminutes_all <- readtext(import_filename, encoding = "utf-8")
ecbminutes_all <- ecbminutes_all$text 

# replace all \n\n(i) type breaks so that they are included in the same paragraph
temp <- as.data.frame(str_locate_all(ecbminutes_all, "\n\n\\("))
ecbminutes_all <- str_replace_all(ecbminutes_all, "\n\n\\(", " \\(")

# Create dataframe for the paragraphs
variables <- c( "year", "month", "document", "paragraph")
ecbstatements.df <- data.frame(matrix(NA,0,length(variables)))
colnames(ecbstatements.df) <- variables

doc.locs <- as.data.frame(str_locate_all(ecbminutes_all, "\n[0-9]+_[0-9]+_[0-9]+_S.txt"))
doc.num <- length(doc.locs[,1])

# Loop over the paragraphs
for (i in 1:(doc.num-1)){
  
  # The whole document
  full.doc <- str_trim(str_sub(ecbminutes_all, doc.locs[i,"start"], (doc.locs[(i+1),"start"]-1)))
  
  # Extract the document name at the top of each document
  docid <- str_trim(str_sub(full.doc, 1, (as.data.frame(str_locate(full.doc, "\t\t\n\n"))[1,"start"]-1)))
  full.doc <- str_trim(str_sub(full.doc, (as.data.frame(str_locate(full.doc, "\t\t\n\n"))[1,"end"])))
  
  para.locs <- as.data.frame(str_locate_all(full.doc, "\n\n"))
  para.num <- length(para.locs[,1])
  
  # Create empty dataframe to fill
  temp.df <- data.frame(matrix(NA,(para.num+1),length(variables)))
  colnames(temp.df) <- variables
  temp.df$document <- docid
  temp.df$year <- as.numeric(str_sub(docid, 1, 4))
  temp.df$month <- clean_month(as.numeric(str_sub(docid, 6,7)))
  
  text <- str_trim(str_sub(full.doc, 1, (para.locs[(1),"start"]-1)))
  temp.df$paragraph[1] <- text
  
  for(j in 2:(para.num)){
    text <- str_trim(str_sub(full.doc, (para.locs[j-1,"end"]), (para.locs[(j),"end"])))
    temp.df$paragraph[j] <- text
  }
  # Extra one at the ed because \n\n has been removed by str_trim
  text <- str_trim(str_sub(full.doc, (para.locs[para.num,"end"])))
  temp.df$paragraph[(para.num+1)] <- text
  
  
  ecbstatements.df <- rbind(ecbstatements.df, temp.df)
}
full.doc <- str_trim(str_sub(ecbminutes_all, (doc.locs[(doc.num),"start"])))
docid <- str_trim(str_sub(full.doc, 1, (as.data.frame(str_locate(full.doc, "\t\t\n\n"))[1,"start"]-1)))
full.doc <- str_trim(str_sub(full.doc, (as.data.frame(str_locate(full.doc, "\t\t\n\n"))[1,"end"])))
para.locs <- as.data.frame(str_locate_all(full.doc, "\n\n"))
para.num <- length(para.locs[,1])
temp.df <- data.frame(matrix(NA,para.num,length(variables)))
colnames(temp.df) <- variables
temp.df$document <- docid
temp.df$year <- as.numeric(str_sub(docid, 1, 4))
temp.df$month <- clean_month(as.numeric(str_sub(docid, 6,7)))
text <- str_trim(str_sub(full.doc, 1, (para.locs[(1),"start"]-1)))
temp.df$paragraph[1] <- text
for(j in 2:(para.num)){
  text <- str_trim(str_sub(full.doc, (para.locs[j-1,"end"]), (para.locs[(j),"end"])))
  temp.df$paragraph[j] <- text
}
ecbstatements.df <- rbind(ecbstatements.df, temp.df)
ecbstatements.df$source <- ecbstatements.df$document

# Import the meeting dates as a double check for ECB
meeting_dates <- read.csv(file = paste0(raw_dir, "CBC/ECB_meeting_dates.csv"), 
          encoding = "utf-8", stringsAsFactors = FALSE)

ecbstatements.df <- merge(ecbstatements.df, meeting_dates, by = c("year", "month", "source"), all.x = TRUE)

# Remove a handful of duplicates
ecbstatements.df <- ecbstatements.df[!duplicated(ecbstatements.df[,c("year", "month", "source","paragraph")]),]

table(is.na(ecbstatements.df$pub_date))
table(is.na(ecbstatements.df$meet_date))

# Order by meeting date
ecbstatements.df$pub_date <- as.Date(ecbstatements.df$pub_date, format = "%d/%m/%Y")
ecbstatements.df$meet_date <- as.Date(ecbstatements.df$meet_date, format = "%d/%m/%Y")
ecbstatements.df <- ecbstatements.df[order(ecbstatements.df$meet_date),]


### Remove the preamble and logistical comments

# First remove the first paragraph of each document
#View(ecbstatements.df[which(ecbstatements.df$meet_date != dplyr::lag(ecbstatements.df$meet_date)),])
ecbstatements.df[which(ecbstatements.df$paragraph == "ECB Press conference: Introductory statement"), "paragraph"] <- NA
ecbstatements.df[which(ecbstatements.df$meet_date != dplyr::lag(ecbstatements.df$meet_date)),"paragraph"] <- NA
# Inspect the new first paragraph and remove if it doesn't include "Ladies and gentlemen"
#View(ecbstatements.df[which(is.na(dplyr::lag(ecbstatements.df$paragraph)) & 
#                         !str_detect(ecbstatements.df$paragraph,"Ladies and gentlemen")),"paragraph"])
ecbstatements.df[which(is.na(dplyr::lag(ecbstatements.df$paragraph)) & 
                         !str_detect(ecbstatements.df$paragraph,"Ladies and gentlemen")),"paragraph"] <- NA
#View(ecbstatements.df[which(is.na(dplyr::lag(ecbstatements.df$paragraph)) & 
#                              !str_detect(ecbstatements.df$paragraph,"Ladies and gentlemen")),"paragraph"])
ecbstatements.df[which(is.na(dplyr::lag(ecbstatements.df$paragraph)) & 
                         !str_detect(ecbstatements.df$paragraph,"Ladies and gentlemen, ")),"paragraph"] <- NA
#View(ecbstatements.df[which(is.na(dplyr::lag(ecbstatements.df$paragraph)) & 
#                              !str_detect(ecbstatements.df$paragraph,"Ladies and gentlemen")),"paragraph"])

# A few ones that slip through to tie up
ecbstatements.df[which(ecbstatements.df$paragraph == "With a Transcript of the questions and answers"), "paragraph"] <- NA
ecbstatements.df[which(ecbstatements.df$paragraph == "Q&A on TARGET2-Securities"), "paragraph"] <- NA
ecbstatements.df[which(ecbstatements.df$paragraph == "Welcome address by Jens Weidmann, President of the Deutsche Bundesbank"), "paragraph"] <- NA
#View(ecbstatements.df[which(is.na(dplyr::lag(ecbstatements.df$paragraph)) & 
#                              !str_detect(ecbstatements.df$paragraph,"Ladies and gentlemen")),])


# Now do the same for the last of each document
#View(ecbstatements.df[which(ecbstatements.df$meet_date != dplyr::lead(ecbstatements.df$meet_date)),])
#View(ecbstatements.df[which((ecbstatements.df$meet_date != dplyr::lead(ecbstatements.df$meet_date)) & 
#                              (str_detect(ecbstatements.df$paragraph, "questions"))), ])
ecbstatements.df[which((ecbstatements.df$meet_date != dplyr::lead(ecbstatements.df$meet_date)) & 
                         (str_detect(ecbstatements.df$paragraph, "questions")) & 
                         !str_detect(ecbstatements.df$paragraph, "statutory function") & 
                         !str_detect(ecbstatements.df$paragraph, "security features") & 
                         !str_detect(ecbstatements.df$paragraph, "sustainable growth")), "paragraph"]<- NA



# Remove all the deleted paragraphs from the dataframe
ecbstatements.df <- ecbstatements.df[which(!is.na(ecbstatements.df$paragraph)),]

plot(table(ecbstatements.df$meet_date))
summary(nchar(ecbstatements.df$paragraph))
ecbstatements.df$nchar <- nchar(ecbstatements.df$paragraph)
# The huge paragraph here is actually correct (https://www.ecb.europa.eu/press/pressconf/1998/html/is981222.en.html)

# Remove the October 16th 2014 press conference as this was not a standard Governing Council meeting, but a "comprehensive assesment"
ecbstatements.df <- ecbstatements.df[which(ecbstatements.df$meet_date != "2014-10-26"),]

# Add unique paragraph identifier
unique_id <- paste0("ecb_", 1:nrow(ecbstatements.df))
ecbstatements.df$unique_id <- unique_id
ecbstatements.df$central_bank <- "European Central Bank"
ecbstatements.df$annex <- 0 

# Add a unique meeting identifier
meeting.df <- unique(ecbstatements.df[, c("year", "month", "central_bank", "pub_date", "meet_date")])
meeting.df <- meeting.df[order(meeting.df$meet_date),]
table(duplicated(meeting.df[,c("year", "month")]))
meeting.df$meeting_id <- paste0("E_", 1:nrow(meeting.df))
ecbstatements.df <- merge(ecbstatements.df, meeting.df, by = c("year", "month", "central_bank", 
                                                         "pub_date", "meet_date"), all.x = TRUE)
ecbstatements.df <- ecbstatements.df[order(ecbstatements.df$meet_date),]

ecbstatements.df <- ecbstatements.df[, c("unique_id", "meeting_id", "year", "month", "document", 
                                   "central_bank", "pub_date", "meet_date", "annex", "source", "paragraph")]

# Write the clean ECB minutes to a file
write.csv(ecbstatements.df, file = paste0(clean_dir, "CBC/ecbstatements_clean.csv"), 
          fileEncoding = "utf-8", row.names = FALSE)


############################# End ############################# 