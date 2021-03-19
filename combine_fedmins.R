### Import the other Fed's data
fedminutes_new <- read.csv(paste0(import_dir, "FedMin-pressClean.csv"), stringsAsFactors = FALSE)
fedminutes_new <- fedminutes_new[which(fedminutes_new$Bulletin == "Minutes"),]
fedminutes_new$date <- as.Date(fedminutes_new$date)
fedminutes_new$text <- fedminutes_new$cleanText
fedminutes_new$text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", fedminutes_new$text)
fedminutes_new$text <- gsub("•", " ", fedminutes_new$text)
fedminutes_new$text <- gsub("’", " ", fedminutes_new$text)
fedminutes_new$text <- str_squish(fedminutes_new$text)


fedminutes_old <- read.csv(paste0(import_dir, "fedminutes_long_clean.csv"), stringsAsFactors = FALSE)
fedminutes_old$date <- as.character(fedminutes_old$meet_date)
fedminutes_df <- data.frame(date = unique(fedminutes_old$date), text = NA)
for (ii in 1:nrow(fedminutes_df)){
  obs <- which(fedminutes_old$date == fedminutes_df$date[ii])
  
  fedminutes_df$text[ii] <- paste(fedminutes_old$paragraph[obs], collapse = " ")
}
fedminutes_df$date <- as.Date(fedminutes_df$date)
fedminutes_df$text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", fedminutes_df$text)
fedminutes_df$text <- gsub("•", " ", fedminutes_df$text)
fedminutes_df$text <- gsub("’", " ", fedminutes_df$text)
fedminutes_df$text <- str_squish(fedminutes_df$text)



fedminutes_df<- rbind(fedminutes_df, fedminutes_new[,c("date", "text")])
fedminutes_df$date <- as.Date(fedminutes_df$date)
fedminutes_df <- fedminutes_df[order(fedminutes_df$date),]

fedminutes_df$agg_date <- floor_date(fedminutes_df$date, unit = "quarters")


fedminutes_phrases <- fedminutes_df[,c("date", "agg_date")]
fedminutes_phrases$nwords <- str_count(fedminutes_df$text," ")
fedminutes_phrases[,str_replace_all(phrases, " ", ".")] <- NA