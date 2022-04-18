####
# This file combines SPF data with the NYT and Fed topics
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
require(plm)
require(lfe)
require(stargazer)
require(readxl)
require(openxlsx)
require(lubridate)
require(zoo)


### Define the directories where raw data is stored and clean will be saved
clean_dir <- "data/"
export_dir <- "figures/"


############################# Import the topic proportions ############################# 

# Import the weekly article data, averaged over articles
import_filename =  "data/articlemeans_jointtopics.csv"
articlelevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import the minutes data estimated at the meeting level
import_filename = "data/fedmeetingmeans_jointtopics.csv"
meetinglevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import details for meetings to merge with 
clean_filename = "data/fedminutes_long_clean.csv"
fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date)
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date)
meeting.key <- unique(fedminutes.df[,c("meeting_id", "pub_date", "meet_date")])

meeting.df <- merge(meetinglevel.means, meeting.key, by = "meeting_id", all.x = TRUE)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df$pub_date <- as.Date(meeting.df$pub_date)


# Split and merge the articles data
pre_articles.df <- articlelevel.means[which(!is.na(articlelevel.means$subsequent_meeting)),]
pre_articles.df$meeting_id <- pre_articles.df$subsequent_meeting
pre_articles.df <- merge(pre_articles.df, meeting.key, by = "meeting_id")

post_articles.df <- articlelevel.means[which(!is.na(articlelevel.means$recent_meeting)),]
post_articles.df$meeting_id <- post_articles.df$recent_meeting
post_articles.df <- merge(post_articles.df, meeting.key, by = "meeting_id")






############################# Get quarterly averages for minutes ############################# 

# Set which variable we will use (to "y", "T" or "lT")
var_used <- "T"

# Toggle the topic number (might need to adjust)
k <- 30
variablenames <- paste0(var_used, 1:k)

meeting.df$quarter <- floor_date(meeting.df$meet_date, "quarter")
meeting.quarterly <- meeting.df %>%
  dplyr::select(quarter, variablenames) %>%
  group_by(quarter) %>%
  summarise_all(mean)

pre_articles.df$quarter <- floor_date(pre_articles.df$meet_date, "quarter")
articles.quarterly <- pre_articles.df %>%
  dplyr::select(quarter, variablenames) %>%
  group_by(quarter) %>%
  summarise_all(mean)

names2 <- paste0(variablenames,"_news")
articles.quarterly[,names2] <- articles.quarterly[,variablenames]
articles.quarterly <- articles.quarterly[,c("quarter", names2)]

topics.df <- merge(meeting.quarterly, articles.quarterly, by = "quarter", all.x = TRUE)
total.df <- topics.df

"
code <- 'NGDP'
descrip <- 'Growth'
topic <- 20
disp_measure = 1 # disp = 1 is levels, and 2 is growth
annual = FALSE
fed = TRUE
news = TRUE
"


############################# Match topics with SPF series ############################# 

# Define a function to import and plot the SPF dispersion data alongside the relevant topic

plot_topicdisp <- function(code, descrip, topic, disp_measure, topics.df, total.df, annual = FALSE,
                           fed = TRUE, news = TRUE){
  import_filename <- paste0(clean_dir, "SPF/Dispersion_", disp_measure, ".xlsx")
  
  # Read and clean the SPF dispersion data
  dispersion.df <- read_xlsx(import_filename, sheet <- code, skip = 9)
  if (annual == FALSE){
    command <- paste0("dispersion.df$",code,"_dispersion <- dispersion.df$`",code,"_D",disp_measure,"(T)`")
  } else{
    command <- paste0("dispersion.df$",code,"_dispersion <- dispersion.df$`",code,"_D",disp_measure,"`")
  }
  eval(parse(text=command))
  if (annual == FALSE){
    command <- paste0("dispersion.df <- dispersion.df[which(dispersion.df$", code,"_dispersion != \"#N/A\"),]")
  } else {
    command <- paste0("dispersion.df[which(dispersion.df$", code,"_dispersion == \"#N/A\"), \"",code,"_dispersion\"] <- NA")
  }
  eval(parse(text=command))
  command <- paste0("dispersion.df$", code,"_dispersion <- as.numeric(dispersion.df$", code, "_dispersion)")
  eval(parse(text=command))
  
  # Convert dates to quarterly 
  dispersion.df$Date <- dispersion.df$`Survey_Date(T)`
  year <- str_sub(dispersion.df$Date, 1,4)
  quarter <- str_sub(dispersion.df$Date, 5,6)
  quarter <- str_replace(quarter, "Q1", "01")
  quarter <- str_replace(quarter, "Q2", "04")
  quarter <- str_replace(quarter, "Q3", "07")
  quarter <- str_replace(quarter, "Q4", "10")
  
  date <- paste(year, quarter, "01", sep = "-")
  
  dispersion.df$Date <- as.Date(date)
  dispersion.df$quarter <- floor_date(dispersion.df$Date, "quarter")
  
  if (annual == TRUE){
    command <- paste0(" dispersion.df$", code,"_dispersion <- na.locf(dispersion.df$", code,"_dispersion, na.rm = FALSE)")
    eval(parse(text=command))
  }
  
  # Keep only the date and dispersion measure
  command <- paste0("dispersion.df <- select(dispersion.df, c(quarter, ", code,"_dispersion))")
  eval(parse(text=command))
  
  # Merge with the topic data
  plot.df <- merge(topics.df, dispersion.df, by = "quarter", all.x = TRUE)
  
  # Calculate mean and variance for normalisation
  command <- paste0("exp.mean <- mean(plot.df$", code, "_dispersion, na.rm = TRUE)
                    exp.var <- sd(plot.df$",code,"_dispersion, na.rm = TRUE)")
  eval(parse(text=command))
  command <- paste0("meeting.mean <- mean(plot.df$T", topic, ", na.rm = TRUE)
                    meeting.var <- sd(plot.df$T",topic, ", na.rm = TRUE)
                    article.mean <- mean(plot.df$T", topic,"_news, na.rm = TRUE)
                    article.var <- sd(plot.df$T", topic, "_news, na.rm = TRUE)")
  eval(parse(text=command))
  
  # Plot CPI nowcast dispersion and quarterly topic propotions
  if (fed){
    command <- paste0("
                    ggplot(plot.df, aes(x = quarter)) + theme_bw() +
                      scale_color_manual(\"Legend\",
                      values = c(\"SPF ", code, " dispersion\" = \"darkturquoise\", \"Fed minutes\" = \"blue3\", 
                      \"NYT articles\" = \"dimgray\")) +
                      geom_line(aes(y = (",code, "_dispersion - exp.mean)/exp.var, color = \"SPF ", code, " dispersion\")) +
                      geom_line(aes(y = (T", topic, " - meeting.mean)/meeting.var, color = \"Fed minutes\")) +
                      geom_line(aes(y = (T", topic, "_news - article.mean)/article.var, color = \"NYT articles\")) +
                      xlab(\"Quarter\") +
                      ylab(\"Normalised value\") + 
                      ggtitle(\"", descrip,  " attention and expectation dispersion\")
                      ggsave(paste0(export_dir, \"SPF_topics/", code, "disp_topic", topic, ".png\"),
                        width = 6,height = 4)")
    eval(parse(text=command))
  } else {
    command <- paste0("
                    ggplot(plot.df, aes(x = quarter)) + theme_bw() +
                      scale_color_manual(\"Legend\",
                      values = c(\"SPF ", code, " dispersion\" = \"darkturquoise\", \"Fed minutes\" = \"blue3\", 
                      \"NYT articles\" = \"dimgray\")) +
                      geom_line(aes(y = (",code, "_dispersion - exp.mean)/exp.var, color = \"SPF ", code, " dispersion\")) +
                      geom_line(aes(y = (T", topic, "_news - article.mean)/article.var, color = \"NYT articles\")) +
                      xlab(\"Quarter\") +
                      ylab(\"Normalised value\") + 
                      ggtitle(\"", descrip,  " attention and expectation dispersion\")
                      ggsave(paste0(export_dir, \"SPF_topics/", code, "disp_topic", topic, ".png\"),
                        width = 6,height = 4)")
    eval(parse(text=command))
  }
  
  
  if (paste0(code, "_dispersion") %in% colnames(total.df)){
    print("Already added to total.df")
    merged.df <- total.df
    
    command <- paste0("merged.df$", code, "_dispersion <- plot.df$",code,"_dispersion")
    eval(parse(text=command))
    
    # Name a topic
    command <- paste0("merged.df$", code, "_topic_fed <- merged.df$T",topic)
    eval(parse(text=command))
    command <- paste0("merged.df$", code, "_topic_news <- merged.df$T",topic, "_news")
    eval(parse(text=command))
    
  } else{
    merged.df <- merge(total.df, dispersion.df, by = "quarter", all.x = TRUE)
    merged.df$quarter <- as.Date(total.df$quarter)
    
    # Name a topic
    command <- paste0("merged.df$", code, "_topic_fed <- merged.df$T",topic)
    eval(parse(text=command))
    command <- paste0("merged.df$", code, "_topic_news <- merged.df$T",topic, "_news")
    eval(parse(text=command))
    
  }
   return(merged.df)
}


# Inflation
total.df <- plot_topicdisp(code = "CPI", descrip = "Inflation", topic = 9, topics.df = topics.df, 
                           disp_measure = 1, total.df = total.df)
cor.test(total.df$CPI_dispersion, total.df$CPI_topic_news)
cor.test(total.df$CPI_dispersion, total.df$CPI_topic_fed)


# GDP
total.df <- plot_topicdisp(code = "NGDP", descrip = "GDP", topic = 20, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$NGDP_dispersion, total.df$NGDP_topic_news)
cor.test(total.df$NGDP_dispersion, total.df$NGDP_topic_fed)


# Employment
total.df <- plot_topicdisp(code = "EMP", descrip = "Employment", topic = 16, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$EMP_dispersion, total.df$EMP_topic_news)
cor.test(total.df$EMP_dispersion, total.df$EMP_topic_fed)

# Unemployment
total.df <- plot_topicdisp(code = "UNEMP", descrip = "Unemployment", topic = 16, topics.df = topics.df, 
                           disp_measure = 1,total.df = total.df)
cor.test(total.df$UNEMP_dispersion, total.df$UNEMP_topic_news)
cor.test(total.df$UNEMP_dispersion, total.df$UNEMP_topic_fed)

# Profit
total.df <- plot_topicdisp(code = "CPROF", descrip = "Profits", topic = 23, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$CPROF_dispersion, total.df$CPROF_topic_news)
cor.test(total.df$CPROF_dispersion, total.df$CPROF_topic_fed)

# Industrial production
total.df <- plot_topicdisp(code = "INDPROD", descrip = "Production", topic = 29, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$INDPROD_dispersion, total.df$INDPROD_topic_news)
cor.test(total.df$INDPROD_dispersion, total.df$INDPROD_topic_fed)

# Housing/mortgages
total.df <- plot_topicdisp(code = "HOUSING", descrip = "Housing", topic = 26, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$HOUSING_dispersion, total.df$HOUSING_topic_news)
cor.test(total.df$HOUSING_dispersion, total.df$HOUSING_topic_fed)

# Interest rates
total.df <- plot_topicdisp(code = "TBILL", descrip = "Interest", topic = 24, topics.df = topics.df, 
                           disp_measure = 1,total.df = total.df, fed = TRUE)
cor.test(total.df$TBILL_dispersion, total.df$TBILL_topic_news)
cor.test(total.df$TBILL_dispersion, total.df$TBILL_topic_fed)
cor.test(total.df$TBILL_dispersion, total.df$NGDP_topic_news)

# Real GDP
total.df <- plot_topicdisp(code = "RGDP", descrip = "RGDP", topic = 20, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$RGDP_dispersion, total.df$TBILL_topic_news)
cor.test(total.df$RGDP_dispersion, total.df$TBILL_topic_fed)

# Consumption
total.df <- plot_topicdisp(code = "RCONSUM", descrip = "Consumption", topic = 19, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$RCONSUM_dispersion, total.df$RCONSUM_topic_news)
cor.test(total.df$RCONSUM_dispersion, total.df$RCONSUM_topic_fed)

# Investment
total.df <- plot_topicdisp(code = "RNRESIN", descrip = "Investment", topic = 15, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$RNRESIN_dispersion, total.df$RNRESIN_topic_news)
cor.test(total.df$RNRESIN_dispersion, total.df$RNRESIN_topic_fed)

# Residential
total.df <- plot_topicdisp(code = "RRESINV", descrip = "Residential", topic = 22, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$RRESINV_dispersion, total.df$RRESINV_topic_news)
cor.test(total.df$RRESINV_dispersion, total.df$RRESINV_topic_fed)

# Government
total.df <- plot_topicdisp(code = "RFEDGOV", descrip = "Fiscal", topic = 3, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$RFEDGOV_dispersion, total.df$RFEDGOV_topic_news)
cor.test(total.df$RFEDGOV_dispersion, total.df$RFEDGOV_topic_fed)

# State Government
total.df <- plot_topicdisp(code = "RSLGOV", descrip = "State", topic = 3, topics.df = topics.df, 
                           disp_measure = 2,total.df = total.df)
cor.test(total.df$RSLGOV_dispersion, total.df$RSLGOV_topic_news)
cor.test(total.df$RSLGOV_dispersion, total.df$RSLGOV_topic_fed)

# Equities
total.df <- plot_topicdisp(code = "STOCK10", descrip = "Equities", topic = 18, topics.df = topics.df, 
                           disp_measure = 1,total.df = total.df, annual = TRUE)
cor.test(total.df$STOCK10_dispersion, total.df$STOCK10_topic_news)
cor.test(total.df$STOCK10_dispersion, total.df$STOCK10_topic_fed)


# Test that GDP and CPI are better fits for their respective topics
cor.test(total.df$NGDP_dispersion, total.df$CPI_topic_fed)
cor.test(total.df$CPI_dispersion, total.df$NGDP_topic_fed)



############################# Create panel df with topics and SPF disp ############################# 

series <- c("CPI", "NGDP", "RGDP", "EMP", "UNEMP", "CPROF", "INDPROD", "HOUSING", "TBILL",
            "RCONSUM", "RNRESIN", "RRESINV", "RFEDGOV", "RSLGOV")

### Keep only the topics which are matched to an SPF series
command <- paste0("total.panel <- select(total.df, quarter, ", 
                  paste(paste0("starts_with(\"", series, "\")"), collapse = ", "), ")")
eval(parse(text=command))

### Gather into panel 
total.panel <- gather(total.panel, topic, value, -quarter)

total.panel$series <- NA
for (s in series){
  total.panel[which(str_detect(total.panel$topic, s)),"series"] <- s
}

total.panel$type <- NA
for (t in c("dispersion", "fed", "news")){
  total.panel[which(str_detect(total.panel$topic, t)),"type"] <- t
}

### Spread back to get dispersion, fed and news measures for each series
total.panel <- spread(total.panel, type, value)
total.panel <- select(total.panel, -topic)

total.panel <- total.panel %>%
  group_by(quarter, series) %>% 
  summarise_all(funs(mean), na.rm = TRUE)

### Normalise by series
total_means <- total.panel %>%
  group_by(series) %>% 
  select(-quarter) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)

total.panel <- merge(total.panel, total_means, by = "series")
total.panel$dispersion_std <- (total.panel$dispersion - total.panel$dispersion_mean)/total.panel$dispersion_sd
total.panel$fed_std <- (total.panel$fed - total.panel$fed_mean)/total.panel$fed_sd
total.panel$news_std <- (total.panel$news - total.panel$news_mean)/total.panel$news_sd



clean_filename = "data/topics_forecasts_panel.csv"
write.csv(total.panel, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)
total.panel <- read.csv("data/topics_forecasts_panel.csv", stringsAsFactors = FALSE)

### Convert to 
total.panel$quarter <- as.Date(total.panel$quarter)
total.panel$period <- as.numeric(as.factor(total.panel$quarter))
total.panel <- pdata.frame(data.frame(total.panel), index = c("series", "period"))


############################# Cross-correlation matrix for the series ############################# 
test1 <- cor.test(total.df$NGDP_dispersion, total.df$CPI_topic_fed)
test1$estimate
test1 <- cor.test(total.df$CPI_topic_fed, total.df$NGDP_dispersion)
test1$estimate
cor.test(total.df$CPI_topic_news, total.df$NGDP_dispersion)
cor.test(total.df$NGDP_topic_fed, total.df$CPI_dispersion)
cor.test(total.df$NGDP_topic_news, total.df$CPI_dispersion)
cor.test(total.df$NGDP_topic_news, total.df$CPI_topic_news)
cor.test(total.df$CPROF_topic_fed, total.df$CPROF_dispersion)
cor.test(total.df$RCONSUM_topic_fed, total.df$RCONSUM_dispersion)
cor.test(total.df$HOUSING_topic_news, total.df$HOUSING_dispersion)
cor.test(total.df$INDPROD_topic_news, total.df$INDPROD_dispersion)




series <- as.character(unique(total.panel$series))


### Correlation matrix for Fed topics
fed.cor.matrix <- data.frame(matrix(NA, nrow = 3*length(series), ncol = (1+length(series))))
fed.cor.matrix.est <- matrix(NA, nrow = length(series), ncol = (length(series)))
fed.cor.matrix.pval <- matrix(NA, nrow = length(series), ncol = (length(series)))
split_rows <- paste(as.vector(paste(series, "x n", "x n x")))
split_rows <- paste(split_rows, collapse = "")
split_rows <- (str_split(split_rows, "x"))[[1]]
colnames(fed.cor.matrix) <- c("Series", series)
fed.cor.matrix$Series <- str_replace_all(split_rows[1:42], "n", "")

i <- 1
k <- 1
s <- series[1]
t <- series[1]
for (s in series){
  
  # Dispersion series
  disp_series <- total.panel[which(total.panel$series == s), "dispersion"]
  j <- 2
  for (t in series){
    
    
    fed_series <- total.panel[which(total.panel$series == t), "fed"]
    
    correlation <- cor.test(disp_series, fed_series)
    
    est <- round(correlation$estimate, 3)
    p_val <- round(correlation$p.value, 3)
    
    fed.cor.matrix.est[k,(j-1)] <- est
    fed.cor.matrix.pval[k,(j-1)] <- p_val
    
    if (p_val <= 0.01){
      stars <- "$^{***}$"
    } else if (p_val <= 0.05){
      stars <- "$^{**}$"
    } else if (p_val <= 0.1){
      stars <- "$^{*}$"
    } else {
      stars <- ""
    }
    
    entry <- paste0(est, stars)
    p_val <- paste0("(", p_val, ")")
    
    fed.cor.matrix[i,j] <- entry
    fed.cor.matrix[i+1,j] <- p_val
    
    print(paste(s,t, i, j, est))
    
    j <- j + 1
  }
  
  i <- i + 3
  k <- k + 1
}

stargazer(as.matrix(fed.cor.matrix), title = "SPF dispersion and FOMC topic correlation matrix")

N <- nrow(fed.cor.matrix.est)*nrow(fed.cor.matrix.est)
N_offdiag <- N - nrow(fed.cor.matrix.est)
N_diag <- nrow(fed.cor.matrix.est)


est_diag <- sum(diag(fed.cor.matrix.est))/N_diag
est_offdiag <- (sum(fed.cor.matrix.est) - sum(diag(fed.cor.matrix.est)))/N_offdiag
pval_diag <- sum(diag(fed.cor.matrix.pval))/N_diag
pval_offdiag <- (sum(fed.cor.matrix.pval) - sum(diag(fed.cor.matrix.pval)))/N_offdiag









### Correlation matrix for NYT topics
nyt.cor.matrix <- data.frame(matrix(NA, nrow = 3*length(series), ncol = (1+length(series))))
nyt.cor.matrix.est <- matrix(NA, nrow = length(series), ncol = (length(series)))
nyt.cor.matrix.pval <- matrix(NA, nrow = length(series), ncol = (length(series)))
split_rows <- paste(as.vector(paste(series, "x n", "x n x")))
split_rows <- paste(split_rows, collapse = "")
split_rows <- (str_split(split_rows, "x"))[[1]]
colnames(nyt.cor.matrix) <- c("Series", series)
nyt.cor.matrix$Series <- str_replace_all(split_rows[1:42], "n", "")

i <- 1
k <- 1
s <- series[1]
t <- series[1]
for (s in series){
  
  # 
  disp_series <- total.panel[which(total.panel$series == s), "dispersion"]
  j <- 2
  for (t in series){
    
    
    nyt_series <- total.panel[which(total.panel$series == t), "news"]
    
    correlation <- cor.test(disp_series, nyt_series)
    
    est <- round(correlation$estimate, 3)
    p_val <- round(correlation$p.value, 3)
    
    nyt.cor.matrix.est[k,(j-1)] <- est
    nyt.cor.matrix.pval[k,(j-1)] <- p_val
    
    if (p_val <= 0.01){
      stars <- "$^{***}$"
    } else if (p_val <= 0.05){
      stars <- "$^{**}$"
    } else if (p_val <= 0.1){
      stars <- "$^{*}$"
    } else {
      stars <- ""
    }
    
    entry <- paste0(est, stars)
    p_val <- paste0("(", p_val, ")")
    
    nyt.cor.matrix[i,j] <- entry
    nyt.cor.matrix[i+1,j] <- p_val
    
    print(paste(s,t, i, j, est))
    
    j <- j + 1
  }
  
  i <- i + 3
  k <- k + 1
}

stargazer(as.matrix(nyt.cor.matrix), title = "SPF dispersion and NYT topic correlation matrix")

N <- nrow(nyt.cor.matrix.est)*nrow(nyt.cor.matrix.est)
N_offdiag <- N - nrow(nyt.cor.matrix.est)
N_diag <- nrow(nyt.cor.matrix.est)


est_diag <- sum(diag(nyt.cor.matrix.est))/N_diag
est_offdiag <- (sum(nyt.cor.matrix.est) - sum(diag(nyt.cor.matrix.est)))/N_offdiag
pval_diag <- sum(diag(nyt.cor.matrix.pval))/N_diag
pval_offdiag <- (sum(nyt.cor.matrix.pval) - sum(diag(nyt.cor.matrix.pval)))/N_offdiag





############################# Panel analysis on the series ############################# 


# SPF dispersion as depepndent variable
model1 <- felm(dispersion ~ plm::lag(fed, 0) + plm::lag(news, 0) | series, data = total.panel)
summary(model1)
model1_std <- felm(dispersion_std ~ plm::lag(fed_std, 0) + plm::lag(news_std, 0) | series, data = total.panel)
summary(model1_std)
model2 <- felm(dispersion ~ plm::lag(fed, 0:1) + plm::lag(news, 0:1) + plm::lag(dispersion, 1:3) | series + period, data = total.panel)
summary(model2)
model2_std <- felm(dispersion_std ~ plm::lag(fed_std, 0:1) + plm::lag(news_std, 0:1) + plm::lag(dispersion_std, 1:3) | series + period, data = total.panel)
summary(model2_std)


# Fed topics as dependent variable
model3 <- felm(fed ~ plm::lag(dispersion, 0) | series, data = total.panel)
summary(model3)
model3_std <- felm(fed_std ~ plm::lag(dispersion_std, 0) + plm::lag(news_std, 0) | series, data = total.panel)
summary(model3_std)
model4 <- felm(fed ~ plm::lag(dispersion, 0:1)  + plm::lag(fed, 1:3) | series + quarter, data = total.panel)
summary(model4)
model4_std <- felm(fed_std ~ plm::lag(dispersion_std, -1:1) +  plm::lag(news_std, -1:1) +
                     plm::lag(fed_std, 1:3) | series + quarter, data = total.panel)
summary(model4_std)

# NYT articles as dependent variable
model5 <- felm(news ~ plm::lag(dispersion, 0) | series, data = total.panel)
summary(model5)
model5_std <- felm(news_std ~ plm::lag(dispersion_std, 0) | series, data = total.panel)
summary(model5_std)
model6 <- felm(news ~ plm::lag(dispersion, -1:1) + plm::lag(news, 1:3) | series + quarter, data = total.panel)
summary(model6)
model6_std <- felm(news_std ~ plm::lag(dispersion_std, -1:1) + plm::lag(news_std, 1:3) | series + quarter, data = total.panel)
summary(model6_std)


# Table
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE,
          title = "FOMC minutes, NYT articles and SPF forecast dispersion (unstandardised data)",
          label = "tab:topic_spf_results_unstand")

stargazer(model1_std, model2_std, model3_std, model4_std, model5_std, model6_std,
          table.placement = "H", df = FALSE,
          title = "Federal Reserve minutes, NYT articles and SPF forecast dispersion",
          label = "tab:topic_spf_results")







############################# End ############################# 