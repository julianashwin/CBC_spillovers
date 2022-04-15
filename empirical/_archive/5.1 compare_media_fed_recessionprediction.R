####
# This file tests whether the Fed minutes or NYT articles are better predictors of the economy.
####

setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())


set.seed(123)

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
require(glmnet)


### Define the directories where raw data is stored and clean will be saved
raw_dir <- "~/Documents/DPhil/Raw_Data/"
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


############################# Import the topic proportions ############################# 

# Import the weekly article data, averaged over articles
import_filename = paste(clean_dir, "CBC/articlemeans_jointtopics.csv", sep = "/")
articlelevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)


# Import the minutes data estimated at the meeting level
import_filename = paste(clean_dir, "CBC/fedmeetingmeans_jointtopics.csv", sep = "/")
meetinglevel.means <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

# Import details for meetings to merge with 
clean_filename = paste(clean_dir, "CBC/fedminutes_long_clean.csv", sep = "/")
fedminutes.df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)
fedminutes.df$meet_date <- as.Date(fedminutes.df$meet_date)
fedminutes.df$pub_date <- as.Date(fedminutes.df$pub_date)
meeting.key <- unique(fedminutes.df[,c("meeting_id", "pub_date", "meet_date")])

meeting.df <- merge(meetinglevel.means, meeting.key, by = "meeting_id", all.x = TRUE)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)
meeting.df$pub_date <- as.Date(meeting.df$pub_date)




import_filename = paste(clean_dir, "CBC/fednyt_sentiment", sep = "/")
sentiment_quarterly <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

sentiment_quarterly$quarter <- as.Date(sentiment_quarterly$quarter)


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
  select(quarter, variablenames) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))

pre_articles.df$quarter <- floor_date(pre_articles.df$meet_date, "quarter")
articles.quarterly <- pre_articles.df %>%
  select(quarter, variablenames) %>%
  group_by(quarter) %>%
  summarise_all(list(mean))

names2 <- paste0(variablenames,"_news")
articles.quarterly[,names2] <- articles.quarterly[,variablenames]
articles.quarterly <- articles.quarterly[,c("quarter", names2)]

topics.df <- merge(meeting.quarterly, articles.quarterly, by = "quarter", all.x = TRUE)
total.df <- topics.df


# US growth
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_growth.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$growth <- import.df[,2]
total.df <- merge(total.df, import.df[,c("quarter", "growth")], by = "quarter", all.x = TRUE)

# US quarterly inflation 
import_filename <- paste(raw_dir, "Macro_data/FRED_downloads/US_inflation_quarterly.csv", sep = "")
import.df <- read.csv(import_filename)
import.df$quarter <- as.Date(import.df$DATE)
import.df$inflation <- import.df[,2]
total.df <- merge(total.df, import.df[,c("quarter", "inflation")], by = "quarter", all.x = TRUE)

total.df$quarter <- as.Date(total.df$quarter)





### Merge in the sentiment index 

total.df <- merge(total.df, sentiment_quarterly, by = "quarter")
total.df$quarter <- as.Date(total.df$quarter)

ggplot(total.df) + 
  scale_color_manual("Variable",
                     values = c("NYT sentiment" = "dimgray", "Fed sentiment" = "blue3", "GDP growth" = "red")) +
  geom_line(aes(x = quarter, y = fed_sentiment_std, color = "Fed sentiment")) +
  geom_line( aes(x = quarter, y = nyt_sentiment_std, color = "NYT sentiment")) +
  geom_line( aes(x = quarter, y = (growth - mean(growth))/sd(growth), color = "GDP growth")) +
  xlab('Date') +
  ylab("Standardised units") + 
  ggtitle("GDP growth, Fed and NYT sentiment")
ggsave(paste0(export_dir, "USgrowth_sentiment.png"))







### Load the SPF nowcasts of GDP growth 

# Notes from the documentation of the SPF:
#Growth rates are those for quarter-over-quarter growth, expressed in annualized
#percentage points, beginning with the forecast for the current quarter.
#• Each worksheet lists the year and quarter in which the survey was conducted.
#• The column headers in each worksheet follow the same nomenclature described
#above. (We use “d” in the column header to denote a growth rate.) In particular, the
#number “2” appended to a column header represents a forecast for the quarter in
#which the survey is conducted. The numbers “3” to “6” represents the quarter-overquarter growth rate forecasts for the following four quarters.



import_filename <- paste0(raw_dir, "SPF/Median_NGDP_Growth.xlsx")
nowcast.df <- read_xlsx(import_filename)

nowcast.df$quarter <- as.character(nowcast.df$QUARTER)
nowcast.df$quarter[which(nowcast.df$quarter == "1")] <- "01-01"
nowcast.df$quarter[which(nowcast.df$quarter == "2")] <- "04-01"
nowcast.df$quarter[which(nowcast.df$quarter == "3")] <- "07-01"
nowcast.df$quarter[which(nowcast.df$quarter == "4")] <- "10-01"

nowcast.df$quarter <- paste(as.character(nowcast.df$YEAR), nowcast.df$quarter, sep = "-")
nowcast.df$quarter <- as.Date(nowcast.df$quarter)
total.df$quarter <- as.Date(total.df$quarter)

total.df <- merge(total.df, nowcast.df, by = "quarter", all.x = TRUE)



ggplot(total.df) + 
  scale_color_manual("Variable", values = c("NYT sentiment" = "dimgray", "Fed sentiment" = "blue3", "GDP growth" = "red", 
                                  "SPF forecast" = "darkturquoise")) +
  geom_line(aes(x = quarter, y = fed_sentiment_std, color = "Fed sentiment")) +
  geom_line( aes(x = quarter, y = nyt_sentiment_std, color = "NYT sentiment")) +
  geom_line( aes(x = quarter, y = (growth - mean(growth))/sd(growth), color = "GDP growth")) +
  geom_line( aes(x = quarter, y = (DNGDP2 - mean(DNGDP2))/sd(DNGDP2), color = "SPF forecast")) +
  xlab('Date') +
  ylab("Standardised units") + 
  ggtitle("GDP growth, Fed and NYT sentiment")
ggsave(paste0(export_dir, "USgrowth_sentiment_spf.png"))
  




total.df$ind <- 1
total.df$period <- as.integer(as.factor(total.df$quarter))
total.df <- pdata.frame(data.frame(total.df), index = c("ind", "period"))

total.df$growth_1lag <- plm::lag(total.df$growth, 1)
total.df$DNGDP2_1lag <- plm::lag(total.df$DNGDP2, 1)

summary(lm(growth ~ growth_1lag, data = total.df))
summary(lm(growth ~ DNGDP2 + growth_1lag, data = total.df))



colnames(total.df)
summary(lm(growth ~ fed_sentiment_std, data = total.df))
summary(lm(growth ~ nyt_sentiment_std, data = total.df))
summary(lm(growth ~ fed_sentiment_std + nyt_sentiment_std, data = total.df))
summary(lm(growth ~ fed_sentiment_std + DNGDP2 + growth_1lag, data = total.df))
summary(lm(growth ~ nyt_sentiment_std + DNGDP2 + growth_1lag, data = total.df))
summary(lm(growth ~ fed_sentiment_std + nyt_sentiment_std + DNGDP2 + growth_1lag, data = total.df))


model1 <- lm(growth ~ DNGDP2 + growth_1lag, data = total.df)
model2 <- lm(growth ~ fed_sentiment_std + DNGDP2 + growth_1lag, data = total.df)
model3 <- lm(growth ~ nyt_sentiment_std + DNGDP2 + growth_1lag, data = total.df)
model4 <- lm(growth ~ fed_sentiment_std + nyt_sentiment_std + DNGDP2 + growth_1lag, data = total.df)

stargazer(model1, model2, model3, model4)







model.df <- select(total.df[which(complete.cases(total.df)),], -inflation, -quarter, -T30, -T30_news, 
                   -fed_sentiment, -nyt_sentiment, -fed_sentiment_std, -nyt_sentiment_std, -ind,
                   -period, -YEAR, -QUARTER, -DNGDP3, -DNGDP4, -DNGDP5, -DNGDP6, -growth_1lag)
colnames(model.df)

noout.df <- model.df[which(model.df$growth > -8),]


#separate training and test sets
set.seed(1)
train_ind <- train_ind <- sample(seq_len(nrow(model.df)), size = 70)

trainset <- model.df[train_ind,]
testset <- model.df[-train_ind,]


# Estimate a linear model over all topics from both sources
glm_model <- glm(growth~.,data = trainset, family = gaussian)
summary(glm_model)
simple_model <- glm(growth~DNGDP2,data = trainset, family = gaussian)
summary(simple_model)



#predict probabilities on testset
#type=”response” gives probabilities, type=”class” gives class
glm_predict <- predict.glm(glm_model,testset,type="response")
simple_predict <- predict.glm(simple_model,testset,type="response")


# calculate the out of sample MSE
mean(glm_predict - testset$growth)
mean((glm_predict - testset$growth)^2)
mean((simple_predict - testset$growth)^2)


#accuracy
mean(round(glm_predict, 0)==round(testset$growth, 0))

### alpha controls the "mix" of lasso and ridge, where 1 is pure lasso.

# convert the training data and class into a matrix and numerical variable
x <- model.matrix(growth~.,trainset)
y <- trainset$growth


#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
cv.out <- cv.glmnet(x,y,alpha=1,family="gaussian",type.measure = "mse" )

#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)
coef(cv.out,s=lambda_min)

#get test data
x_test <- model.matrix(growth~.,testset)
#predict class, type="class"
lasso_predict <- predict(cv.out,newx = x_test,s=lambda_min,type="response")


### Compare the MSE of the LASSO and the GLM
mean((lasso_predict - testset$growth)^2)
mean((glm_predict - testset$growth)^2)
mean((simple_predict - testset$growth)^2)
mean((mean(testset$growth) - testset$growth)^2)




### Estimate the LASSO model on the whole sample

glm_model <- glm(growth~.,data = model.df, family = gaussian)
summary(glm_model)

#predict probabilities on testset
#type=”response” gives probabilities, type=”class” gives class
glm_predict <- predict.glm(glm_model,model.df,type="response")

# calculate the in sample MSE
mean(glm_predict - model.df$growth)
mean((glm_predict - model.df$growth)^2)

#accuracy
mean(round(glm_predict, 0)==round(model.df$growth, 0))

### alpha controls the "mix" of lasso and ridge, where 1 is pure lasso.

# convert the training data and class into a matrix and numerical variable
x <- model.matrix(growth~.,model.df)
y <- model.df$growth


#perform grid search to find optimal value of lambda
cv.out <- cv.glmnet(x,y,alpha=1,family="gaussian",type.measure = "mse" )

#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)
coef(cv.out,s=lambda_min)

stargazer(as.matrix(cbind(coef(cv.out,s=lambda_1se), coef(cv.out,s=lambda_min))))

#get test data
x_test <- model.matrix(growth~.,model.df)
#predict class, type="class"
lasso_predict <- predict(cv.out,newx = x_test,s=lambda_min,type="response")


### Compare the MSE of the LASSO and the GLM
mean((lasso_predict - model.df$growth)^2)
mean((glm_predict - model.df$growth)^2)
mean((mean(testset$growth) - model.df$growth)^2)










################################ Now do the exact same thing for inflation ################################ 



model.df <- select(total.df, -growth, -quarter, -T30, -T30_news, -fed_sentiment_std, -nyt_sentiment_std)
colnames(model.df)

#separate training and test sets
train_ind <- train_ind <- sample(seq_len(nrow(model.df)), size = 70)

trainset <- model.df[train_ind,]
testset <- model.df[-train_ind,]


# Estimate a linear model over all topics from both sources
glm_model <- glm(inflation~.,data = trainset, family = gaussian)
summary(glm_model)

#predict probabilities on testset
#type=”response” gives probabilities, type=”class” gives class
glm_predict <- predict.glm(glm_model,testset,type="response")

# calculate the out of sample MSE
mean(glm_predict - testset$inflation)
mean((glm_predict - testset$inflation)^2)

#accuracy
mean(round(glm_predict, 0)==round(testset$inflation, 0))

### alpha controls the "mix" of lasso and ridge, where 1 is pure lasso.

# convert the training data and class into a matrix and numerical variable
x <- model.matrix(inflation~.,trainset)
y <- trainset$inflation


#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
cv.out <- cv.glmnet(x,y,alpha=1,family="gaussian",type.measure = "mse" )

#plot result
plot(cv.out, xlab = "Number of parameters" , ylab = "lambda")

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)
coef(cv.out,s=lambda_min)

#get test data
x_test <- model.matrix(inflation~.,testset)
#predict class, type="class"
lasso_predict <- predict(cv.out,newx = x_test,s=lambda_min,type="response")


### Compare the MSE of the LASSO and the GLM
mean((lasso_predict - testset$inflation)^2)
mean((glm_predict - testset$inflation)^2)
mean((mean(testset$inflation) - testset$inflation)^2)









