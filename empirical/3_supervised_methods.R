####
# This file illustrates supervised topic model on FOMC minutes
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())


require(ggplot2)
require(forcats)
require(glmnet)
require(lda)



############### Import data ############### 
# GDP growth 
gdp_df <- read.csv("data/GDPC1.csv", stringsAsFactors = FALSE) %>%
  rename(quarter = DATE, gdp = GDPC1) %>%
  mutate(gdp_lag = lag(gdp), 
         growth = 100*(log(gdp) - log(gdp_lag)),
         quarter = as.Date(quarter))
# Minutes
fedminutes_df <- read.csv("data/fedminutes_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(quarter = floor_date(as.Date(date), unit = "quarter")) %>%
  left_join(gdp_df) %>%
  filter(quarter >= "2000-01-01" & quarter < "2020-01-01")


############### lasso on term counts ############### 
corpus <- Corpus(VectorSource(unlist(fedminutes_df$paragraph_clean)))
mins_dtm <- DocumentTermMatrix(corpus, control = list(minsWordLength = 3))
mins_dtm$dimnames$Docs <- fedminutes_df$unique_id
mins_tf <- as.matrix(mins_dtm)[,which(colSums(as.matrix(mins_dtm)) > 20)]


lasso_model <- glmnet(mins_tf, fedminutes_df$growth,
                family="gaussian", intercept = T, alpha=1, 
                standardize = TRUE)
# Plot coefficients as lambda increases
plot(lasso_model)

coeffs <- coef(lasso_model, s = 0.1) 
coeffs_df <- data.frame(name = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x) 



############### supervised LDA ############### 

# Create DTM in correct form for slda package
docs <- lexicalize(fedminutes_df$paragraph_clean)
docs$vocab
docs$documents[[10]]


train_labels <- fedminutes_df$growth

# Train model
K <- 20
growth_slda <- slda.em(documents=docs$documents, K=K, vocab=docs$vocab,
                       num.e.iterations=200, num.m.iterations=50,
                       alpha=0.5, eta=0.01, train_labels, rep(c(0), K), variance=0.1,
                       logistic=FALSE, method="sLDA")
if (FALSE){
  saveRDS(growth_slda, file = "data/mins_slda_20.rds")
} else {
  growth_slda <- readRDS(file = "data/mins_slda_20.rds")
}
# Extract topics and coefficients
Topics <- apply(top.topic.words(growth_slda$topics, 10, by.score=TRUE),
                2, paste, collapse=" ")
coefs <- data.frame(coef(summary(growth_slda$model)))
coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
coefs <- coefs[order(coefs$Estimate),]

# Plot the coefficients
ggplot(coefs,aes(x = Estimate, y = Topics)) + theme_bw() + 
  geom_point() + 
  geom_errorbar(width=0.5, aes(xmin= Estimate - Std..Error,
                               xmax= Estimate + Std..Error)) 






