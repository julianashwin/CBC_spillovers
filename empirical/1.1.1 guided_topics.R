setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(quanteda)
require(seededlda)

dict <- dictionary(file = "tests/data/topics.yml")
print(dict)

### Define the directories where raw data is stored and clean will be saved
import_dir <- "data/clean_text/"
export_dir <- "data/topic_data/"
fig_dir <- "figures/fed_media_topics/"


### Import the text data
clean_filename = paste0(import_dir, "fedminutes_clean.csv")
fedminutes_df <- read.csv(clean_filename, encoding = "utf-8", stringsAsFactors = FALSE)

rownames(fedminutes_df) <- fedminutes_df$unique_id
corp_mins <- corpus(fedminutes_df, text_field = "paragraph_clean")
toks_mins <- tokens(corp_mins, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
dfm_mins <- dfm(toks_mins)

total_tf <- data.frame(colnames(dfm_mins), tf = colSums(dfm_mins))


dict_mins <- dictionary(list(inflation = c("price", "inflat"),
                        growth = c("economi", "growth", "recess", "recoveri"),
                        employment = c("job", "worker", "unemploy", "employ", "labor"),
                        rates = c("interest", "bond", "fed", "rate"),
                        profits = c("profit", "compani", "busi"),
                        indprod = c("industri", "product", "manufactur", "factori"),
                        housing = c("mortgage", "house", "hous", "home", "residenti"),
                        nresinv = c("nonresidenti","invest", "investor", "capit","expenditur"),
                        consum = c("retail", "consum", "household", "sale"),
                        fedgov = c("tax", "budget", "govern")
                        ))

set.seed(1234)
slda <- textmodel_seededlda(dfm_mins, dict_mins, residual = 9, max_iter = 6000)
print(terms(slda, 20))


dict <- dictionary(list(economy = c("market*", "money", "bank*", "stock*", "bond*", "industry", 
                                    "company", "shop*"),
                        politics = c("parliament*", "congress*", "white house", "party leader*", 
                                     "party member*", "voter*", "lawmaker*", "politician*"),
                        society = c("police", "prison*", "school*", "hospital*"),
                        diplomacy = c("ambassador*", "diplomat*", "embassy", "treaty"),
                        military = c("military", "soldier*", "terrorist*", "air force", "marine", 
                                     "navy", "army")))

corp <- readRDS("tests/data/data_corpus_sputnik.RDS")
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) %>%
  tokens_select(min_nchar = 2) %>% 
  tokens_compound(dict) # for multi-word expressions
dfmt <- dfm(toks) %>% 
  dfm_remove(stopwords('en')) %>% 
  dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
           max_docfreq = 0.2, docfreq_type = "prop")

set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE)
print(terms(slda, 20))