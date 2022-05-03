setwd("~/Documents/GitHub/CBC_spillovers")
rm(list = ls())

require(panelvar)
require(lfe)
require(plm)


### Define the directories where raw data is stored and clean will be saved
raw_dir <- "~/Documents/DPhil/Raw_Data/"
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


### Import the SPF-text data panel

import_filename <- "data/jointmedia_spf_gb_panel.csv"
total.panel <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
total.panel <- pdata.frame(total.panel, index = c("series", "period"))


### Import the media and minutes panel

import_filename = paste(clean_dir, "CBC/mediaminutes_panel.csv", sep = "/")
full_panel.df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)
#full_panel.df <- pdata.frame(full_panel.df, index = c("topic", "meet_date"))

full_panel.df$id_no <- as.numeric(as.factor(full_panel.df$topic))
full_panel.df$time_no <- as.numeric(as.factor(full_panel.df$meet_date))
full_panel.df <- pdata.frame(full_panel.df, index = c("id_no", "time_no"))
full_panel.df$id_no <- as.numeric((full_panel.df$topic))
full_panel.df$time_no <- as.numeric((full_panel.df$meet_date))


half_panel.df <- full_panel.df[which(full_panel.df$time_no > 70),]


### Do media articles predict the minutes?
### First use the pre meeting articles and the minutes, so the we can restrict that the minutes don't affect the 
### articles contemporaneously
model <- felm(meeting_value ~ prearticle_value + plm::lag(meeting_value, 1:3) | topic, data = full_panel.df)
summary(model)
model <- felm(meeting_value ~ prearticle_value + plm::lag(meeting_value, 1:3) | topic, data = half_panel.df)
summary(model)



### Order so that the prearticle is top as it doesn't respond to the minutes
prearticle_model <- 
  pvarfeols(dependent_vars = c("prearticle_value", "meeting_value"),
            lags = 3,
            transformation = "demean",
            data = full_panel.df,
            panel_identifier= c("id_no", "time_no"))
summary(prearticle_model)
prearticle_model <- 
  pvarfeols(dependent_vars = c("prearticle_value", "meeting_value"),
            lags = 3,
            transformation = "demean",
            data = half_panel.df,
            panel_identifier= c("id_no", "time_no"))
summary(prearticle_model)


stab_prearticle_model <- stability(prearticle_model)
print(stab_prearticle_model)
plot(stab_prearticle_model)


prearticle_model_oirf <- oirf(prearticle_model, n.ahead = 8)

prearticle_model_bs <- bootstrap_irf(prearticle_model, typeof_irf = c("OIRF"),
                              n.ahead = 8, 
                              nof_Nstar_draws = 10,
                              confidence.band = 0.6)


plot(prearticle_model_oirf)



### Do minutes predict media articles?
### Now use the post publication articles and the minutes, so the we can restrict that the articles don't affect the 
### minutes contemporaneously
model <- felm(postarticle_value ~ meeting_value + plm::lag(postarticle_value, 1:3) | topic, data = full_panel.df)
summary(model)
model <- felm(postarticle_value ~ meeting_value + plm::lag(postarticle_value, 1:3) | topic, data = half_panel.df)
summary(model)



### Order so that the prearticle is top as it doesn't respond to the minutes
postarticle_model <- 
  pvarfeols(dependent_vars = c("meeting_value", "postarticle_value"),
            lags = 2,
            transformation = "demean",
            data = half_panel.df,
            panel_identifier= c("id_no", "time_no"))
summary(postarticle_model)

stab_postarticle_model <- stability(postarticle_model)
print(stab_postarticle_model)
plot(stab_postarticle_model)

postarticle_model_oirf <- oirf(postarticle_model, n.ahead = 8)
postarticle_model_girf <- girf(postarticle_model, n.ahead = 8, ma_approx_steps = 8)

plot(postarticle_model_oirf)
plot(postarticle_model_girf)




postarticle_model_gmm <-  pvargmm(
  dependent_vars = c("meeting_value", "postarticle_value"),
  lags = 3,
  transformation = "fd",
  data = half_panel.df,
  panel_identifier = c("id_no", "time_no"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 15,
  min_instr_dependent_vars = 2L,
  max_instr_predet_vars = 15,
  min_instr_predet_vars = 1L,
  collapse = FALSE)
summary(postarticle_model_gmm)




meeting_long <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops_panel.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
meeting_long <- pdata.frame(meeting_long, index = c("centralbank_topic", "meeting_no"))





############################# Inflation data ############################# 
model <- felm(dispersion ~ plm::lag(fed, 0:1) + plm::lag(news, 0:1) + plm::lag(dispersion, 1:3)
               | series + quarter, data = total.panel)
summary(model)


##
pvar_model <- 
  pvarfeols(dependent_vars = c("fed", "news", "dispersion"),
            lags = 3,
            transformation = "demean",
            data = data.frame(total.panel),
            panel_identifier= c("series", "period"))
summary(pvar_model)





pvar_gmm <- pvargmm(
  dependent_vars = c("fed", "news", ),
  lags = 4,
  predet_vars = c("dispersion"),
  exog_vars = c("news"),
  transformation = "fd",
  data = total.panel,
  panel_identifier = c("series", "quarter"),
  steps = c("twostep"),
  system_instruments = TRUE,
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)
summary(pvar_gmm)

ex1_dahlberg_data <- pvargmm(dependent_vars = c("expenditures", "revenues", "grants"),
                             lags = 1,
                             transformation = "fod",
                             data = Dahlberg,
                             panel_identifier=c("id", "year"),
                             steps = c("twostep"),
                             system_instruments = FALSE,
                             max_instr_dependent_vars = 99,
                             max_instr_predet_vars = 99,
                             min_instr_dependent_vars = 2L,
                             min_instr_predet_vars = 1L,
                             collapse = FALSE
)
summary(ex1_dahlberg_data)




