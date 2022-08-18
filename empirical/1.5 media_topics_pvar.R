####
# This file estimates panel regressions on the Fed minutes NYT articles combined/joint topics.
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
require(panelvar)
require(scales)


### Define the directories where raw data is stored and clean will be saved
raw_dir <- "~/Documents/DPhil/Raw_Data/"
clean_dir <- "~/Documents/DPhil/Clean_Data/"
export_dir <- "~/Documents/DPhil/central_bank_communication/figures/"


import_filename = "data/jointmedia_spf_gb_panel.csv"
import_filename <- "data/topics_forecasts_panel.csv"
total.df <- read.csv(import_filename, encoding = "utf-8", stringsAsFactors = FALSE)

total.panel <- pdata.frame(total.df, index = c("variable", "period"))


## Panel VAR
var_std <- pvarfeols(
  dependent_vars = c("disp_std","news_std","mins_std"),
  lags = 3,
  #exog_vars,
  transformation = c("fod"),
  data = total.df,
  panel_identifier = c("variable", "period")
)


summary(var_std)
stab_varone <- stability(var_std)
print(stab_varone)
plot(stab_varone)

varone_oirf <- oirf(var_std, n.ahead = 12)
plot(varone_oirf)
varone_girf <- girf(var_std, n.ahead = 12, ma_approx_steps = 12)
plot(varone_girf)

#### Manually bootstrap CIs
## Options
individuals <- unique(total.df$variable)
ndraws <- 5000
n_ahead <- 12
nlags <- 3
## Store results
oirf_draws <- data.frame(draw = 1:ndraws)
# Dispersion shock
oirf_draws[,c(paste0("disp_on_disp_", 1:n_ahead),paste0("disp_on_news_", 1:n_ahead),
              paste0("disp_on_fed_", 1:n_ahead))] <- 0
# News shock
oirf_draws[,c(paste0("news_on_disp_", 1:n_ahead),paste0("news_on_news_", 1:n_ahead),
              paste0("news_on_fed_", 1:n_ahead))] <- 0
# FOMC shock
oirf_draws[,c(paste0("fed_on_disp_", 1:n_ahead),paste0("fed_on_news_", 1:n_ahead),
              paste0("fed_on_fed_", 1:n_ahead))] <- 0
girf_draws <- oirf_draws

pb = txtProgressBar(min = 1, max = ndraws, initial = 1) 
for (nn in 1:ndraws){
  setTxtProgressBar(pb,nn)
  # Blockwise sample individuals
  sub_inds <- sample(individuals, length(individuals), replace = TRUE)
  sub.df <- total.df[which(total.df$variable == sub_inds[1]),]
  for (si in sub_inds[2:length(sub_inds)]){
    sub.df <- rbind(sub.df, total.df[which(total.df$variable == si),])
  }
  # Estimate VAR by OLS
  var_sub <- pvarfeols(
    dependent_vars = c("disp_std","news_std","mins_std"),
    lags = nlags,
    #exog_vars,
    transformation = c("fod"),
    data = sub.df,
    panel_identifier = c("variable", "period")
  )
  # Compute IRFs
  varsub_oirf <- oirf(var_sub, n.ahead = n_ahead)
  varsub_girf <- girf(var_sub, n.ahead = n_ahead, ma_approx_steps = n_ahead)
  
  # Save OIRF results for disp shock
  oirf_draws[nn,paste0("disp_on_disp_", 1:n_ahead)] <- varsub_oirf$disp_std[,"disp_std"]
  oirf_draws[nn,paste0("disp_on_news_", 1:n_ahead)] <- varsub_oirf$disp_std[,"news_std"]
  oirf_draws[nn,paste0("disp_on_fed_", 1:n_ahead)] <- varsub_oirf$disp_std[,"mins_std"]
  # Save OIRF results for news shock
  oirf_draws[nn,paste0("news_on_disp_", 1:n_ahead)] <- varsub_oirf$news_std[,"disp_std"]
  oirf_draws[nn,paste0("news_on_news_", 1:n_ahead)] <- varsub_oirf$news_std[,"news_std"]
  oirf_draws[nn,paste0("news_on_fed_", 1:n_ahead)] <- varsub_oirf$news_std[,"mins_std"]
  # Save OIRF results for fed shock
  oirf_draws[nn,paste0("fed_on_disp_", 1:n_ahead)] <- varsub_oirf$mins_std[,"disp_std"]
  oirf_draws[nn,paste0("fed_on_news_", 1:n_ahead)] <- varsub_oirf$mins_std[,"news_std"]
  oirf_draws[nn,paste0("fed_on_fed_", 1:n_ahead)] <- varsub_oirf$mins_std[,"mins_std"]
  
  # Save GIRF results for disp shock
  girf_draws[nn,paste0("disp_on_disp_", 1:n_ahead)] <- varsub_girf$disp_std[,"disp_std"]
  girf_draws[nn,paste0("disp_on_news_", 1:n_ahead)] <- varsub_girf$disp_std[,"news_std"]
  girf_draws[nn,paste0("disp_on_fed_", 1:n_ahead)] <- varsub_girf$disp_std[,"mins_std"]
  # Save OIRF results for news shock
  girf_draws[nn,paste0("news_on_disp_", 1:n_ahead)] <- varsub_girf$news_std[,"disp_std"]
  girf_draws[nn,paste0("news_on_news_", 1:n_ahead)] <- varsub_girf$news_std[,"news_std"]
  girf_draws[nn,paste0("news_on_fed_", 1:n_ahead)] <- varsub_girf$news_std[,"mins_std"]
  # Save OIRF results for fed shock
  girf_draws[nn,paste0("fed_on_disp_", 1:n_ahead)] <- varsub_girf$mins_std[,"disp_std"]
  girf_draws[nn,paste0("fed_on_news_", 1:n_ahead)] <- varsub_girf$mins_std[,"news_std"]
  girf_draws[nn,paste0("fed_on_fed_", 1:n_ahead)] <- varsub_girf$mins_std[,"mins_std"]

}

### Plot IRFs with bootstrapped CIs
varobj <- varone_oirf 
btstrpobj <- oirf_draws
name <- "disp_on_fed_"
create_btsrp_df <- function(n_ahead,btstrpobj,name){
  btstrp_df <- data.frame(period = 0:(n_ahead-1))
  btstrp_df[,c("quant_0p025", "quant_0p975", "quant_0p5", "quant_0p15", "quant_0p85")] <- 0
  for (ll in 1:n_ahead){
    btstrp_dist <- btstrpobj[,paste0(name, ll)]
    btstrp_df$quant_0p025[ll] <- quantile(btstrp_dist, probs = c(0.025))
    btstrp_df$quant_0p975[ll] <- quantile(btstrp_dist, probs = c(0.975))
    btstrp_df$quant_0p15[ll] <- quantile(btstrp_dist, probs = c(0.15))
    btstrp_df$quant_0p85[ll] <- quantile(btstrp_dist, probs = c(0.85))
    btstrp_df$quant_0p5[ll] <- quantile(btstrp_dist, probs = c(0.5))
  }
  return(btstrp_df)
}

plt_btstrp <- function(btstrp_df){
  p1 <- ggplot(btstrp_df,aes(x=period)) + geom_line(aes(y=quant_0p5)) + theme_bw() +
    xlab("Periods") + ylab("Response") + geom_hline(yintercept = 0, linetype = 2) + 
    geom_ribbon(aes(ymin = quant_0p025, ymax = quant_0p975), fill= "black", alpha = 0.2) +
    geom_ribbon(aes(ymin = quant_0p15, ymax = quant_0p85), fill = "black", alpha = 0.4) +
    scale_x_continuous(breaks= pretty_breaks())
  return(p1)
}

#### Plot and save all the OIRFs
# Dispersion
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("SPF dispersion on SPF dispersion")
ggsave("figures/IRFs/disp/spf_spf.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_news_")
plt_btstrp(btstrp_df) + ggtitle("SPF dispersion on NYT focus")
ggsave("figures/IRFs/disp/spf_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("SPF dispersion on FOMC minutes focus")
ggsave("figures/IRFs/disp/spf_fomc.pdf", width = 4, height = 3)
# News
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on SPF dispersion")
ggsave("figures/IRFs/disp/nyt_spf.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_news_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on NYT focus")
ggsave("figures/IRFs/disp/nyt_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on FOMC minutes focus")
ggsave("figures/IRFs/disp/nyt_fomc.pdf", width = 4, height = 3)
# Fed
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on SPF dispersion")
ggsave("figures/IRFs/disp/fomc_spf.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_news_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on NYT focus")
ggsave("figures/IRFs/disp/fomc_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on FOMC minutes focus")
ggsave("figures/IRFs/disp/fomc_fomc.pdf", width = 4, height = 3)




############################# GB updates ############################# 


individuals <- unique(total.df$variable)
ndraws <- 5000
n_ahead <- 12
nlags <- 3
## Store results
oirf_draws <- data.frame(draw = 1:ndraws)
# Dispersion shock
oirf_draws[,c(paste0("disp_on_disp_", 1:n_ahead),paste0("disp_on_news_", 1:n_ahead),
              paste0("disp_on_fed_", 1:n_ahead))] <- 0
# News shock
oirf_draws[,c(paste0("news_on_disp_", 1:n_ahead),paste0("news_on_news_", 1:n_ahead),
              paste0("news_on_fed_", 1:n_ahead))] <- 0
# FOMC shock
oirf_draws[,c(paste0("fed_on_disp_", 1:n_ahead),paste0("fed_on_news_", 1:n_ahead),
              paste0("fed_on_fed_", 1:n_ahead))] <- 0
girf_draws <- oirf_draws

pb = txtProgressBar(min = 1, max = ndraws, initial = 1) 
for (nn in 1:ndraws){
  setTxtProgressBar(pb,nn)
  # Blockwise sample individuals
  sub_inds <- sample(individuals, length(individuals), replace = TRUE)
  sub.df <- total.df[which(total.df$variable == sub_inds[1]),]
  for (si in sub_inds[2:length(sub_inds)]){
    sub.df <- rbind(sub.df, total.df[which(total.df$variable == si),])
  }
  sub.df$disp_std <- sub.df$GB_update_abs_std
  # Estimate VAR by OLS
  var_sub <- pvarfeols(
    dependent_vars = c("disp_std","news_std","mins_std"),
    lags = nlags,
    #exog_vars,
    transformation = c("fod"),
    data = sub.df,
    panel_identifier = c("variable", "period")
  )
  # Compute IRFs
  varsub_oirf <- oirf(var_sub, n.ahead = n_ahead)
  varsub_girf <- girf(var_sub, n.ahead = n_ahead, ma_approx_steps = n_ahead)
  
  # Save OIRF results for disp shock
  oirf_draws[nn,paste0("disp_on_disp_", 1:n_ahead)] <- varsub_oirf$disp_std[,"disp_std"]
  oirf_draws[nn,paste0("disp_on_news_", 1:n_ahead)] <- varsub_oirf$disp_std[,"news_std"]
  oirf_draws[nn,paste0("disp_on_fed_", 1:n_ahead)] <- varsub_oirf$disp_std[,"mins_std"]
  # Save OIRF results for news shock
  oirf_draws[nn,paste0("news_on_disp_", 1:n_ahead)] <- varsub_oirf$news_std[,"disp_std"]
  oirf_draws[nn,paste0("news_on_news_", 1:n_ahead)] <- varsub_oirf$news_std[,"news_std"]
  oirf_draws[nn,paste0("news_on_fed_", 1:n_ahead)] <- varsub_oirf$news_std[,"mins_std"]
  # Save OIRF results for fed shock
  oirf_draws[nn,paste0("fed_on_disp_", 1:n_ahead)] <- varsub_oirf$mins_std[,"disp_std"]
  oirf_draws[nn,paste0("fed_on_news_", 1:n_ahead)] <- varsub_oirf$mins_std[,"news_std"]
  oirf_draws[nn,paste0("fed_on_fed_", 1:n_ahead)] <- varsub_oirf$mins_std[,"mins_std"]
  
  # Save GIRF results for disp shock
  girf_draws[nn,paste0("disp_on_disp_", 1:n_ahead)] <- varsub_girf$disp_std[,"disp_std"]
  girf_draws[nn,paste0("disp_on_news_", 1:n_ahead)] <- varsub_girf$disp_std[,"news_std"]
  girf_draws[nn,paste0("disp_on_fed_", 1:n_ahead)] <- varsub_girf$disp_std[,"mins_std"]
  # Save OIRF results for news shock
  girf_draws[nn,paste0("news_on_disp_", 1:n_ahead)] <- varsub_girf$news_std[,"disp_std"]
  girf_draws[nn,paste0("news_on_news_", 1:n_ahead)] <- varsub_girf$news_std[,"news_std"]
  girf_draws[nn,paste0("news_on_fed_", 1:n_ahead)] <- varsub_girf$news_std[,"mins_std"]
  # Save OIRF results for fed shock
  girf_draws[nn,paste0("fed_on_disp_", 1:n_ahead)] <- varsub_girf$mins_std[,"disp_std"]
  girf_draws[nn,paste0("fed_on_news_", 1:n_ahead)] <- varsub_girf$mins_std[,"news_std"]
  girf_draws[nn,paste0("fed_on_fed_", 1:n_ahead)] <- varsub_girf$mins_std[,"mins_std"]
  
}
beep()
### Plot IRFs with bootstrapped CIs
varobj <- varone_oirf 
btstrpobj <- oirf_draws
name <- "disp_on_fed_"
create_btsrp_df <- function(n_ahead,btstrpobj,name){
  btstrp_df <- data.frame(period = 0:(n_ahead-1))
  btstrp_df[,c("quant_0p025", "quant_0p975", "quant_0p5", "quant_0p15", "quant_0p85")] <- 0
  for (ll in 1:n_ahead){
    btstrp_dist <- btstrpobj[,paste0(name, ll)]
    btstrp_df$quant_0p025[ll] <- quantile(btstrp_dist, probs = c(0.025))
    btstrp_df$quant_0p975[ll] <- quantile(btstrp_dist, probs = c(0.975))
    btstrp_df$quant_0p15[ll] <- quantile(btstrp_dist, probs = c(0.15))
    btstrp_df$quant_0p85[ll] <- quantile(btstrp_dist, probs = c(0.85))
    btstrp_df$quant_0p5[ll] <- quantile(btstrp_dist, probs = c(0.5))
  }
  return(btstrp_df)
}

plt_btstrp <- function(btstrp_df){
  p1 <- ggplot(btstrp_df,aes(x=period)) + geom_line(aes(y=quant_0p5)) + theme_bw() +
    xlab("Periods") + ylab("Response") + geom_hline(yintercept = 0, linetype = 2) + 
    geom_ribbon(aes(ymin = quant_0p025, ymax = quant_0p975), fill= "black", alpha = 0.2) +
    geom_ribbon(aes(ymin = quant_0p15, ymax = quant_0p85), fill = "black", alpha = 0.4) +
    scale_x_continuous(breaks= pretty_breaks())
  return(p1)
}

#### Plot and save all the OIRFs
# Dispersion
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("Tealbook update on Tealbook update")
ggsave("figures/IRFs/GB_up/GBup_GBup.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_news_")
plt_btstrp(btstrp_df) + ggtitle("Tealbook update on NYT focus")
ggsave("figures/IRFs/GB_up/GBup_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("Tealbook update on FOMC minutes focus")
ggsave("figures/IRFs/GB_up/GBup_fomc.pdf", width = 4, height = 3)
# News
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on Tealbook update")
ggsave("figures/IRFs/GB_up/nyt_GBup.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_news_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on NYT focus")
ggsave("figures/IRFs/GB_up/nyt_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on FOMC minutes focus")
ggsave("figures/IRFs/GB_up/nyt_fomc.pdf", width = 4, height = 3)
# Fed
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on Tealbook update")
ggsave("figures/IRFs/GB_up/fomc_GBup.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_news_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on NYT focus")
ggsave("figures/IRFs/GB_up/fomc_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on FOMC minutes focus")
ggsave("figures/IRFs/GB_up/fomc_fomc.pdf", width = 4, height = 3)



















############################# GB error ############################# 


individuals <- unique(total.df$variable)
ndraws <- 5000
n_ahead <- 12
nlags <- 3
## Store results
oirf_draws <- data.frame(draw = 1:ndraws)
# Dispersion shock
oirf_draws[,c(paste0("disp_on_disp_", 1:n_ahead),paste0("disp_on_news_", 1:n_ahead),
              paste0("disp_on_fed_", 1:n_ahead))] <- 0
# News shock
oirf_draws[,c(paste0("news_on_disp_", 1:n_ahead),paste0("news_on_news_", 1:n_ahead),
              paste0("news_on_fed_", 1:n_ahead))] <- 0
# FOMC shock
oirf_draws[,c(paste0("fed_on_disp_", 1:n_ahead),paste0("fed_on_news_", 1:n_ahead),
              paste0("fed_on_fed_", 1:n_ahead))] <- 0
girf_draws <- oirf_draws

pb = txtProgressBar(min = 1, max = ndraws, initial = 1) 
for (nn in 1:ndraws){
  setTxtProgressBar(pb,nn)
  # Blockwise sample individuals
  sub_inds <- sample(individuals, length(individuals), replace = TRUE)
  sub.df <- total.df[which(total.df$variable == sub_inds[1]),]
  for (si in sub_inds[2:length(sub_inds)]){
    sub.df <- rbind(sub.df, total.df[which(total.df$variable == si),])
  }
  sub.df$disp_std <- sub.df$GB_SPF_error_diff_std
  # Estimate VAR by OLS
  var_sub <- pvarfeols(
    dependent_vars = c("disp_std","news_std","mins_std"),
    lags = nlags,
    #exog_vars,
    transformation = c("fod"),
    data = sub.df,
    panel_identifier = c("variable", "period")
  )
  # Compute IRFs
  varsub_oirf <- oirf(var_sub, n.ahead = n_ahead)
  varsub_girf <- girf(var_sub, n.ahead = n_ahead, ma_approx_steps = n_ahead)
  
  # Save OIRF results for disp shock
  oirf_draws[nn,paste0("disp_on_disp_", 1:n_ahead)] <- varsub_oirf$disp_std[,"disp_std"]
  oirf_draws[nn,paste0("disp_on_news_", 1:n_ahead)] <- varsub_oirf$disp_std[,"news_std"]
  oirf_draws[nn,paste0("disp_on_fed_", 1:n_ahead)] <- varsub_oirf$disp_std[,"mins_std"]
  # Save OIRF results for news shock
  oirf_draws[nn,paste0("news_on_disp_", 1:n_ahead)] <- varsub_oirf$news_std[,"disp_std"]
  oirf_draws[nn,paste0("news_on_news_", 1:n_ahead)] <- varsub_oirf$news_std[,"news_std"]
  oirf_draws[nn,paste0("news_on_fed_", 1:n_ahead)] <- varsub_oirf$news_std[,"mins_std"]
  # Save OIRF results for fed shock
  oirf_draws[nn,paste0("fed_on_disp_", 1:n_ahead)] <- varsub_oirf$mins_std[,"disp_std"]
  oirf_draws[nn,paste0("fed_on_news_", 1:n_ahead)] <- varsub_oirf$mins_std[,"news_std"]
  oirf_draws[nn,paste0("fed_on_fed_", 1:n_ahead)] <- varsub_oirf$mins_std[,"mins_std"]
  
  # Save GIRF results for disp shock
  girf_draws[nn,paste0("disp_on_disp_", 1:n_ahead)] <- varsub_girf$disp_std[,"disp_std"]
  girf_draws[nn,paste0("disp_on_news_", 1:n_ahead)] <- varsub_girf$disp_std[,"news_std"]
  girf_draws[nn,paste0("disp_on_fed_", 1:n_ahead)] <- varsub_girf$disp_std[,"mins_std"]
  # Save OIRF results for news shock
  girf_draws[nn,paste0("news_on_disp_", 1:n_ahead)] <- varsub_girf$news_std[,"disp_std"]
  girf_draws[nn,paste0("news_on_news_", 1:n_ahead)] <- varsub_girf$news_std[,"news_std"]
  girf_draws[nn,paste0("news_on_fed_", 1:n_ahead)] <- varsub_girf$news_std[,"mins_std"]
  # Save OIRF results for fed shock
  girf_draws[nn,paste0("fed_on_disp_", 1:n_ahead)] <- varsub_girf$mins_std[,"disp_std"]
  girf_draws[nn,paste0("fed_on_news_", 1:n_ahead)] <- varsub_girf$mins_std[,"news_std"]
  girf_draws[nn,paste0("fed_on_fed_", 1:n_ahead)] <- varsub_girf$mins_std[,"mins_std"]
  
}
beep()
### Plot IRFs with bootstrapped CIs
varobj <- varone_oirf 
btstrpobj <- oirf_draws
name <- "disp_on_fed_"
create_btsrp_df <- function(n_ahead,btstrpobj,name){
  btstrp_df <- data.frame(period = 0:(n_ahead-1))
  btstrp_df[,c("quant_0p025", "quant_0p975", "quant_0p5", "quant_0p15", "quant_0p85")] <- 0
  for (ll in 1:n_ahead){
    btstrp_dist <- btstrpobj[,paste0(name, ll)]
    btstrp_df$quant_0p025[ll] <- quantile(btstrp_dist, probs = c(0.025))
    btstrp_df$quant_0p975[ll] <- quantile(btstrp_dist, probs = c(0.975))
    btstrp_df$quant_0p15[ll] <- quantile(btstrp_dist, probs = c(0.15))
    btstrp_df$quant_0p85[ll] <- quantile(btstrp_dist, probs = c(0.85))
    btstrp_df$quant_0p5[ll] <- quantile(btstrp_dist, probs = c(0.5))
  }
  return(btstrp_df)
}

plt_btstrp <- function(btstrp_df){
  p1 <- ggplot(btstrp_df,aes(x=period)) + geom_line(aes(y=quant_0p5)) + theme_bw() +
    xlab("Periods") + ylab("Response") + geom_hline(yintercept = 0, linetype = 2) + 
    geom_ribbon(aes(ymin = quant_0p025, ymax = quant_0p975), fill= "black", alpha = 0.2) +
    geom_ribbon(aes(ymin = quant_0p15, ymax = quant_0p85), fill = "black", alpha = 0.4) +
    scale_x_continuous(breaks= pretty_breaks())
  return(p1)
}

#### Plot and save all the OIRFs
# Dispersion
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("TB-SPF error diff on TB-SPF error diff")
ggsave("figures/IRFs/GB_errdiff/GBerr_GBerr.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_news_")
plt_btstrp(btstrp_df) + ggtitle("TB-SPF error diff on NYT focus")
ggsave("figures/IRFs/GB_errdiff/GBerr_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"disp_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("TB-SPF error diff on FOMC minutes focus")
ggsave("figures/IRFs/GB_errdiff/GBerr_fomc.pdf", width = 4, height = 3)
# News
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on TB-SPF error diff")
ggsave("figures/IRFs/GB_errdiff/nyt_GBerr.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_news_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on NYT focus")
ggsave("figures/IRFs/GB_errdiff/nyt_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"news_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("NYT focus on FOMC minutes focus")
ggsave("figures/IRFs/GB_errdiff/nyt_fomc.pdf", width = 4, height = 3)
# Fed
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_disp_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on TB-SPF error diff")
ggsave("figures/IRFs/GB_errdiff/fomc_GBerr.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_news_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on NYT focus")
ggsave("figures/IRFs/GB_errdiff/fomc_nyt.pdf", width = 4, height = 3)
btstrp_df <- create_btsrp_df(n_ahead,oirf_draws,"fed_on_fed_")
plt_btstrp(btstrp_df) + ggtitle("FOMC minutes focus on FOMC minutes focus")
ggsave("figures/IRFs/GB_errdiff/fomc_fomc.pdf", width = 4, height = 3)










############################# End ############################# 