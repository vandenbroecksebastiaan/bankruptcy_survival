library(dplyr)
library(survival)
library(parfm)
library(frailtySurv)
library(frailtyEM)
library(ggstatsplot)
options(scipen = 999)
rm(list=ls())

# --- Read data
data <- read.csv("data/bankruptcy_transformed.csv")

# The last row can be empty
data <- na.omit(data)

# Events other than bankruptcy are considered censored
data$censor <- ifelse(data$event_indicator == 2, 1, 0)

# Make dummies for the size classification relative to medium sized companies
data$size_small <- ifelse(data$size_classification == "Small", 1, 0)
data$size_large <- ifelse(data$size_classification == "Large", 1, 0)
data$size_v_large <- ifelse(data$size_classification == "Very large", 1, 0)

# Rename
data$EBITDA <- data$EBTIDA
data$EBTIDA <- NULL

# --- Transform variables / remove outliers

# total_liabilities_by_net_assets
# outliers are to be truncated
var_mean <- mean(data$total_liabilities_by_net_assets)
var_std <- sqrt(var(data$total_liabilities_by_net_assets))
var_stand <- (data$total_liabilities_by_net_assets - var_mean) / var_std

var_stand[var_stand > 0.5] <- 0.5
var_stand[var_stand < -0.5] <- -0.5

data$transf_total_liabilities_by_net_assets <- (
  var_stand * var_std + var_mean
)

# cash_by_total_assets
data$log_cash_by_total_assets <- log(data$cash_by_total_assets)
min <- min(
  data$log_cash_by_total_assets[is.finite(data$log_cash_by_total_assets)]
)
data$log_cash_by_total_assets <- replace(
  data$log_cash_by_total_assets, data$log_cash_by_total_assets == -Inf,
  min
)

# cash_by_current_liabilities
data$log_cash_by_current_liabilities <- log(data$cash_by_current_liabilities)
min <- min(
  data$log_cash_by_current_liabilities[is.finite(data$log_cash_by_current_liabilities)]
)
data$log_cash_by_current_liabilities <- replace(
  data$log_cash_by_current_liabilities, data$log_cash_by_current_liabilities == -Inf,
  min
)

# current_assets_by_current_liabilities
data$log_current_assets_by_current_liabilities <- log(data$current_assets_by_current_liabilities)

# tax
# some of the outliers are very large, but still plausible for large firms

# financial_expenses_by_total_assets
# there outliers, but we will not treat them

# EBIT
# some of the outliers are very large, but still plausible for large firms

# working_capital_by_total_assets
# some of the outliers are very small, but still plausible

# income_tax_by_total_assets
# plausible outliers

# EBITDA
# plausible outliers

# Don't forget to report that we did this:
# 3 observations are removed
data <- data[data$years_to_event!=0, ]
data <- data %>%
  group_by(sector) %>%
  mutate(n=n()) %>%
  filter(n > 1) %>%
  select(-n)

# --- Make visualizations of the continuous variables
# total_liabilities_by_net_assets
par(mfrow=c(2, 2))
hist(data$total_liabilities_by_net_assets, breaks=50)
hist(data$transf_total_liabilities_by_net_assets, breaks=50)
boxplot(data$total_liabilities_by_net_assets)
boxplot(data$transf_total_liabilities_by_net_assets)

# cash_by_total_assets
par(mfrow=c(2, 2))
hist(data$cash_by_total_assets, breaks=50)
hist(data$log_cash_by_total_assets, breaks=50)
boxplot(data$cash_by_total_assets)
boxplot(data$log_cash_by_total_assets)

# cash_by_current_liabilities
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$cash_by_current_liabilities, breaks=50)
hist(data$log_cash_by_current_liabilities, breaks=50)
boxplot(data$cash_by_current_liabilities)
boxplot(data$log_cash_by_current_liabilities)

# current_assets_by_current_liabilities
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$current_assets_by_current_liabilities, breaks=50)
hist(data$log_current_assets_by_current_liabilities, breaks=50)
boxplot(data$current_assets_by_current_liabilities)
boxplot(data$log_current_assets_by_current_liabilities)

# tax
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$tax, breaks=50)
boxplot(data$tax)

# financial_expenses_by_total_assets
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$financial_expenses_by_total_assets, breaks=50)
boxplot(data$financial_expenses_by_total_assets)

# EBIT
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$EBIT, breaks=50)
boxplot(data$EBIT)

# working_capital_by_total_assets
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$working_capital_by_total_assets, breaks=50)
boxplot(data$working_capital_by_total_assets)

# income_tax_by_total_assets
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$income_tax_by_total_assets, breaks=50)
boxplot(data$income_tax_by_total_assets)

# EBITDA
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$EBITDA, breaks=50)
boxplot(data$EBITDA)

# --- Stepwise variable selection using AIC
# Iteration 1
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("tax", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", fit.AIC))

AIC_df
#                                     Variable AIC
# 1     transf_total_liabilities_by_net_assets 53272.44
# 2                   log_cash_by_total_assets 52969.39
# 3            log_cash_by_current_liabilities 52511.14
# 4  log_current_assets_by_current_liabilities 52345.74 <-
# 5                                        tax 52942.31
# 6         financial_expenses_by_total_assets 53263.75
# 7                                       EBIT 53283.42
# 8            working_capital_by_total_assets 53258.00
# 9                 income_tax_by_total_assets 53279.39
# 10                              type_dummies 52521.67
# 11                                    EBITDA 53281.52
# 12                             size_dummmies 53029.85

# Iteration 2
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("tax", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2- 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummies", fit.AIC))

AIC_df
#                                  Variable AIC
# 1  transf_total_liabilities_by_net_assets 52344.21
# 2                log_cash_by_total_assets 52236.22
# 3         log_cash_by_current_liabilities 52143.19
# 4                                     tax 52105.36
# 5      financial_expenses_by_total_assets 52346.76
# 6                                    EBIT 52347.73
# 7         working_capital_by_total_assets 52339.90
# 8              income_tax_by_total_assets 52347.12
# 9                            type_dummies 51683.14 <-
# 10                                 EBITDA 52346.46
# 11                           size_dummies 52080.25

# Iteration 3
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("tax", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummies", fit.AIC))

AIC_df
#                                  Variable AIC
# 1  transf_total_liabilities_by_net_assets 51682.81
# 2                log_cash_by_total_assets 51540.10
# 3         log_cash_by_current_liabilities 51451.68 <-
# 4                                     tax 51539.15
# 5      financial_expenses_by_total_assets 51683.92
# 6                                    EBIT 51684.65
# 7         working_capital_by_total_assets 51675.80
# 8              income_tax_by_total_assets 51683.95
# 9                                  EBITDA 51685.10
# 10                           size_dummies 51592.41

# Iteration 4
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("tax", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 9 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummies", fit.AIC))

AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51451.64
# 2               log_cash_by_total_assets 51368.20
# 3                                    tax 51318.45
# 4     financial_expenses_by_total_assets 51450.53
# 5                                   EBIT 51451.38
# 6        working_capital_by_total_assets 51452.62
# 7             income_tax_by_total_assets 51453.28
# 8                                 EBITDA 51453.52
# 9                           size_dummies 51330.05 <-

# Iteration 5
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("tax", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51329.81
# 2               log_cash_by_total_assets 51256.28 <-
# 3                                    tax 51263.97
# 4     financial_expenses_by_total_assets 51325.94
# 5                                   EBIT 51332.01
# 6        working_capital_by_total_assets 51330.30
# 7             income_tax_by_total_assets 51331.04
# 8                                 EBITDA 51331.98

# Iteration 6
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("tax", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51256.10
# 2                                    tax 51191.90 <-
# 3     financial_expenses_by_total_assets 51248.87
# 4                                   EBIT 51258.06
# 5        working_capital_by_total_assets 51254.83
# 6             income_tax_by_total_assets 51257.18
# 7                                 EBITDA 51257.71

# Iteration 7
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("financial_expenses_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51191.76
# 2     financial_expenses_by_total_assets 51184.92 <-
# 3                                   EBIT 51191.64
# 4        working_capital_by_total_assets 51191.46
# 5             income_tax_by_total_assets 51187.42
# 6                                 EBITDA 51188.70

# Iteration 8
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 13 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 13 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 13 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 13 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 13 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBITDA", fit.AIC))

AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51184.83
# 2                                   EBIT 51184.72
# 3        working_capital_by_total_assets 51185.03
# 4             income_tax_by_total_assets 51178.26
# 5                                 EBITDA 51181.80 <-

# Iteration 9
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 14 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 14 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 14 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 14 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("income_tax_by_total_assets", fit.AIC))
  
AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51181.74
# 2                                   EBIT 51183.36
# 3        working_capital_by_total_assets 51182.01
# 4             income_tax_by_total_assets 51174.75 <-

# Iteration 10
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 15 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("transf_total_liabilities_by_net_assets", fit.AIC))
names(AIC_df) <- c("Variable", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 15 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 15 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))
  
AIC_df
#                                 Variable AIC
# 1 transf_total_liabilities_by_net_assets 51174.71 <-
# 2                                   EBIT 51176.18
# 3        working_capital_by_total_assets 51175.19

# Iteration 11
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 16 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 16 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))

AIC_df




# Iteration 11
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 14 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("EBIT", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + tax
                   + financial_expenses_by_total_assets
                   + EBITDA
                   + income_tax_by_total_assets
                   + transf_total_liabilities_by_net_assets
                   + working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 14 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("working_capital_by_total_assets", fit.AIC))
  
AIC_df

# Histogram of the estimated frailties
jpeg(file = "visualizations/temp1.jpeg", width = 2500, height = 2000, res = 300)
autoplot(fit1, type="hist")
dev.off()

jpeg(file = "visualizations/temp2.jpeg", width = 2500, height = 2000, res = 300)
autoplot(fit1, type="hr", lp=c(0, 1))
dev.off()

jpeg(file = "visualizations/temp3.jpeg", width = 2500, height = 2000, res = 300)
autoplot(fit1, type="pred", lp=c(0))
dev.off()

# Predict baseline
# predict(fit1, lp=c(0))


# fit2 <- fitfrail(Surv(years_to_event, censor) ~
#                      current_assets_by_current_liabilities
#                      + cluster(size_classification),
#                 dat = data,
#                 frailty = "gamma")
# fit2
