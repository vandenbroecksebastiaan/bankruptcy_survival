library(dplyr)
library(survival)
library(frailtySurv)
library(frailtyEM)
library(ggstatsplot)
options(scipen = 999)
rm(list=ls())

# --- Read data
data <- read.csv("data/bankruptcy_transformed_alive_and_fail.csv")

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

# There is a row without a cluster
data <- data[data$sector!="",]

# --- Transform variables / remove outliers

# total_liabilities_by_net_assets
data$log_total_liabilities_by_net_assets <- log(data$total_liabilities_by_net_assets)
min <- min(
  data$log_total_liabilities_by_net_assets[is.finite(data$log_total_liabilities_by_net_assets)]
)
data$log_total_liabilities_by_net_assets <- replace(
  data$log_total_liabilities_by_net_assets, data$log_total_liabilities_by_net_assets == -Inf,
  min
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
min <- min(
  data$log_current_assets_by_current_liabilities[is.finite(data$log_current_assets_by_current_liabilities)]
)
data$log_current_assets_by_current_liabilities <- replace(
  data$log_current_assets_by_current_liabilities, data$log_current_assets_by_current_liabilities == -Inf,
  min
)

# tax
data$log_tax <- log(data$tax)
min <- min(
  data$log_tax[is.finite(data$log_tax)]
)
data$log_tax <- replace(
  data$log_tax, data$log_tax == -Inf,
  min
)

# financial_expenses_by_total_assets
data$log_financial_expenses_by_total_assets <- log(data$financial_expenses_by_total_assets)
min <- min(
  data$log_financial_expenses_by_total_assets[is.finite(data$log_financial_expenses_by_total_assets)]
)
data$log_financial_expenses_by_total_assets <- replace(
  data$log_financial_expenses_by_total_assets, data$log_financial_expenses_by_total_assets == -Inf,
  min
)

# EBIT
data$log_EBIT <- log(data$EBIT)
min <- min(
  data$log_EBIT[is.finite(data$log_EBIT)]
)
data$log_EBIT <- replace(
  data$log_EBIT, data$log_EBIT == -Inf,
  min
)

# working_capital_by_total_assets
data$log_working_capital_by_total_assets <- log(data$working_capital_by_total_assets)
min <- min(
  data$log_working_capital_by_total_assets[is.finite(data$log_working_capital_by_total_assets)]
)
data$log_working_capital_by_total_assets <- replace(
  data$log_working_capital_by_total_assets, data$log_working_capital_by_total_assets == -Inf,
  min
)

# income_tax_by_total_assets
data$log_income_tax_by_total_assets <- log(data$income_tax_by_total_assets)
min <- min(
  data$log_income_tax_by_total_assets[is.finite(data$log_income_tax_by_total_assets)]
)
data$log_income_tax_by_total_assets <- replace(
  data$log_income_tax_by_total_assets, data$log_income_tax_by_total_assets == -Inf,
  min
)

# EBITDA
data$log_EBITDA <- log(data$EBITDA)
min <- min(
  data$log_EBITDA[is.finite(data$log_EBITDA)]
)
data$log_EBITDA <- replace(
  data$log_EBITDA, data$log_EBITDA == -Inf,
  min
)

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
jpeg(file = "visualizations/total_liabilities_by_total_asssets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
hist(data$total_liabilities_by_net_assets, breaks=50,
     main="total_liabilities_by_total_assets")
hist(data$log_total_liabilities_by_net_assets, breaks=50,
     main="log(total_liabilities_by_total_assets)")
boxplot(data$total_liabilities_by_net_assets,
     main="total_liabilities_by_total_assets")
boxplot(data$log_total_liabilities_by_net_assets,
     main="log(total_liabilities_by_total_assets)")
dev.off()

# cash_by_total_assets
jpeg(file = "visualizations/cash_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
hist(data$cash_by_total_assets, breaks=50,
     main="cash_by_total_assets")
hist(data$log_cash_by_total_assets, breaks=50,
     main="log(cash_by_total_assets)")
boxplot(data$cash_by_total_assets,
     main="cash_by_total_assets")
boxplot(data$log_cash_by_total_assets,
     main="log(cash_by_total_assets)")
dev.off()

# cash_by_current_liabilities
jpeg(file = "visualizations/cash_by_current_liabilities.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
hist(data$cash_by_current_liabilities, breaks=50,
     main="cash_by_current_liabilities")
hist(data$log_cash_by_current_liabilities, breaks=50,
     main="log(cash_by_current_liabilities)")
boxplot(data$cash_by_current_liabilities,
     main="cash_by_current_liabilities")
boxplot(data$log_cash_by_current_liabilities,
     main="log(cash_by_current_liabilities)")
dev.off()

# current_assets_by_current_liabilities
jpeg(file = "visualizations/current_assets_by_current_liabilities.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$current_assets_by_current_liabilities, breaks=50,
     main="current_assets_by_current_liabilities")
hist(data$log_current_assets_by_current_liabilities, breaks=50,
     main="log(current_assets_by_current_liabilities)")
boxplot(data$current_assets_by_current_liabilities,
        main="current_assets_by_current_liabilities")
boxplot(data$log_current_assets_by_current_liabilities,
        main="log(current_assets_by_current_liabilities)")
dev.off()

# tax
jpeg(file = "visualizations/tax.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$tax, breaks=50,
     main="tax")
hist(data$log_tax, breaks=50,
     main="log(tax)")
boxplot(data$tax,
        main="tax")
boxplot(data$log_tax,
        main="log(tax)")
dev.off()

# financial_expenses_by_total_assets
jpeg(file = "visualizations/financial_expenses_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$financial_expenses_by_total_assets, breaks=50,
     main="financial_expenses_by_total_assets")
hist(data$log_financial_expenses_by_total_assets, breaks=50,
     main="log(financial_expenses_by_total_assets)")
boxplot(data$financial_expenses_by_total_assets,
        main="financial_expenses_by_total_assets")
boxplot(data$log_financial_expenses_by_total_assets,
        main="log(log_financial_expenses_by_total_assets)")
dev.off()

# EBIT
jpeg(file = "visualizations/financial_expenses_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$EBIT, breaks=50,
     main="EBIT")
hist(data$log_EBIT, breaks=50,
     main="log(EBIT)")
boxplot(data$EBIT,
        main="EBIT")
boxplot(data$log_EBIT,
        main="log(EBIT)")
dev.off()

# working_capital_by_total_assets
jpeg(file = "visualizations/working_capital_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$working_capital_by_total_assets, breaks=50,
     main="working_capital_by_total_assets")
hist(data$log_working_capital_by_total_assets, breaks=50,
     main="log(working_capital_by_total_assets)")
boxplot(data$working_capital_by_total_assets,
        main="working_capital_by_total_assets")
boxplot(data$log_working_capital_by_total_assets,
        main="log(working_capital_by_total_assets)")
dev.off()

# income_tax_by_total_assets
jpeg(file = "visualizations/working_capital_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$income_tax_by_total_assets, breaks=50,
     main="income_tax_by_total_assets")
hist(data$log_income_tax_by_total_assets, breaks=50,
     main="log(income_tax_by_total_assets)")
boxplot(data$income_tax_by_total_assets,
        main="income_tax_by_total_assets")
boxplot(data$log_income_tax_by_total_assets,
        main="log(income_tax_by_total_assets)")
dev.off()

# EBITDA
jpeg(file = "visualizations/EBITDA.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$EBITDA, breaks=50,
     main="EBITDA")
hist(data$log_EBITDA, breaks=50,
     main="log(EBITDA)")
boxplot(data$EBITDA,
        main="EBITDA")
boxplot(data$log_EBITDA,
        main="log(EBITDA)")
dev.off()

# --- Stepwise variable selection using AIC
# Iteration 1
AIC_df = data.frame()
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_total_liabilities_by_net_assets", "Iteration 1", fit.AIC))
names(AIC_df) <- c("Variable", "iteration", "AIC")

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_tax", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_financial_expenses_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBIT", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_working_capital_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBITDA", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 1", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 1",]
#                                     Variable   iteration      AIC
# 1        log_total_liabilities_by_net_assets Iteration 1 25714.76 <-
# 2                   log_cash_by_total_assets Iteration 1 59993.60
# 3            log_cash_by_current_liabilities Iteration 1 59137.00
# 4  log_current_assets_by_current_liabilities Iteration 1 59117.92
# 5                                    log_tax Iteration 1 58281.23
# 6     log_financial_expenses_by_total_assets Iteration 1 59753.98
# 7                                   log_EBIT Iteration 1 26072.96
# 8        log_working_capital_by_total_assets Iteration 1 36181.37
# 9             log_income_tax_by_total_assets Iteration 1 59204.57
# 10                              type_dummies Iteration 1 60267.79
# 11                                log_EBITDA Iteration 1 35034.76
# 12                             size_dummmies Iteration 1 59801.01

# Iteration 2
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_tax", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_financial_expenses_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBIT", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_working_capital_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBITDA", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 2", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 2",]
#                                     Variable   iteration      AIC
# 13                  log_cash_by_total_assets Iteration 2 25514.74
# 14           log_cash_by_current_liabilities Iteration 2 25236.32
# 15 log_current_assets_by_current_liabilities Iteration 2 25091.19
# 16                                   log_tax Iteration 2 24911.49
# 17    log_financial_expenses_by_total_assets Iteration 2 25627.77
# 18                                  log_EBIT Iteration 2 14985.93 <-
# 19       log_working_capital_by_total_assets Iteration 2 19885.24
# 20            log_income_tax_by_total_assets Iteration 2 25323.80
# 21                              type_dummies Iteration 2 25631.42
# 22                                log_EBITDA Iteration 2 17746.90
# 23                             size_dummmies Iteration 2 25335.27

# Iteration 3
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_tax", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_financial_expenses_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_working_capital_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBITDA", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 3", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 3",]
#                                     Variable   iteration      AIC
# 25           log_cash_by_current_liabilities Iteration 3 14624.87
# 26 log_current_assets_by_current_liabilities Iteration 3 14587.80
# 27                                   log_tax Iteration 3 14890.39
# 28    log_financial_expenses_by_total_assets Iteration 3 14952.30
# 29       log_working_capital_by_total_assets Iteration 3 11512.54 <-
# 30            log_income_tax_by_total_assets Iteration 3 14823.38
# 31                              type_dummies Iteration 3 14942.04
# 32                                log_EBITDA Iteration 3 14699.08
# 33                             size_dummmies Iteration 3 14948.72

# Iteration 4
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_tax", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_financial_expenses_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBITDA", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 4", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 4",]
#                                     Variable   iteration      AIC
# 34                  log_cash_by_total_assets Iteration 4 11396.02
# 35           log_cash_by_current_liabilities Iteration 4 11276.54
# 36 log_current_assets_by_current_liabilities Iteration 4 11237.64 <-
# 37                                   log_tax Iteration 4 11454.35
# 38    log_financial_expenses_by_total_assets Iteration 4 11485.93
# 39            log_income_tax_by_total_assets Iteration 4 11395.58
# 40                              type_dummies Iteration 4 11483.17
# 41                                log_EBITDA Iteration 4 11315.01
# 42                             size_dummmies Iteration 4 11484.82

# Iteration 5
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_tax", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_financial_expenses_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBITDA", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 5", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 5",]
#                                  Variable   iteration      AIC
# 43               log_cash_by_total_assets Iteration 5 11148.62
# 44        log_cash_by_current_liabilities Iteration 5 11107.54 <-
# 45                                log_tax Iteration 5 11167.51
# 46 log_financial_expenses_by_total_assets Iteration 5 11222.10
# 47         log_income_tax_by_total_assets Iteration 5 11105.67
# 48                           type_dummies Iteration 5 11210.05
# 49                             log_EBITDA Iteration 5 11056.24
# 50                          size_dummmies Iteration 5 11198.18

# Iteration 6
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + log_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_tax", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + log_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_financial_expenses_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + log_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 9 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + log_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_EBITDA", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_total_liabilities_by_net_assets
                   + log_EBIT
                   + log_working_capital_by_total_assets
                   + log_current_assets_by_current_liabilities
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 6", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 6",]
#                                  Variable   iteration      AIC
# 51               log_cash_by_total_assets Iteration 6 10988.88
# 52                                log_tax Iteration 6 11038.80
# 53 log_financial_expenses_by_total_assets Iteration 6 11091.89
# 54         log_income_tax_by_total_assets Iteration 6 10964.85
# 55                           type_dummies Iteration 6 11075.96
# 56                             log_EBITDA Iteration 6 10934.76 <-
# 57                          size_dummmies Iteration 6 11048.98











# --- The model
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
fit
# Call: 
#   emfrail(formula = Surv(years_to_event, censor) ~ log_current_assets_by_current_liabilities + 
#             cooperative + nonprofit + other + private + log_cash_by_current_liabilities + 
#             size_small + size_large + size_v_large + log_cash_by_total_assets + 
#             tax + financial_expenses_by_total_assets + EBITDA + income_tax_by_total_assets + 
#             transf_total_liabilities_by_net_assets + cluster(sector), 
#           data = data)
# 
# log-likelihood: -25572.36 
# frailty variance: 0.1754221 
# theta: 5.700538 
# 
# coef        exp(coef)         se(coef)
# log_current_assets_by_current_liabilities  -0.098753582673   0.905965926222   0.033200347669
# cooperative                                 0.627237938436   1.872431658792   0.147138494327
# nonprofit                                  -0.444146312150   0.641371573508   0.583808498311
# other                                       0.514382870255   1.672605968332   0.648626957536
# private                                     1.203042779324   3.330234691125   0.066084370773
# log_cash_by_current_liabilities            -0.438716455892   0.644863600984   0.035686602567
# size_small                                  0.307433293454   1.359930089238   0.057538438457
# size_large                                 -0.358481657903   0.698736442012   0.156767973970
# size_v_large                               -0.355997095328   0.700474654881   0.511512792811
# log_cash_by_total_assets                    0.285151309063   1.329963248391   0.033331460170
# tax                                        -0.000004656999   0.999995343012   0.000000621245
# financial_expenses_by_total_assets         -0.850995990613   0.426989442616   0.293163009317
# EBITDA                                      0.000000062326   1.000000062326   0.000000028030
# income_tax_by_total_assets                  0.627564376753   1.873042992007   0.210396635122
# transf_total_liabilities_by_net_assets     -0.000000955367   0.999999044634   0.000000665747
# adj. se                z      p
# log_current_assets_by_current_liabilities   0.033200972551  -2.974418370510 0.0029
# cooperative                                 0.147141635420   4.262817499916 0.0000
# nonprofit                                   0.583824415154  -0.760753234399 0.4468
# other                                       0.648666064672   0.792985633547 0.4278
# private                                     0.066084419622  18.204635619136 0.0000
# log_cash_by_current_liabilities             0.035690848857 -12.292127252249 0.0000
# size_small                                  0.057539156678   5.343027447801 0.0000
# size_large                                  0.156768021048  -2.286701429961 0.0222
# size_v_large                                0.511512999052  -0.695968814064 0.4864
# log_cash_by_total_assets                    0.033337006416   8.553596729935 0.0000
# tax                                         0.000000621342  -7.495064926235 0.0000
# financial_expenses_by_total_assets          0.293176993926  -2.902669746408 0.0037
# EBITDA                                      0.000000028030   2.223570351752 0.0262
# income_tax_by_total_assets                  0.210503910302   2.981248072074 0.0029
# transf_total_liabilities_by_net_assets      0.000000665757  -1.435008798780 0.1513
# 
# Score test for heterogeneity: p-val 0.000000000000000000000000000000000000000000337

fit$frail
# C - Manufacturing 
# 0.7663449 
# G - Wholesale and retail trade; repair of motor vehicles and motorcycles 
# 0.8639690 
# H - Transportation and storage 
# 1.4532523 
# J - Information and communication 
# 1.2472926 
# M - Professional, scientific and technical activities 
# 0.7480826 
# D - Electricity, gas, steam and air conditioning supply 
# 1.1550865 
# L - Real estate activities 
# 0.6649859 
# F - Construction 
# 1.3723498 
# N - Administrative and support service activities 
# 1.2010806 
# E - Water supply; sewerage, waste management and remediation activities 
# 0.6526173 
# Q - Human health and social work activities 
# 0.6097234 
# K - Financial and insurance activities 
# 0.3032562 
# I - Accommodation and food service activities 
# 1.6981274 
# R - Arts, entertainment and recreation 
# 1.4354011 
# P - Education 
# 1.0394652 
# O - Public administration and defence; compulsory social security 
# 0.9381831 
# S - Other service activities 
# 0.9922108 
# A - Agriculture, forestry and fishing 
# 0.7613264

table(data$sector)
# A - Agriculture, forestry and fishing 
# 44 
# C - Manufacturing 
# 708 
# D - Electricity, gas, steam and air conditioning supply 
# 11 
# E - Water supply; sewerage, waste management and remediation activities 
# 23 
# F - Construction 
# 1382 
# G - Wholesale and retail trade; repair of motor vehicles and motorcycles 
# 2317 
# H - Transportation and storage 
# 501 
# I - Accommodation and food service activities 
# 899 
# J - Information and communication 
# 398 
# K - Financial and insurance activities 
# 517 
# L - Real estate activities 
# 202 
# M - Professional, scientific and technical activities 
# 1134 
# N - Administrative and support service activities 
# 550 
# O - Public administration and defence; compulsory social security 
# 3 
# P - Education 
# 35 
# Q - Human health and social work activities 
# 225 
# R - Arts, entertainment and recreation 
# 106 
# S - Other service activities 
# 200

# --- Other things

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

# Are the results much more different when using the penalized partial
# likelihood instead of the marginal likelihood?

# Plot log cumulative hazard
# Public and varying size dummies
new_data <- data.frame(
  log_current_assets_by_current_liabilities   = c(1,     1,     1,      1,     1,     1,     1,      1,     1,     1,     1,      1,     1,     1,     1,      1,      1,     1,     1,      1      ),
  cooperative                                 = c(0,     0,     0,      0,     1,     1,     1,      1,     0,     0,     0,      0,     0,     0,     0,      0,      0,     0,     0,      0      ),     
  nonprofit                                   = c(0,     0,     0,      0,     0,     0,     0,      0,     1,     1,     1,      1,     0,     0,     0,      0,      0,     0,     0,      0      ),     
  other                                       = c(0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,     1,     1,     1,      1,      0,     0,     0,      0      ),     
  private                                     = c(0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,      1,     1,     1,      1      ),     
  log_cash_by_current_liabilities             = c(-8,   -8,    -8,     -8,     -8,   -8,    -8,     -8,     -8,   -8,    -8,     -8,     -8,   -8,    -8,     -8,      -8,   -8,    -8,     -8      ),    
  size_small                                  = c(1,     0,     0,      0,     1,     0,     0,      0,     1,     0,     0,      0,     1,     0,     0,      0,      1,     0,     0,      0      ),     
  size_large                                  = c(0,     0,     1,      0,     0,     0,     1,      0,     0,     0,     1,      0,     0,     0,     1,      0,      0,     0,     1,      0      ),     
  size_v_large                                = c(0,     0,     0,      1,     0,     0,     0,      1,     0,     0,     0,      1,     0,     0,     0,      1,      0,     0,     0,      1      ),     
  log_cash_by_total_assets                    = c(-10,  -10,   -10,    -10,    -10,  -10,   -10,    -10,    -10,  -10,   -10,    -10,    -10,  -10,   -10,    -10,     -10,  -10,   -10,    -10     ),   
  tax                                         = c(10000, 10000, 10000,  10000, 10000, 10000, 10000,  10000, 10000, 10000, 10000,  10000, 10000, 10000, 10000,  10000,  10000, 10000, 10000,  10000  ), 
  financial_expenses_by_total_assets          = c(0.1,   0.1,   0.1,    0.1,   0.1,   0.1,   0.1,    0.1,   0.1,   0.1,   0.1,    0.1,   0.1,   0.1,   0.1,    0.1,    0.1,   0.1,   0.1,    0.1    ),   
  EBITDA                                      = c(0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,      0,     0,     0,      0      ),     
  income_tax_by_total_assets                  = c(0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,      0,     0,     0,      0      ),     
  transf_total_liabilities_by_net_assets      = c(0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,     0,     0,     0,      0,      0,     0,     0,      0      )       
)

cum_haz <- predict(fit, type="conditional", quantity="cumhaz",
                   newdata=new_data)

plot(log(cum_haz[[1]]$cumhaz), col=1, type="S", ylim=c(-10, 1))
for (i in 2:length(cum_haz)) {
  lines(log(cum_haz[[i]]$cumhaz), col=i, type="S")
}
































