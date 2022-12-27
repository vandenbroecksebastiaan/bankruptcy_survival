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

# [x] total_liabilities                         -> use the log
# [x] cash_by_total_assets                      -> use the log
# [x] cash_by_current_liabilities               -> use the log
# [x] current_assets_by_current_liabilities     -> use the log
# [x] tax                                       -> use the truncation
# [x] EBITDA                                    -> use the truncation
# [x] roa                                       -> use the truncation
# [x] financial_expenses_by_total_assets        -> use the truncation
# [x] EBIT                                      -> use the truncation
# [x] current_liabilites_by_total_liabilities   -> use the log
# [x] working_capital_by_total_assets           -> use the truncation
# [x] size_classification
# [x] income_tax_by_total_assets                -> use the truncation
# [x] sector
# [x] n_employees                               -> use the log
# [x] cooperative	nonprofit	other	private	public

# total_liabilities
data$log_total_liabilities <- log(data$total_liabilities)

# cash_by_total_assets
data$log_cash_by_total_assets <- log(data$cash_by_total_assets)
min <- min(
  data$log_cash_by_total_assets[is.finite(data$log_cash_by_total_assets)]
)
data$log_cash_by_total_assets <- replace(
  data$log_cash_by_total_assets, data$log_cash_by_total_assets == -Inf,
  min
)
data$log_cash_by_total_assets <- replace(
  data$log_cash_by_total_assets, is.na(data$log_cash_by_total_assets),
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
data$log_cash_by_current_liabilities <- replace(
  data$log_cash_by_current_liabilities, is.na(data$log_cash_by_current_liabilities),
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
data$log_current_assets_by_current_liabilities <- replace(
  data$log_current_assets_by_current_liabilities, is.na(data$log_current_assets_by_current_liabilities),
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
data$log_tax <- replace(
  data$log_tax, is.na(data$log_tax),
  min
)

# Truncate extreme outliers
upper_bound <- quantile(data$tax, probs=c(0.25)) + 10 * IQR(data$tax)
lower_bound <- quantile(data$tax, probs=c(0.75)) - 10 * IQR(data$tax)
data$trunc_tax <- ifelse(data$tax > upper_bound, upper_bound,
                          ifelse(data$tax < lower_bound, lower_bound, data$tax))

# EBITDA
data$log_EBITDA <- log(data$EBITDA)
min <- min(
  data$log_EBITDA[is.finite(data$log_EBITDA)]
)
data$log_EBITDA <- replace(
  data$log_EBITDA, data$log_EBITDA == -Inf,
  min
)
data$log_EBITDA <- replace(
  data$log_EBITDA, is.na(data$log_EBITDA),
  min
)

# Truncate extreme outliers
upper_bound <- quantile(data$EBITDA, probs=c(0.25)) + 10 * IQR(data$EBITDA)
lower_bound <- quantile(data$EBITDA, probs=c(0.75)) - 10 * IQR(data$EBITDA)
data$trunc_EBITDA <- ifelse(data$EBITDA > upper_bound, upper_bound,
                          ifelse(data$EBITDA < lower_bound, lower_bound, data$EBITDA))


# ROA
# Truncate extreme outliers
upper_bound <- quantile(data$roa, probs=c(0.25)) + 10 * IQR(data$roa)
lower_bound <- quantile(data$roa, probs=c(0.75)) - 10 * IQR(data$roa)
data$trunc_roa <- ifelse(data$roa > upper_bound, upper_bound,
                          ifelse(data$roa < lower_bound, lower_bound, data$roa))

# financial_expenses_by_total_assets
data$log_financial_expenses_by_total_assets <- log(data$financial_expenses_by_total_assets)
min <- min(
  data$log_financial_expenses_by_total_assets[is.finite(data$log_financial_expenses_by_total_assets)]
)
data$log_financial_expenses_by_total_assets <- replace(
  data$log_financial_expenses_by_total_assets, data$log_financial_expenses_by_total_assets == -Inf,
  min
)
data$log_financial_expenses_by_total_assets <- replace(
  data$log_financial_expenses_by_total_assets, is.na(data$log_financial_expenses_by_total_assets),
  min
)

# Truncate extreme outliers
upper_bound <- quantile(data$financial_expenses_by_total_assets, probs=c(0.25)) + 10 * IQR(data$financial_expenses_by_total_assets)
lower_bound <- quantile(data$financial_expenses_by_total_assets, probs=c(0.75)) - 10 * IQR(data$financial_expenses_by_total_assets)
data$trunc_financial_expenses_by_total_assets <- ifelse(data$financial_expenses_by_total_assets > upper_bound, upper_bound,
                          ifelse(data$financial_expenses_by_total_assets < lower_bound, lower_bound, data$financial_expenses_by_total_assets))

# EBIT
data$log_EBIT <- log(data$EBIT)
min <- min(
  data$log_EBIT[is.finite(data$log_EBIT)]
)
data$log_EBIT <- replace(
  data$log_EBIT, data$log_EBIT == -Inf,
  min
)
data$log_EBIT <- replace(
  data$log_EBIT, is.na(data$log_EBIT),
  min
)

# Truncate extreme outliers
upper_bound <- quantile(data$EBIT, probs=c(0.25)) + 10 * IQR(data$EBIT)
lower_bound <- quantile(data$EBIT, probs=c(0.75)) - 10 * IQR(data$EBIT)
data$trunc_EBIT <- ifelse(data$EBIT > upper_bound, upper_bound,
                          ifelse(data$EBIT < lower_bound, lower_bound, data$EBIT))

# current_liabilities_by_total_liabilities
data$log_current_liabilities_by_total_liabilities <- log(data$current_liabilites_by_total_liabilities)
min <- min(
  data$log_current_liabilities_by_total_liabilities[is.finite(data$log_current_liabilities_by_total_liabilities)]
)
data$log_current_liabilities_by_total_liabilities <- replace(
  data$log_current_liabilities_by_total_liabilities, data$log_current_liabilities_by_total_liabilities == -Inf,
  min
)
data$log_current_liabilities_by_total_liabilities <- replace(
  data$log_current_liabilities_by_total_liabilities, is.na(data$log_current_liabilities_by_total_liabilities),
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
data$log_working_capital_by_total_assets <- replace(
  data$log_working_capital_by_total_assets, is.na(data$log_working_capital_by_total_assets),
  min
)

# Truncate extreme outliers
upper_bound <- quantile(data$working_capital_by_total_assets, probs=c(0.25)) + 5 * IQR(data$working_capital_by_total_assets)
lower_bound <- quantile(data$working_capital_by_total_assets, probs=c(0.75)) - 5 * IQR(data$working_capital_by_total_assets)
data$trunc_working_capital_by_total_assets <- ifelse(data$working_capital_by_total_assets > upper_bound, upper_bound,
                          ifelse(data$working_capital_by_total_assets < lower_bound, lower_bound, data$working_capital_by_total_assets))

# Size classification
# No transformations

# income_tax_by_total_assets
data$log_income_tax_by_total_assets <- log(data$income_tax_by_total_assets)
min <- min(
  data$log_income_tax_by_total_assets[is.finite(data$log_income_tax_by_total_assets)]
)
data$log_income_tax_by_total_assets <- replace(
  data$log_income_tax_by_total_assets, data$log_income_tax_by_total_assets == -Inf,
  min
)
data$log_income_tax_by_total_assets <- replace(
  data$log_income_tax_by_total_assets, is.na(data$log_income_tax_by_total_assets),
  min
)
# Truncate extreme outliers
upper_bound <- quantile(data$income_tax_by_total_assets, probs=c(0.25)) + 5 * IQR(data$income_tax_by_total_assets)
lower_bound <- quantile(data$income_tax_by_total_assets, probs=c(0.75)) - 5 * IQR(data$income_tax_by_total_assets)
data$trunc_income_tax_by_total_assets <- ifelse(data$income_tax_by_total_assets > upper_bound, upper_bound,
                          ifelse(data$income_tax_by_total_assets < lower_bound, lower_bound, data$income_tax_by_total_assets))

# sector

# n_employees
data$log_n_employees = log(data$n_employees)

# Don't forget to report that we did this:
# 3 observations are removed
data <- data[data$years_to_event!=0, ]
data <- data %>%
  group_by(sector) %>%
  mutate(n=n()) %>%
  filter(n > 1) %>%
  select(-n)

# --- Make visualizations of the continuous variables
# total_liabilities
jpeg(file = "visualizations/total_liabilities.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
hist(data$total_liabilities, breaks=50,
     main="total_liabilities")
hist(data$log_total_liabilities, breaks=50,
     main="log(total_liabilities)")
boxplot(data$total_liabilities,
     main="total_liabilities")
boxplot(data$log_total_liabilities,
     main="log(total_liabilities)")
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
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
hist(data$tax, breaks=50,
     main="tax")
hist(data$log_tax, breaks=50,
     main="log(tax)")
hist(data$trunc_tax, breaks=50,
     main="trunc(tax)")
boxplot(data$tax,
        main="tax")
boxplot(data$log_tax,
        main="log(tax)")
boxplot(data$trunc_tax,
        main="trunc(tax)")
dev.off()

# EBITDA
jpeg(file = "visualizations/EBITDA.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
hist(data$EBITDA, breaks=50,
     main="EBITDA")
hist(data$log_EBITDA, breaks=50,
     main="log(EBITDA)")
hist(data$trunc_EBITDA, breaks=50,
     main="trunc(EBITDA)")
boxplot(data$EBITDA,
        main="EBITDA")
boxplot(data$log_EBITDA,
        main="log(EBITDA)")
boxplot(data$trunc_EBITDA,
        main="trunc(EBITDA)")
dev.off()

# ROA
jpeg(file = "visualizations/roa.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$roa, breaks=50,
     main="roa")
hist(data$trunc_roa, breaks=50,
     main="trunc(roa)")
boxplot(data$roa,
        main="roa")
boxplot(data$trunc_roa,
        main="trunc(roa)")
dev.off()

# financial_expenses_by_total_assets
jpeg(file = "visualizations/financial_expenses_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
hist(data$financial_expenses_by_total_assets, breaks=50,
     main="financial_expenses_by_total_assets")
hist(data$log_financial_expenses_by_total_assets, breaks=50,
     main="log(financial_expenses_by_total_assets)")
hist(data$trunc_financial_expenses_by_total_assets, breaks=50,
     main="trunc(financial_expenses_by_total_assets)")
boxplot(data$financial_expenses_by_total_assets,
        main="financial_expenses_by_total_assets")
boxplot(data$log_financial_expenses_by_total_assets,
        main="log(log_financial_expenses_by_total_assets)")
boxplot(data$trunc_financial_expenses_by_total_assets,
        main="trunc(log_financial_expenses_by_total_assets)")
dev.off()

# EBIT
jpeg(file = "visualizations/EBIT.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
hist(data$EBIT, breaks=50,
     main="EBIT")
hist(data$log_EBIT, breaks=50,
     main="log(EBIT)")
hist(data$trunc_EBIT, breaks=50,
     main="trunc(EBIT)")
boxplot(data$EBIT,
        main="EBIT")
boxplot(data$log_EBIT,
        main="log(EBIT)")
boxplot(data$trunc_EBIT,
        main="trunc(EBIT)")
dev.off()

# current_liabilities_by_total_liabilities
jpeg(file = "visualizations/current_liabilities_by_total_liabilities.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$current_liabilites_by_total_liabilities, breaks=50,
     main="current_liabilities_by_total_liabilities")
hist(data$log_current_liabilities_by_total_liabilities, breaks=50,
     main="log(current_liabilites_by_total_liabilities)")
boxplot(data$current_liabilites_by_total_liabilities,
        main="current_liabilities_by_total_liabilities")
boxplot(data$log_current_liabilities_by_total_liabilities,
        main="log(current_liabilites_by_total_liabilities)")
dev.off()

# working_capital_by_total_assets
jpeg(file = "visualizations/working_capital_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
hist(data$working_capital_by_total_assets, breaks=50,
     main="working_capital_by_total_assets")
hist(data$log_working_capital_by_total_assets, breaks=50,
     main="log(working_capital_by_total_assets)")
hist(data$trunc_working_capital_by_total_assets, breaks=50,
     main="trunc(working_capital_by_total_assets)")
boxplot(data$working_capital_by_total_assets,
        main="working_capital_by_total_assets")
boxplot(data$log_working_capital_by_total_assets,
        main="log(working_capital_by_total_assets)")
boxplot(data$trunc_working_capital_by_total_assets,
        main="trunc(working_capital_by_total_assets)")
dev.off()

# Size classification
jpeg(file = "visualizations/size_classification.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(1, 1))
par(mar=c(3, 3, 3, 3))
barplot(table(data$size_classification))
dev.off()

# income_tax_by_total_assets
jpeg(file = "visualizations/income_tax_by_total_assets.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
hist(data$income_tax_by_total_assets, breaks=50,
     main="income_tax_by_total_assets")
hist(data$log_income_tax_by_total_assets, breaks=50,
     main="log(income_tax_by_total_assets)")
hist(data$trunc_income_tax_by_total_assets, breaks=50,
     main="trunc(income_tax_by_total_assets)")
boxplot(data$income_tax_by_total_assets,
        main="income_tax_by_total_assets")
boxplot(data$log_income_tax_by_total_assets,
        main="log(income_tax_by_total_assets)")
boxplot(data$trunc_income_tax_by_total_assets,
        main="trunc(income_tax_by_total_assets)")
dev.off()

# n_employees
jpeg(file = "visualizations/n_employees.jpeg",
     width = 3000, height = 3000, res = 300)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
hist(data$n_employees, breaks=50,
     main="n_employees")
hist(data$log_n_employees, breaks=50,
     main="log(n_employees)")
boxplot(data$n_employees,
        main="n_employees")
boxplot(data$log_n_employees,
        main="log(n_employees)")
dev.off()

# --- Stepwise variable selection using AIC

# [x] total_liabilities                         -> do not use this variable
# [x] cash_by_total_assets                      -> use the log
# [x] cash_by_current_liabilities               -> use the log
# [x] current_assets_by_current_liabilities     -> use the log
# [x] tax                                       -> use the truncation
# [x] EBITDA                                    -> use the truncation
# [x] roa                                       -> use the truncation
# [x] financial_expenses_by_total_assets        -> use the truncation
# [x] EBIT                                      -> use the truncation
# [x] current_liabilites_by_total_liabilities   -> use the log
# [x] working_capital_by_total_assets           -> use the truncation
# [x] size_classification
# [x] income_tax_by_total_assets                -> use the truncation
# [x] sector
# [x] n_employees                               -> use the log
# [x] cooperative	nonprofit	other	private	public

AIC_df = data.frame()
frailty_df = data.frame()

# Iteration 1
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 1", fit.AIC))
names(AIC_df) <- c("Variable", "iteration", "AIC")

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
                   trunc_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_tax", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   trunc_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBITDA", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 1", fit.AIC))

fit.1 <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + cluster(sector),
                 data = data)
fit.1.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_liabilities_by_total_liabilities", "Iteration 1", fit.1.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 1 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 1", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 1", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 1",]
#                                        Variable   iteration      AIC
# 1                      log_cash_by_total_assets Iteration 1 60132.47
# 2               log_cash_by_current_liabilities Iteration 1 59274.14
# 3     log_current_assets_by_current_liabilities Iteration 1 59256.05
# 4                                     trunc_tax Iteration 1 59721.04
# 5                                  trunc_EBITDA Iteration 1 59715.16
# 6                                     trunc_roa Iteration 1 59669.94
# 7      trunc_financial_expenses_by_total_assets Iteration 1 59682.65
# 8                                    trunc_EBIT Iteration 1 59846.10
# 9  log_current_liabilities_by_total_liabilities Iteration 1 58550.59 <-
# 10        trunc_working_capital_by_total_assets Iteration 1 60340.60
# 11                                size_dummmies Iteration 1 59938.95
# 12               log_income_tax_by_total_assets Iteration 1 60584.69
# 13                              log_n_employees Iteration 1 60389.96
# 14                                 type_dummies Iteration 1 60406.98

# Iteration 2
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_tax", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBITDA", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 2 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 2", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 2", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 2",]
#                                     Variable   iteration      AIC
# 1                   log_cash_by_total_assets Iteration 2 58266.40
# 2            log_cash_by_current_liabilities Iteration 2 58205.89
# 3  log_current_assets_by_current_liabilities Iteration 2 58552.23
# 4                                  trunc_tax Iteration 2 57958.30 <-
# 5                               trunc_EBITDA Iteration 2 58049.40
# 6                                  trunc_roa Iteration 2 58459.50
# 7   trunc_financial_expenses_by_total_assets Iteration 2 58441.74
# 8                                 trunc_EBIT Iteration 2 58137.67
# 9      trunc_working_capital_by_total_assets Iteration 2 58549.27
# 10                             size_dummmies Iteration 2 58043.87
# 11            log_income_tax_by_total_assets Iteration 2 58550.13
# 12                           log_n_employees Iteration 2 58373.72
# 13                              type_dummies Iteration 2 58404.85

# Iteration 3
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_current_liabilities", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + trunc_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBITDA", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 5 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 3 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 3", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 3", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 3",]
#                                     Variable   iteration      AIC
# 15                  log_cash_by_total_assets Iteration 3 57690.88
# 16           log_cash_by_current_liabilities Iteration 3 57628.30 <-
# 17 log_current_assets_by_current_liabilities Iteration 3 57954.01
# 18                              trunc_EBITDA Iteration 3 57788.77
# 19                                 trunc_roa Iteration 3 57918.05
# 20  trunc_financial_expenses_by_total_assets Iteration 3 57884.06
# 21                                trunc_EBIT Iteration 3 57838.02
# 22     trunc_working_capital_by_total_assets Iteration 3 57957.47
# 23                             size_dummmies Iteration 3 57768.37
# 24            log_income_tax_by_total_assets Iteration 3 57917.29
# 25                           log_n_employees Iteration 3 57918.36
# 26                              type_dummies Iteration 3 57843.57

# Iteration 4
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + trunc_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBITDA", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 6 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("size_dummmies", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 4 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 4", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 4", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 4",]
#                                     Variable   iteration      AIC
# 27                  log_cash_by_total_assets Iteration 4 57554.04
# 28 log_current_assets_by_current_liabilities Iteration 4 57589.40
# 29                              trunc_EBITDA Iteration 4 57450.57
# 30                                 trunc_roa Iteration 4 57592.68
# 31  trunc_financial_expenses_by_total_assets Iteration 4 57583.88
# 32                                trunc_EBIT Iteration 4 57526.64
# 33     trunc_working_capital_by_total_assets Iteration 4 57615.03
# 34                             size_dummmies Iteration 4 57356.39 <-
# 35            log_income_tax_by_total_assets Iteration 4 57561.09
# 36                           log_n_employees Iteration 4 57569.48
# 37                              type_dummies Iteration 4 57502.79

# Iteration 5
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBITDA", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 7 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 5", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 10 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 5", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 5",]
#                                     Variable   iteration      AIC
# 48                  log_cash_by_total_assets Iteration 5 57211.71
# 49 log_current_assets_by_current_liabilities Iteration 5 57329.19
# 50                              trunc_EBITDA Iteration 5 57193.05 <-
# 51                                 trunc_roa Iteration 5 57326.70
# 52  trunc_financial_expenses_by_total_assets Iteration 5 57329.30
# 53                                trunc_EBIT Iteration 5 57201.85
# 54     trunc_working_capital_by_total_assets Iteration 5 57342.57
# 55            log_income_tax_by_total_assets Iteration 5 57318.50
# 56                           log_n_employees Iteration 5 57342.69
# 57                              type_dummies Iteration 5 57280.26

# Iteration 6
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 8 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 6", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 11 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("type_dummies", "Iteration 6", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 6",]
#                                     Variable   iteration      AIC
# 58                  log_cash_by_total_assets Iteration 6 57052.71 <- (but model of fit changes; coef sign flip)
# 59 log_current_assets_by_current_liabilities Iteration 6 57177.49
# 60                                 trunc_roa Iteration 6 57189.99
# 61  trunc_financial_expenses_by_total_assets Iteration 6 57168.72
# 62                                trunc_EBIT Iteration 6 57184.26
# 63     trunc_working_capital_by_total_assets Iteration 6 57182.23
# 64            log_income_tax_by_total_assets Iteration 6 57157.40
# 65                           log_n_employees Iteration 6 57178.52
# 66                              type_dummies Iteration 6 57099.50 <- (second best choice; and nice for interpretation)

# Iteration 7
fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + log_cash_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_cash_by_total_assets", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + log_current_assets_by_current_liabilities
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_current_assets_by_current_liabilities", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + trunc_roa
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_roa", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + trunc_financial_expenses_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_financial_expenses_by_total_assets", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + trunc_EBIT
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_EBIT", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + trunc_working_capital_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("trunc_working_capital_by_total_assets", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + trunc_income_tax_by_total_assets
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_income_tax_by_total_assets", "Iteration 7", fit.AIC))

fit <- emfrail(Surv(years_to_event, censor) ~
                   log_current_liabilities_by_total_liabilities
                   + trunc_tax
                   + log_cash_by_current_liabilities
                   + size_small + size_large + size_v_large
                   + trunc_EBITDA
                   + cooperative + nonprofit + other + private
                   + log_n_employees
                   + cluster(sector),
                 data = data)
fit.AIC <- 2 * 12 - 2 * fit$loglik[2]
AIC_df <- rbind(AIC_df, list("log_n_employees", "Iteration 7", fit.AIC))

AIC_df[AIC_df$iteration=="Iteration 7",]
#                                     Variable   iteration      AIC
# 67                  log_cash_by_total_assets Iteration 7 56952.39 
# 68 log_current_assets_by_current_liabilities Iteration 7 57080.98
# 69                                 trunc_roa Iteration 7 57094.82
# 70  trunc_financial_expenses_by_total_assets Iteration 7 57075.66
# 71                                trunc_EBIT Iteration 7 57087.15
# 72     trunc_working_capital_by_total_assets Iteration 7 57088.24
# 73            log_income_tax_by_total_assets Iteration 7 57069.05
# 74                           log_n_employees Iteration 7 57083.72

# --- The model
fit <- emfrail(Surv(years_to_event, censor) ~
                 log_current_liabilities_by_total_liabilities
               + trunc_tax
               + log_cash_by_current_liabilities
               + size_small + size_large + size_v_large
               + trunc_EBITDA
               + cooperative + nonprofit + other + private
               + cluster(sector),
               data = data)
fit
# Call: 
#   emfrail(formula = Surv(years_to_event, censor) ~ log_current_liabilities_by_total_liabilities + 
#             trunc_tax + log_cash_by_current_liabilities + size_small + 
#             size_large + size_v_large + trunc_EBITDA + cooperative + 
#             nonprofit + other + private + cluster(sector), data = data)
# 
# log-likelihood: -28538.75 
# frailty variance: 0.1942132 
# theta: 5.148982 
# 
#                                         coef        exp(coef)         se(coef)   
# log_current_liabilities_by_total_liabilities   0.485625320705   1.625190956720   
# trunc_tax                                     -0.000007347273   0.999992652754   
# log_cash_by_current_liabilities               -0.175362207689   0.839153018041   
# size_small                                     0.520564155530   1.682976842620   
# size_large                                    -0.629913600896   0.532637818449   
# size_v_large                                  -2.225695297898   0.107992305702   
# trunc_EBITDA                                  -0.000001104077   0.999998895923   
# cooperative                                    0.130654818703   1.139574353661   
# nonprofit                                     -1.423540754689   0.240859680249   
# other                                          0.636267445803   1.889415356066   
# private                                        0.526676224329   1.693294812821   
# 
#        adj. se                z      p
# 0.017969001781   0.017973239805  27.019353548890 0.0000
# 0.000000839221   0.000000839495  -8.752014050674 0.0000
# 0.007540675480   0.007540999129 -23.254505759748 0.0000
# 0.057343678779   0.057346800366   9.077475154854 0.0000
# 0.155316900707   0.155318202183  -4.055632836601 0.0000
# 0.511138930939   0.511144681150  -4.354335240056 0.0000
# 0.000000075048   0.000000075048 -14.711690729879 0.0000
# 0.146999934109   0.147009556593   0.888750512081 0.3741
# 0.583472906265   0.583475405371  -2.439761370548 0.0147
# 0.582324103107   0.582340496545   1.092603810961 0.2746
# 0.065365280797   0.065365441601   8.057410941208 0.0000
# 
# Score test for heterogeneity: p-val 0.000000000000000000000000000000000000000000000000000000000000000000000358

fit$frail
# K - Financial and insurance activities 
# 0.4011734 
# Q - Human health and social work activities 
# 0.5901528 
# L - Real estate activities 
# 0.6006089 
# A - Agriculture, forestry and fishing 
# 0.6147331 
# E - Water supply; sewerage, waste management and remediation activities 
# 0.6752981 
# M - Professional, scientific and technical activities 
# 0.6972201 
# C - Manufacturing 
# 0.7403557 
# G - Wholesale and retail trade; repair of motor vehicles and motorcycles 
# 0.8026844 
# O - Public administration and defence; compulsory social security 
# 0.8856265 
# B - Mining and quarrying 
# 0.9228458 
# T - Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use 
# 0.9848851 
# P - Education 
# 1.0149337 
# D - Electricity, gas, steam and air conditioning supply 
# 1.0474577 
# S - Other service activities 
# 1.1252522 
# J - Information and communication 
# 1.1591249 
# R - Arts, entertainment and recreation 
# 1.1912897 
# N - Administrative and support service activities 
# 1.2897014 
# F - Construction 
# 1.4069089 
# H - Transportation and storage 
# 1.6875412 
# I - Accommodation and food service activities 
# 2.0554311

table(data$sector)
# A - Agriculture, forestry and fishing 
# 318 
# B - Mining and quarrying 
# 12 
# C - Manufacturing 
# 2376 
# D - Electricity, gas, steam and air conditioning supply 
# 25 
# E - Water supply; sewerage, waste management and remediation activities 
# 102 
# F - Construction 
# 4492 
# G - Wholesale and retail trade; repair of motor vehicles and motorcycles 
# 7016 
# H - Transportation and storage 
# 1282 
# I - Accommodation and food service activities 
# 2371 
# J - Information and communication 
# 1121 
# K - Financial and insurance activities 
# 1290 
# L - Real estate activities 
# 755 
# M - Professional, scientific and technical activities 
# 3475 
# N - Administrative and support service activities 
# 1542 
# O - Public administration and defence; compulsory social security 
# 10 
# P - Education 
# 112 
# Q - Human health and social work activities 
# 909 
# R - Arts, entertainment and recreation 
# 360 
# S - Other service activities 
# 563 
# T - Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use 
# 4

# --- Other things

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
































