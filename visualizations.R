# --- Read data
data <- read.csv("data/bankruptcy_transformed_alive_and_fail.csv")

# The last row can be empty
data <- na.omit(data)

# Events other than bankruptcy are considered censored
data$censor <- ifelse(data$event_indicator == 2, 1, 0)
data$censor_total <- ifelse(data$status=="alive", 1, 0)
data$censor_liq <- ifelse(data$event_indicator == 4, 1, 0)
data$censor_mer <- ifelse(data$event_indicator == 0, 1, 0)

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
upper_bound <- quantile(data$tax, probs=c(0.50)) + 10 * IQR(data$tax)
lower_bound <- quantile(data$tax, probs=c(0.50)) - 10 * IQR(data$tax)
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
upper_bound <- quantile(data$roa, probs=c(0.5)) + 10 * IQR(data$roa)
lower_bound <- quantile(data$roa, probs=c(0.5)) - 10 * IQR(data$roa)
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

# n_employees
data$log_n_employees = log(data$n_employees)

# We remove sectors with only 1 company and consider them as outliers
data <- data[data$years_to_event!=0, ]
data <- data %>%
  group_by(sector) %>%
  mutate(n=n()) %>%
  filter(n > 1) %>%
  select(-n)

# --- Make visualizations of the continuous variables
# total_liabilities
jpeg(file = "visualizations/total_liabilities.jpeg",
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(family="Times")
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
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
par(family="Times")
hist(data$cash_by_total_assets, breaks=50,
     main="cash_by_total_assets", ylab="frequency")
hist(data$log_cash_by_total_assets, breaks=50,
     main="log(cash_by_total_assets)", ylab="frequency")
boxplot(data$cash_by_total_assets,
     main="cash_by_total_assets")
boxplot(data$log_cash_by_total_assets,
     main="log(cash_by_total_assets)")
dev.off()

# cash_by_current_liabilities
jpeg(file = "visualizations/cash_by_current_liabilities.jpeg",
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(2, 2, 2, 2))
par(family="Times")
hist(data$cash_by_current_liabilities, breaks=50,
     main="cash_by_current_liabilities", ylab="frequency")
hist(data$log_cash_by_current_liabilities, breaks=50,
     main="log(cash_by_current_liabilities)", ylab="frequency")
boxplot(data$cash_by_current_liabilities,
     main="cash_by_current_liabilities")
boxplot(data$log_cash_by_current_liabilities,
     main="log(cash_by_current_liabilities)")
dev.off()

# current_assets_by_current_liabilities
jpeg(file = "visualizations/current_assets_by_current_liabilities.jpeg",
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 4500, height = 3000, res = 400)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 4500, height = 3000, res = 400)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 4500, height = 3000, res = 400)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 4500, height = 3000, res = 400)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 4500, height = 3000, res = 400)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 3000, height = 3000, res = 400)
par(mfrow=c(1, 1))
par(mar=c(3, 3, 3, 3))
par(family="Times")
barplot(table(data$size_classification), main = "Size classification")
dev.off()

# income_tax_by_total_assets
jpeg(file = "visualizations/income_tax_by_total_assets.jpeg",
     width = 4500, height = 3000, res = 400)
par(mfrow=c(2, 3))
par(mar=c(3, 3, 3, 3))
par(family="Times")
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
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(3, 3, 3, 3))
par(family="Times")
hist(data$n_employees, breaks=50,
     main="n_employees")
hist(data$log_n_employees, breaks=50,
     main="log(n_employees)")
boxplot(data$n_employees,
        main="n_employees")
boxplot(data$log_n_employees,
        main="log(n_employees)")
dev.off()

# Kaplan-Meier estimator
jpeg(file = "visualizations/KM_estimator.jpeg",
     width = 3000, height = 3000, res = 400)
par(mfrow=c(2, 2))
par(mar=c(4, 4, 4, 4))
par(family="Times")
plot(survfit(Surv(years_to_event, censor_total) ~ 1, data=data),
     main="All causes confounded", xlab="Years to event", ylab="KM estimator")
plot(survfit(Surv(years_to_event, censor_mer) ~ 1, data=data),
     main="Main event = merger", xlab="Years to event", ylab="KM estimator")
plot(survfit(Surv(years_to_event, censor_liq) ~ 1, data=data),
     main="Main event = liquidation", xlab="Years to event", ylab="KM estimator")
plot(survfit(Surv(years_to_event, censor) ~ 1, data=data),
     main="Main event = bankruptcy", xlab="Years to event", ylab="KM estimator")
dev.off()

# Sector clusters
jpeg(file = "visualizations/sector.jpeg",
     width = 4000, height = 3000, res = 400)
par(mfrow=c(1, 1))
par(mar=c(3, 30, 3, 3))
par(family="Times")
data$sector <- replace(data$sector, data$sector=="T - Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use", "T - Activities of households as employers")
barplot(sort(table(data$sector), decreasing = T), las=2, horiz=T)
dev.off()

# Event types
jpeg(file = "visualizations/event.jpeg",
     width = 4000, height = 3000, res = 400)
par(mfrow=c(1, 1))
par(mar=c(3, 20, 3, 3))
par(family="Times")
data$status <- replace(data$status, data$status=="alive", "Alive")
barplot(sort(table(data$status), decreasing = T), las=2,
        horiz=T, xlim=c(0, 20000), beside=T)
dev.off()

# Histogram of survival times
jpeg(file = "visualizations/survival_time.jpeg",
     width = 3000, height = 3000, res = 400)
par(mfrow=c(1, 1))
par(mar=c(3, 3, 3, 3))
par(family="Times")
hist(data$years_to_event, main="Years to event", breaks=50)
dev.off()

# Correlation plot
cor.mat <- cor(matrix(c(
  data$log_cash_by_total_assets,
  data$log_cash_by_current_liabilities,
  data$log_current_assets_by_current_liabilities,
  data$trunc_tax,
  data$trunc_EBITDA,
  data$trunc_roa,
  data$trunc_financial_expenses_by_total_assets,
  data$trunc_EBIT,
  data$log_current_liabilities_by_total_liabilities,
  data$trunc_working_capital_by_total_assets,
  data$log_income_tax_by_total_assets,
  data$log_n_employees), nrow=28135)
)
cor.mat <- round(cor.mat, 4)
cor.mat <- as.data.frame(cor.mat)
rownames(cor.mat) <- c(
 "1 log_cash_by_total_assets",
 "2 log_cash_by_current_liabilities",
 "3 log_current_assets_by_current_liabilities",
 "4 trunc_tax",
 "5 trunc_EBITDA",
 "6 trunc_roa",
 "7 trunc_financial_expenses_by_total_assets",
 "8 trunc_EBIT",
 "9 log_current_liabilities_by_total_liabilities",
 "10 trunc_working_capital_by_total_assets",
 "11 log_income_tax_by_total_assets",
 "12 log_n_employees"
)
colnames(cor.mat) <- c(
 "1",
 "2",
 "3",
 "4",
 "5",
 "6",
 "7",
 "8",
 "9",
 "10",
 "11",
 "12"
)
cor.mat