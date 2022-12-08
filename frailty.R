library(dplyr)
library(survival)
library(parfm)
library(frailtySurv)
options(scipen = 999)

data <- read.csv("data/bankruptcy_transformed.csv")
data <- na.omit(data)
data$censor <- ifelse(data$event_indicator == 2, 1, 0)
data$type <- ifelse(data$cooperative == 1, "Cooperative",
                    ifelse(data$nonprofit == 1, "Nonprofit",
                           ifelse(data$other == 1, "Other",
                                  ifelse(data$private == 1, "Private",
                                         "Public"))))

table(data$type)

cat("\n"); print(colnames(data)); print(dim(data)); cat("\n")

# sel <- select.parfm(Surv(years_to_event, censor) ~
#                     current_assets_by_current_liabilities,
#                     cluster = "size_classification", data = data,
#                     dist = c("exponential", "weibull", "lognormal",
#                              "loglogistic"),
#                     frailty = c("gamma", "ingau", "possta", "lognormal"))
# sel

fit1 <- coxph(Surv(years_to_event, censor) ~
              current_assets_by_current_liabilities
              + frailty(size_classification, dist = "gamma"), data = data)
fit1


# fit2 <- fitfrail(Surv(years_to_event, censor) ~
#                      current_assets_by_current_liabilities
#                      + cluster(size_classification),
#                 dat = data,
#                 frailty = "gamma")
# fit2