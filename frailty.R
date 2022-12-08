library(dplyr)
library(survival)
library(parfm)
library(frailtySurv)
options(scipen = 999)

data <- read.csv("data/bankruptcy_transformed.csv")
data <- na.omit(data)
data$censor <- ifelse(data$event_indicator == 2, 1, 0)

cat("\n"); print(colnames(data)); print(dim(data)); cat("\n")

sel <- select.parfm(Surv(years_to_event, censor) ~ .,
                    cluster = "size", data = data,
                    dist = c("exponential", "weibull", "lognormal"),
                    frailty = c("gamma", "ingau", "possta"))
sel

stop(0)

fit <- parfm(Surv(years_to_event, censor) ~ mean_pl,
             cluster = "type", data = data,
             dist = "exponential", frailty = "gamma")
fit