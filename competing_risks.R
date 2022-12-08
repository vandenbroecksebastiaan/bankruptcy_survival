library(survival)
library(survminer)
library(smcure)
library(cmprsk)
library(plm)
options(scipen = 999)

data <- read.csv("data/bankruptcy_transformed.csv")
data <- na.omit(data)
print(colnames(data))
data$censor <- ifelse(data$event_indicator == 2, 1, 0)

# Kendall's tau
# cor.test(data$years_to_event, data$event_indicator, method="kendall")

# Plot survival curve
# fit <- survfit(Surv(years_to_event, censor)~1, data = data)
# jpeg(file = "visualizations/km_bankruptcy.jpeg")
# ggsurvplot(fit, data = data)

# Cox PH model
# cox0 <- coxph(Surv(years_to_event, censor) ~
#               total_liabilities_by_net_assets +
#             # cash_by_total_assets +
#             # cash_by_current_liabilities +
#               current_assets_by_current_liabilities +
#               tax +
#               EBTIDA +
#               financial_expenses_by_total_assets +
#               EBIT +
#               working_capital_by_total_assets +
#               income_tax_by_total_assets +
#               cooperative +
#               nonprofit +
#               other +
#               private,
#              data = data)


# cox0

# coefficients_cox <- cox0$coefficients
# stand_e_cox <- sqrt(diag(cox0$var))
# p_value_cox <- 2 * (1 - pnorm(abs(coefficients_cox / stand_e_cox),
#                   lower.tail = TRUE))

# cox_summary <- data.frame(coefficients_cox, stand_e_cox, p_value_cox)
# write.csv(cox_summary, "data/cox_summary.csv")

# Population mixture cure model
# smcure(Surv(years_to_event, censor)~mean_pl+mean_total_assets,
#        cureform=~mean_pl+mean_total_assets, model="ph", Var=TRUE,
#        data=data)

# Competing risk Cox PH on the subdistribution hazard
cov <- cbind(# data$total_liabilities_by_net_assets,
           # data$cash_by_total_assets,
           # data$cash_by_current_liabilities,
             data$current_assets_by_current_liabilities,
             data$tax,
           # data$EBTIDA,
             data$financial_expenses_by_total_assets,
           # data$EBIT,
             data$working_capital_by_total_assets,
             data$income_tax_by_total_assets,
             data$cooperative,
             data$nonprofit,
             data$other,
             data$private)
dim(cov)
dimnames(cov)[[2]] <- c(# "Total liabilities by net assets",
                      # "Cash by total assets",
                      # "Cash by current liabilities",
                        "Current assets by current liabilities",
                        "Tax",
                      # "EBITDA",
                        "Financial expenses by total assets",
                      # "EBIT",
                        "Working capital by total assets",
                        "Income tax by total assets",
                        "Cooperative",
                        "Nonprofit",
                        "Other",
                        "Private")

# c0 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 0)
# c1 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 1)
c2 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 2)

c2$coef
2 * (1 - pnorm(abs(c2$coef / sqrt(diag(c2$var))), lower.tail = TRUE))

stop(0)

# Create a table of coefficients
coeff_liquidation <- c0$coef
se_liquidation <- sqrt(diag(c0$var))
p_value_liquidation <- 2 * (1 - pnorm(abs(coeff_liquidation / se_liquidation),
                                      lower.tail = TRUE))

coeff_merger <- c1$coef
se_merger <- sqrt(diag(c1$var))
p_value_merger <- 2 * (1 -  pnorm(abs(coeff_merger / se_merger),
                                  lower.tail = TRUE))

coeff_bankruptcy <- c2$coef
se_bankruptcy <- sqrt(diag(c2$var))
p_value_bankruptcy <- 2 * (1 - pnorm(abs(coeff_bankruptcy / se_bankruptcy),
                                     lower.tail = TRUE))

competing_summary <- data.frame(
  coeff_liquidation, se_liquidation, p_value_liquidation,
  coeff_merger, se_merger, p_value_merger,
  coeff_bankruptcy, se_bankruptcy, p_value_bankruptcy
)
write.csv(competing_summary, "data/competing_summary.csv")

# Create a plot of survival probabilities
x_cooperative_retail        <- c(1, 0, 0, 0, 1, 0, 0)
x_cooperative_services      <- c(1, 0, 0, 0, 0, 1, 0)
x_cooperative_wholesale     <- c(1, 0, 0, 0, 0, 0, 1)
x_cooperative_manufacturing <- c(1, 0, 0, 0, 0, 0, 0)

x_nonprofit_retail        <- c(0, 1, 0, 0, 1, 0, 0)
x_nonprofit_services      <- c(0, 1, 0, 0, 0, 1, 0)
x_nonprofit_wholesale     <- c(0, 1, 0, 0, 0, 0, 1)
x_nonprofit_manufacturing <- c(0, 1, 0, 0, 0, 0, 0)

x_other_retail        <- c(0, 0, 1, 0, 1, 0, 0)
x_other_services      <- c(0, 0, 1, 0, 0, 1, 0)
x_other_wholesale     <- c(0, 0, 1, 0, 0, 0, 1)
x_other_manufacturing <- c(0, 0, 1, 0, 0, 0, 0)

x_private_retail        <- c(0, 0, 0, 1, 1, 0, 0)
x_private_services      <- c(0, 0, 0, 1, 0, 1, 0)
x_private_wholesale     <- c(0, 0, 0, 1, 0, 0, 1)
x_private_manufacturing <- c(0, 0, 0, 1, 0, 0, 0)

x_public_retail        <- c(0, 0, 0, 0, 1, 0, 0)
x_public_services      <- c(0, 0, 0, 0, 0, 1, 0)
x_public_wholesale     <- c(0, 0, 0, 0, 0, 0, 1)
x_public_manufacturing <- c(0, 0, 0, 0, 0, 0, 0)

x_cooperative_mat <- rbind(x_cooperative_retail, x_cooperative_services,
                           x_cooperative_wholesale, x_cooperative_manufacturing)
x_nonprofit_mat <- rbind(x_nonprofit_retail, x_nonprofit_services,
                         x_nonprofit_wholesale, x_nonprofit_manufacturing)
x_other_mat <- rbind(x_other_retail, x_other_services,
                     x_other_wholesale, x_other_manufacturing)
x_private_mat <- rbind(x_private_retail, x_private_services,
                       x_private_wholesale, x_private_manufacturing)
x_public_mat <- rbind(x_public_retail, x_public_services,
                      x_public_wholesale, x_public_manufacturing)

prob_bankruptcy_cooperative <- predict(c2, x_cooperative_mat)
prob_bankruptcy_nonprofit <- predict(c2, x_nonprofit_mat)
prob_bankruptcy_other <- predict(c2, x_other_mat)
prob_bankruptcy_private <- predict(c2, x_private_mat)
prob_bankruptcy_public <- predict(c2, x_public_mat)

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
labels <- c("Retail", "Services", "Wholesale", "Manufacturing")

jpeg(file = "visualizations/temp.jpeg", width = 2500, height = 2000, res = 300)
par(mfrow = c(2, 3))

plot(prob_bankruptcy_private, col = colors, ylim = c(0, 1), lwd = 3,
     lty = 1, main = "Private (not significant)", xlab = "Time in years",
     ylab = "Probability")
legend(0, 1, labels, col = colors, lty = 1)

plot(prob_bankruptcy_public, col = colors, ylim = c(0, 1), lwd = 3,
     lty = 1, main = "Public", xlab = "Time in years",
     ylab = "Probability")
legend(0, 1, labels, col = colors, lty = 1)

plot(prob_bankruptcy_cooperative, col = colors, ylim = c(0, 1), lwd = 3,
     lty = 1, main = "Cooperative", xlab = "Time in years",
     ylab = "Probability")
legend(0, 1, labels, col = colors, lty = 1)

plot(prob_bankruptcy_nonprofit, col = colors, ylim = c(0, 1), lwd = 3,
     lty = 1, main = "Nonprofit", xlab = "Time in years",
     ylab = "Probability")
legend(0, 1, labels, col = colors, lty = 1)

plot(prob_bankruptcy_other, col = colors, ylim = c(0, 1), lwd = 3,
     lty = 1, main = "Other", xlab = "Time in years",
     ylab = "Probability")
legend(0, 1, labels, col = colors, lty = 1)

dev.off()