library(survival)
library(survminer)
library(smcure)
library(cmprsk)
options(scipen = 999)

data <- read.csv("data/bankruptcy_transformed.csv")
data <- na.omit(data)
data$censor <- ifelse(data$event_indicator == 2, 1, 0)

# Plot survival curve
fit <- survfit(Surv(years_to_event, censor)~1, data = data)
jpeg(file = "visualizations/km_bankruptcy.jpeg")
ggsurvplot(fit, data = data)
dev.off()

# Cox PH model
cox0 <- coxph(Surv(years_to_event, censor) ~ mean_pl + mean_total_assets
             + mean_n_employees + cooperative + nonprofit + other + private,
             data = data)

coefficients_cox <- cox0$coefficients
stand_e_cox <- sqrt(diag(cox0$var))
p_value_cox <- 2 * (1 - pnorm(abs(coefficients_cox / stand_e_cox),
                  lower.tail = TRUE))

cox_summary <- data.frame(coefficients_cox, stand_e_cox, p_value_cox)
write.csv(cox_summary, "data/cox_summary.csv")

# Population mixture cure model
# smcure(Surv(years_to_event, censor)~mean_pl+mean_total_assets,
#        cureform=~mean_pl+mean_total_assets, model="ph", Var=TRUE,
#        data=data)

# Competing risk Cox PH on the subdistribution hazard
cov <- cbind(data$mean_pl, data$mean_total_assets, data$mean_n_employees,
             data$cooperative, data$nonprofit, data$other, data$private)
dimnames(cov)[[2]] <- c("Mean profit/loss", "Mean total assets",
                        "Mean number of employees", "Cooperative", "Nonprofit",
                        "Other", "Private")

c0 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 0)
c1 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 1)
c2 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 2)

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
  coeff_bankruptcy, se_bankruptcy
)
write.csv(competing_summary, "data/competing_summary.csv")

# Create a plot of survival probabilities
pc0 <- predict(c0, rbind(c(0, 0, 0, 1, 0, 0, 0), c(0, 0, 0, 0, 1, 0, 0),
                         c(0, 0, 0, 0, 0, 1, 0), c(0, 0, 0, 0, 0, 0, 1)))
pc1 <- predict(c1, rbind(c(0, 0, 0, 1, 0, 0, 0), c(0, 0, 0, 0, 1, 0, 0),
                         c(0, 0, 0, 0, 0, 1, 0), c(0, 0, 0, 0, 0, 0, 1)))
pc2 <- predict(c2, rbind(c(0, 0, 0, 1, 0, 0, 0), c(0, 0, 0, 0, 1, 0, 0),
                         c(0, 0, 0, 0, 0, 1, 0), c(0, 0, 0, 0, 0, 0, 1)))

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")

jpeg(file = "visualizations/temp.jpeg", width = 4000, height = 1500, res = 300)
par(mfrow = c(1, 3))
plot(pc0, col = colors, lty = 1, ylim = c(0, 1),
     main = "Liquidation, relative to the public sector",
     xlab = "Time", ylab = "Probability", lwd = 2)
legend(0, 1, c("Cooperative", "Nonprofit", "Other", "Private"),
       col = colors, lty = 1)
plot(pc1, col = colors, lty = 1, ylim = c(0, 1),
     main = "Merger, relative to the public sector",
     xlab = "Time", ylab = "Probability", lwd = 2)
legend(0, 1, c("Cooperative", "Nonprofit", "Other", "Private"),
       col = colors, lty = 1)
plot(pc2, col = colors, lty = 1, ylim = c(0, 1),
     main = "Bankruptcy, relative to the public sector",
     xlab = "Time", ylab = "Probability", lwd = 2)
legend(0, 1, c("Cooperative", "Nonprofit", "Other", "Private"),
       col = colors, lty = 1)
dev.off()