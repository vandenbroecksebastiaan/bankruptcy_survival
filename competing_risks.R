library(survival)
library(survminer)
library(smcure)
library(cmprsk)
library(plm)
library(corrplot)
options(scipen = 999)



# Competing risk Cox PH on the sub-distribution hazard
cov <- cbind(data$log_current_liabilities_by_total_liabilities,
             data$trunc_tax,
             data$log_cash_by_current_liabilities,
             data$size_small,
             data$size_large,
             data$size_v_large,
             data$trunc_EBITDA,
             data$cooperative,
             data$nonprofit,
             data$other,
             data$private)

dim(cov)
dimnames(cov)[[2]] <- c("log_current_liabilities_by_total_liabilities",
                        "trunc_tax",
                        "log_cash_by_current_liabilities",
                        "size_small",
                        "size_large",
                        "size_v_large",
                        "trunc_EBITDA",
                        "cooperative",
                        "nonprofit",
                        "other",
                        "private")

corrplot(cor(cov))


# c0 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 0)
# c1 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 1)
c2 <- crr(data$years_to_event, data$event_indicator, cov, failcode = 2)

c2$coef
2 * (1 - pnorm(abs(c2$coef / sqrt(diag(c2$var))), lower.tail = TRUE))

stop(0)

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