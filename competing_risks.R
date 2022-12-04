library(survival)
library(survminer)
library(smcure)
library(cmprsk)
options(scipen=999)

data <- read.csv("data/bankruptcy_transformed.csv")
data <- na.omit(data)
data$censor <- ifelse(data$event_indicator==3, 1, 0)

head(data)
table(data$status, data$event_indicator)
cat("\n")

# Plot survival curve
fit <- survfit(Surv(years_to_event, censor)~1, data=data)
jpeg(file="visualizations/km_bankruptcy.jpeg")
ggsurvplot(fit, data=data)
dev.off()

# Cox PH model
# coxph(Surv(years_to_event, censor)~mean_pl+mean_total_assets, data=data)

# Population mixture cure model
# smcure(Surv(years_to_event, censor)~mean_pl+mean_total_assets,
#        cureform=~mean_pl+mean_total_assets, model="ph", Var=TRUE,
#        data=data)

# Competing risk Cox PH on the subdistribution hazard
cov <- cbind(data$mean_pl, data$mean_total_assets, data$mean_n_employees,
             data$cooperative, data$nonprofit, data$other, data$private)
dimnames(cov)[[2]] = c("Mean profit/loss", "Mean total assets",
                       "Mean number of employees", "Cooperative","Nonprofit",
                       "Other", "Private")

c0 <- crr(data$years_to_event, data$event_indicator, cov, failcode=0)
c1 <- crr(data$years_to_event, data$event_indicator, cov, failcode=1)
c2 <- crr(data$years_to_event, data$event_indicator, cov, failcode=2)

pc0 <- predict(c0, rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))
pc1 <- predict(c1, rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))
pc2 <- predict(c2, rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))

cat("Liquidation\n\n"); c0; cat("\n");
cat("Merger\n\n"); c1; cat("\n");
cat("Bankruptcy\n\n"); c2; cat("\n");

colors = c("black", "green", "blue", "red")

jpeg(file="visualizations/temp.jpeg", width=2000, height=2000, res=200)
par(mfrow=c(2, 2))
plot(pc0, col=colors, lty=1, ylim=c(0,0.8), main="Liquidation",
     xlab="Time",ylab="Probability")
plot(pc1, col=colors, lty=1, ylim=c(0,0.8), main="Merger",
     xlab="Time",ylab="Probability")
plot(pc2, col=colors, lty=1, ylim=c(0,0.8), main="Bankruptcy",
     xlab="Time",ylab="Probability")
dev.off()
