# DATA
earnings = c(45617,7166,18594,2236,1278,19828,4033,28151,2414,3800);
earnings.sd = 15000;

# A
earnings.mean = mean(earnings);
earnings.se = earnings.sd / sqrt(length(earnings));

cat("Earnings mean: ", earnings.mean, "\n");
cat("Earnings standard derivation: ", earnings.sd, "\n");
cat("Earnings standard error: ", earnings.se, "\n");

earnings.interval1 = 0.1;
earnings.ci1.margin = qnorm(p = (1 - earnings.interval1 / 2)) * earnings.se;
earnings.ci1 = earnings.mean + c(-1,1) * earnings.ci1.margin;

print("Confidence interval for 0.9 assuming normal distribution:");
print(earnings.ci1);

earnings.interval2 = 0.05;
earnings.ci2.margin = qnorm(p = (1 - earnings.interval2 / 2)) * earnings.se;
earnings.ci2 = earnings.mean + c(-1,1) * earnings.ci2.margin;

print("Confidence interval for 0.95 assuming normal distribution:");
print(earnings.ci2);

# B
earnings.sd = sd(earnings);
earnings.se = earnings.sd / sqrt(length(earnings));

cat("Earnings standard derivation: ", earnings.sd, "\n");
cat("Earnings standard error: ", earnings.se, "\n");

earnings.interval1 = 0.1;
earnings.ci1.margin = qt(p = (1 - earnings.interval1 / 2),
                         df = length(earnings) - 1) * earnings.se;
earnings.ci1 = earnings.mean + c(-1,1) * earnings.ci1.margin;

print("Confidence interval for 0.9 assuming Student's t-distribution:");
print(earnings.ci1);

earnings.interval2 = 0.05;
earnings.ci2.margin = qt(p = (1 - earnings.interval2 / 2),
                         df = length(earnings) - 1) * earnings.se;
earnings.ci2 = earnings.mean + c(-1,1) * earnings.ci2.margin;

print("Confidence interval for 0.95 assuming Student's t-distribution:");
print(earnings.ci2);

# C
my_bootstrap = function(data) {
    n = length(data);
    means = c();

    for (i in 1:10000) {
        rands = sample(1:n, n, replace = T);
        xs = data[rands];

        means = append(means, mean(xs));
    }

    return(means);
}

png(file = "plots/earnings_hist.png");
hist(earnings);
dev.off();

png(file = "plots/earnings_qq.png");
qqnorm(earnings, main = "Q-Q Plot of earnings");
qqline(earnings);
dev.off();

earnings.bootstrap = my_bootstrap(earnings);
earnings.bootstrap.mean = mean(earnings.bootstrap);

cat("Earnings mean (bootstrap): ", earnings.bootstrap.mean, "\n");

print("Confidence interval for 0.9:");
quantile(earnings.bootstrap, probs = c(0.05, 0.95));
print("Confidence interval for 0.95:");
quantile(earnings.bootstrap, probs = c(0.025, 0.975));

# 2
library(MASS);

adv_bootstrap = function(data, f) {
    n = length(data);
    result = c();

    for (i in 1:10000) {
        rands = sample(1:n, n, replace = T);
        xs = data[rands];

        result = append(result, f(xs));
    }

    return(result);
}

# CASE 1

png(file = "plots/shrimps_qq.png");
qqnorm(shrimp, main = "Q-Q Plot of shrimps in cocktails");
qqline(shrimp);
dev.off();

shrimps.bootstrap.mean = adv_bootstrap(shrimp, mean);
shrimps.bootstrap.sd = adv_bootstrap(shrimp, sd);
shrimps.bootstrap.var = adv_bootstrap(shrimp, var);

print("Confidence intervals for 0.95 for shrimps:");
print("Mean:");
quantile(shrimps.bootstrap.mean, probs = c(0.025, 0.975));
print("Standard deviation:");
quantile(shrimps.bootstrap.sd, probs = c(0.025, 0.975));
print("Standard error:");
quantile(shrimps.bootstrap.var, probs = c(0.025, 0.975));

# CASE 2
png(file = "plots/npkyield_qq.png");
qqnorm(npk$yield, main = "Q-Q Plot of yield of peas");
qqline(npk$yield);
dev.off();

npkyield.bootstrap.mean = adv_bootstrap(npk$yield, mean);
npkyield.bootstrap.sd = adv_bootstrap(npk$yield, sd);
npkyield.bootstrap.var = adv_bootstrap(npk$yield, var);

print("Confidence intervals for 0.95 for npk yield:");
print("Mean:");
quantile(npkyield.bootstrap.mean, probs = c(0.025, 0.975));
print("Standard deviation:");
quantile(npkyield.bootstrap.sd, probs = c(0.025, 0.975));
print("Standard error:");
quantile(npkyield.bootstrap.var, probs = c(0.025, 0.975));

# CASE 3
png(file = "plots/quineabsent_qq.png");
qqnorm(quine$Days, main = "Q-Q Plot of days absent from school");
qqline(quine$Days);
dev.off();

quineabsent.bootstrap.mean = adv_bootstrap(quine$Days, mean);
quineabsent.bootstrap.sd = adv_bootstrap(quine$Days, sd);
quineabsent.bootstrap.var = adv_bootstrap(quine$Days, var);

print("Confidence intervals for 0.95 for absent days:");
print("Mean:");
quantile(quineabsent.bootstrap.mean, probs = c(0.025, 0.975));
print("Standard deviation:");
quantile(quineabsent.bootstrap.sd, probs = c(0.025, 0.975));
print("Standard error:");
quantile(quineabsent.bootstrap.var, probs = c(0.025, 0.975));
