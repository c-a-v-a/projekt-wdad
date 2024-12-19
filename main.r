# DATA
earnings = c(45617,7166,18594,2236,1278,19828,4033,28151,2414,3800);
earnings.sd = 15000;

# A
earnings.mean = mean(earnings);
earnings.se = earnings.sd / sqrt(length(earnings));

# CONFIDENCE INTERVAL 0.9
earnings.interval1 = 0.1;
earnings.ci1.margin = qnorm(p = (1 - earnings.interval1 / 2)) * earnings.se;
earnings.ci1 = earnings.mean + c(-1,1) * earnings.ci1.margin;

# CONFIDENCE INTERVAL 0.95
earnings.interval2 = 0.05;
earnings.ci2.margin = qnorm(p = (1 - earnings.interval2 / 2)) * earnings.se;
earnings.ci2 = earnings.mean + c(-1,1) * earnings.ci2.margin;

# B
earnings.sd = sd(earnings);
earnings.se = earnings.sd / sqrt(length(earnings));

# CONFIDENCE INTERVAL 0.9
earnings.interval1 = 0.1;
earnings.ci1.margin = qt(p = (1 - earnings.interval1 / 2),
                         df = length(earnings) - 1) * earnings.se;
earnings.ci1 = earnings.mean + c(-1,1) * earnings.ci1.margin;

# CONFIDENCE INTERVAL 0.95
earnings.interval2 = 0.05;
earnings.ci2.margin = qt(p = (1 - earnings.interval2 / 2),
                         df = length(earnings) - 1) * earnings.se;
earnings.ci2 = earnings.mean + c(-1,1) * earnings.ci2.margin;

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

# hist
# qqplot

earnings.bootstrap = my_bootstrap(earnings);
earnings.bootstrap.mean = mean(earnings.bootstrap);

quantile(earnings.bootstrap, probs = c(0.05, 0.95));
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
shrimps.bootstrap.mean = adv_bootstrap(shrimp, mean);
shrimps.bootstrap.sd = adv_bootstrap(shrimp, sd);
shrimps.bootstrap.var = shrimps.bootstrap.sd ^ 2;
quantile(shrimps.bootstrap.mean, probs = c(0.025, 0.975));
quantile(shrimps.bootstrap.sd, probs = c(0.025, 0.975));
quantile(shrimps.bootstrap.var, probs = c(0.025, 0.975));
