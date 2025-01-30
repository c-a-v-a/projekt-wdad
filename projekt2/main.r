# DATA
library(MASS);

# REGRESSION MODEL
regression = lm(CW ~ CL, data = crabs);
cat("\nRegression model width~length of crabs body\n");
summary(regression);
cat("\n");

# PLOTS
png(file = "plots/scatterplot.png", height=1000, width=1000, res=150);
plot(x=crabs$CL, y=crabs$CW, xlab="Carapace length [mm]", ylab="Carapace width [mm]", main="Crab's carapace size");
abline(reg = regression, col = "red");
dev.off();

png(file = "plots/model_plots.png", height=1000, width=1000, res=150);
par(mfrow = c(2,2));
plot(regression);
dev.off();
