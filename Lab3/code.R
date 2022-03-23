#define population mean and standard deviation
x = c(653, 646, 654, 153, 305, 1200, 1193, 172)
sample_mean <- mean(x)

population_mean <- 860
population_sd <- 270

sampling_distributions_sd <- population_sd/sqrt(length(x))
sampling_distributions_mean <- population_mean
#Create a sequence of 1000 x values based on population mean and standard deviation
sampling_distributions <- seq(-4, 4, length = 1000) * sampling_distributions_sd + sampling_distributions_mean
#for each value in x
y <- dnorm(sampling_distributions, sampling_distributions_mean, sampling_distributions_sd)
#plot normal distribution with customized x-axis labels
plot(sampling_distributions,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
sd_axis_bounds = 5 # we will plot until 5 stdev
axis_bounds <- seq(-sd_axis_bounds * sampling_distributions_sd + sampling_distributions_mean,
                   sd_axis_bounds * sampling_distributions_sd + sampling_distributions_mean,
                   by = sampling_distributions_sd)

axis(side = 1, at = axis_bounds, pos = 0)
#lets calculate 3sd if x bar falls above or bellow 3stdev we know it has very low probability
upper_b <- sampling_distributions_mean +1 * sampling_distributions_sd
lower_b <- sampling_distributions_mean - 1 * sampling_distributions_sd

abline(v=upper_b,col="orange")
polygon(c(sampling_distributions[sampling_distributions>=upper_b], max(sampling_distributions), upper_b), c(y[sampling_distributions>=upper_b], 0, 0), col="red")
text(upper_b+10, 0.001, "P(X>)=0.01")

abline(v=lower_b,col="orange")
polygon(c(min(sampling_distributions),sampling_distributions[sampling_distributions<=lower_b],lower_b ), c(0,y[sampling_distributions<=lower_b], 0), col="red")
text(lower_b-10, 0.001, "P(X<)=0.01")


abline(v=sample_mean,col="green")
text(sample_mean+10, 0.004, "sample mean")


