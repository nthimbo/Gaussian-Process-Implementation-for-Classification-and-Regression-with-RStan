
library(rstan)
library(RGraphics)
library(rstantools)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Part 01 ------------- Simulation of Regression Gaussian Processes ------------------------------------#
source("stan_utility.R")
source("regression_data.R")


#Creating data for simulations_
writeLines(readLines("simu_gauss.stan"))
simu_fit <- stan(file='simu_gauss.stan', data=simu_data, iter=1,
                 chains=1, seed=494838, algorithm="Fixed_param")

#Sampling from and reserve some say 11
f_total <- simu_data$f                                                           
y_total <- extract(simu_fit)$y[1,]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     

true_realization <- data.frame(f_total, x_total)
names(true_realization) <- c("f_total", "x_total")

observed_idx <- c(5*(0:99)+1)

N = length(observed_idx)
N
x <- x_total[observed_idx]
y <- y_total[observed_idx]



plot(x_total, f_total,lwd=2, xlab="x", ylab="y",
     xlim=c(-4, 4), ylim=c(-2, 2), main = "Data for Regression : Train and Test")
#points(x_total, y_total, col="white", pch=16, cex=0.6)
#points(x_total, y_total, col=c_mid_teal, pch=13, cex=.8)
points(x, y, col="white", pch=16, cex=.2)
points(x, y, col="green", pch=16, cex=2)
legend("topleft", legend = c("Training Data", "Testing Data"), col = c("black", "green"),
       pch = c(5,5), lty = 1:2, pt.cex = 2, cex = 1.2, text.col = "black", horiz = F,
       inset = c(.1,.1,.1,.1))


#making predictions 

N_predict <- length(y_total)
x_predict <- x_total
y_predict <- y_total

#Per good Stan workflow we save these simulated data in its own file.
stan_rdump(c("N", "x", "y",
             "N_predict", "x_predict", "y_predict",
             "sample_idx"), file="gp.data.R")

data <- read_rdump("gp.data.R")

stan_rdump(c("f_total", "x_total", "sigma_true"), file="gp.truth.R")




#nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

#load data 
data <- read_rdump('gp.data.R')
true_realization <- read_rdump('gp.truth.R')

f_data <- list(sigma=true_realization$sigma_true,
               N=length(true_realization$f_total),
               f=true_realization$f_total)

writeLines(readLines("gp_prior_tune.stan"))
writeLines(readLines("Model_Gaussian_noise.stan"))
writeLines(readLines("Regression_model.stan"))
fit <- stan(file='gp_prior_tune.stan', iter=1, warmup=0, chains=1,
            seed=5838298, algorithm="Fixed_param")
fit_model1 <- stan(file='Regression_model.stan', data=data, seed=5838298)
fit_model2 <- stan(file='Model_Gaussian_noise.stan', data=data, seed=5838298)




#................plot of posterior realizations....................# 
plot_posterior_realizations(fit_model1, fit_model2, data, 
                            true_realization, "Posterior Realizations")
plot_posterior_realizations_single(fit_model1, fit_model2, data, 
                                   true_realization, "Posterior Realizations")

plot_posterior_predictive(fit_model1, fit_model2, data, true_realization)
plot_gp_realizations(fit_model1, data, true_realization,
                     "Posterior Realizations")




#part 02 ---- Simulation of Classification Gaussian Processes for Multi-label data-------------------#

#simulations with R-stan 
writeLines(readLines("Classification_model.stan"))
source("classification_data.R")
classification.model <- stan(file='Classification_model.stan', data=simu.data, iter=1,
                   chains=100, seed=1234, algorithm="Fixed_param")


params <- extract(classification.model)
p_values <- as.data.frame(params$rho1)
latent.values <- params$f
test.pred <- as.array(params$y_pred[1,])
train.pred <- as.array(params$y_pred_in[1, ])
train.data <- as.array(simu.data$y)
test.data <- as.array(simu.data$y_pred)




#--------------- Plots of the scales parameters for the betas --------------------#

plot(0,0, xlab = "iterations 1 to 100", ylab = "scales values", type = "l", xlim = c(1, 100), ylim = c(-.5, 10),
     main = "Progress of parameters")
lines(c(1:100), p_values[, 1], type = "l" ,col="black", pch = 5)
lines(c(1:100), p_values[, 2],  type = "l",col="green", pch = 5)
lines(c(1:100), p_values[, 3], type = "l", col="blue", pch = 5)
lines(c(1:100), p_values[, 4],  type = "l", col="pink", pch = 5)
legend("topright", legend = c("p1", "p2", "p3", "p4"), col = c("black", "green", "blue", "pink"),
       pch = c(5,5,5,5), lty = 1:4, pt.cex = 2, cex = 1.2, text.col = "black", horiz = F,
       inset = c(.1,.1,.1,.1))

#----------------Plots of latent values for one point of training data (x1, x2)-----#

plot(0,0, xlab = "iterations 0 to 100", ylab = "scales values", type = "l", xlim = c(0, 100), ylim = c(-100, 100),
     main = "Progress of latent values for single point")
lines(c(1:100), latent.values[, 1], type = "l" ,col="black", pch = 5)
lines(c(1:100), latent.values[, 2],  type = "l",col="green", pch = 5)
lines(c(1:100), latent.values[, 3], type = "l", col="blue", pch = 5)
legend("topleft", legend = c("Class1", "Class2", "Class3"), col = c("black", "green", "blue"),
       pch = c(5,5,5,5), lty = 1:3, pt.cex = 2, cex = 1.2, text.col = "black", horiz = F,
       inset = c(.1,.1,.1,.1))
#----------------                               ------------------------------------#



                                                                                                                                                                                                                                                                                                                                                                                        