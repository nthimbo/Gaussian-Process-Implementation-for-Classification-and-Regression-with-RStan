
alpha_true <- 3
rho_true <- 5.5
sigma_true <- 0.1
mu = c()


set.seed(34526)

# data 
N <- 600
x_total <- rnorm(N, 0, 1)
x_total

y_total <- rep(0, N)
length(y_total)
prob_samples = c(rep(0.1, N*0.95), rep(1, N*0.05))

for (i in 1:length(x_total)){
  pos <- x_total[i]
  sigma <- sample(prob_samples, size = 1, replace = FALSE)
  mui <- 0.3+0.4*pos+0.5*sin(2.7*pos)
  mu[i] <- mui
  y_total[i] <- rnorm(1, mui, sigma)
}

plot(x_total, y_total, main = "Regression Actual Data")
simu_data <- list(alpha=alpha_true, rho=rho_true, sigma=sigma_true,
                  mu = mu, N=N, x=x_total, f=y_total)







