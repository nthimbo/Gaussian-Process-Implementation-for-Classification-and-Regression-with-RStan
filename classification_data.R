library(rdist)

#------ code to generate the overall data to be used ---------------#
#prior values 
set.seed(1234)
alpha_true <- rnorm(1, 0, 1)
rho_true <- rnorm(1, 0, 1)
uB = rep(0, 4 )     # mean vector in the Gaussian prior of beta
VB = diag(4)*400
beta = as.vector(c(1:4))
beta
sigma_true <- rnorm(1, 0, 1)

mu = c()
N_samples = 1000
N_test = 10 
mat_x = matrix(NA, nrow = N_samples, ncol = 5)


populate_mat <- function(mat){
  
  for(row in 1:NROW(mat)){
    for(col in 1:4){
      mat[row, col] <- runif(1, 0, 1)
    }
    XX1 <- mat[row, 1]
    XX2 <- mat[row, 2]
    x1 <- c(XX1, XX2)
    x2 <- c(0.4, 0.5)
    distance1 <- rdist(rbind(x1, x2), metric = "euclidean")
    distance2 <- 0.8*XX1 + 1.8*XX2
    if(distance1 < 0.35){mat[row, 5] <- 0}
    else if(distance2 < 0.6){mat[row, 5] <- 1}
    else{mat[row, 5] <- 2}
    
  }
  colnames(mat) <- c("x1", "x2", "x3","x4", "y") 
  return(mat)
}

mat_x <- as.data.frame(populate_mat(mat_x))
vec.f <- as.numeric(mat_x[(N_test+1):N_samples, 5])
vec.x <- mat_x[(N_test+1):N_samples , 1:4]
x_pred <- mat_x[1:N_test, 1:4]
y_pred <- as.numeric(mat_x[1:N_test, 5])
npred <- length(y_pred)
N = length(vec.f)

simu.data <- list(sigma=sigma_true,rho = rho_true, alpha = alpha_true,
                  mu = mu, N=N, D = 4, N_pred = npred, x_pred =x_pred,
                  x=vec.x, y_pred = y_pred, y=vec.f, VB = VB, uB = uB, beta = beta, x1 = vec.x)

#-----------plots of training data--------------------# 

plot(vec.x[,1], vec.x[,2], type="n", xlab="X1", ylab="X2", xlim = c(-0.2, 1.2), ylim=c(-0.2, 1.2), main = "Classification Actual Data") 
for(i in 1:nrow(vec.x)){
  if(vec.f[i] == 0){points(vec.x[i, 1], vec.x[i, 2], type="p", col="blue", pch=17)}
  else if (vec.f[i] == 1){points(vec.x[i, 1], vec.x[i, 2], type="p", col="red", pch = 20)}
  else{points(vec.x[i, 1], vec.x[i, 2], type="p", col="green", pch=24)}
}
 



