
#plot realization function for GP
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_light_trans <- c("#DCBCBC80")
c_light_highlight_trans <- c("#C7999980")
c_mid_trans <- c("#B97C7C80")
c_mid_highlight_trans <- c("#A2505080")
c_dark_trans <- c("#8F272780")
c_dark_highlight_trans <- c("#7C000080")

c_light_teal <- c("#6B8E8E")
c_mid_teal <- c("#487575")
c_dark_teal <- c("#1D4F4F")

# Plot Gaussian process realizations
plot_gp_realizations <- function(fit, data, true, title) {
  params <- extract(fit)
  I <- length(params$f_predict[,1])
  
  #c_superfine <- c("#8F272705")
  plot(true_realization$x_total, true_realization$f_total, lwd=2, xlab="x", ylab="y", main="Posterior Realizations",
       xlim=c(-3, 3), ylim=c(-2, 2))
  for (i in 1:1)
    points(data$x_predict, params$f_predict[i,], col="green")
  
  #points(data$x_predict, data$y_predict, col="white", pch=16, cex=0.6)
  #points(data$x_predict, data$y_predict, col=c_mid_teal, pch=16, cex=0.4)
  #points(true$x_total, true$f_total, lwd=2, xlab="x", ylab="y")
  points(data$x, data$y, col="white", pch=16, cex=1.2)
  points(data$x, data$y, col="blue", pch=16, cex=0.8)
}

### .... Function to find the means of posterior realizations........##



#------------------------------------#

# plots of GP processes posteriors with gaussian noise and student-t noise assumptions

plot_posterior_predictive <- function(fit1, fit2, data, true_realization) {

  params1 <- extract(fit_model1)
  params2 <- extract(fit_model2)
  y1<- as.data.frame(params1$y_predict)
  y2<- as.data.frame(params2$y_predict)
  
  y11 <- colMeans(y1, na.rm = FALSE, dims = 1)
  y22 <- colMeans(y2, na.rm = FALSE, dims = 1)
  #c_superfine <- c("#8F272705")
  plot(true_realization$x_total, true_realization$f_total, lwd=2, xlab="x", ylab="y", main="Posterior Predictive means",
       xlim=c(-3, 3), ylim=c(-2, 2))
  for (i in 1:1){
    points(data$x_predict, as.array(y11), col="green", pch=6, cex=0.4)
    points(data$x_predict, as.array(y22), col="red", pch=6, cex=0.4)
  }
  
  #points(data$x_predict, data$y_predict, col="white", pch=16, cex=0.6)
  #points(data$x_predict, data$y_predict, col=c_mid_teal, pch=16, cex=0.4)
  #points(true$x_total, true$f_total, lwd=2, xlab="x", ylab="y")
  points(data$x, data$y, col="white", pch=16, cex=1.2)
  points(data$x, data$y, col="blue", pch=16, cex=0.8)
}


plot_posterior_realizations <- function(fit1, fit2, data, true, title) {
  params1 <- extract(fit1)
  params2 <- extract(fit2)
  f1<- as.data.frame(params1$f_predict)
  f2<- as.data.frame(params2$f_predict)
  
  f11 <- colMeans(f1, na.rm = FALSE, dims = 1)
  f22 <- colMeans(f2, na.rm = FALSE, dims = 1)
  #c_superfine <- c("#8F272705")
  plot(true_realization$x_total, true_realization$f_total, lwd=2, xlab="x", ylab="y", main=title,
       xlim=c(-3, 3), ylim=c(-2, 2))
  for (i in 1:1){
    points(data$x_predict, as.array(f11), col="green", pch=6, cex=0.4)
    points(data$x_predict, as.array(f22), col="red", pch=6, cex=0.4)
  }
  
  #points(data$x_predict, data$y_predict, col="white", pch=16, cex=0.6)
  #points(data$x_predict, data$y_predict, col=c_mid_teal, pch=16, cex=0.4)
  #points(true$x_total, true$f_total, lwd=2, xlab="x", ylab="y")
  points(data$x, data$y, col="white", pch=16, cex=1.2)
  points(data$x, data$y, col="blue", pch=16, cex=0.8)
}


#-------------- Posterior realizations of any two runs -------------------#

plot_posterior_realizations_single <- function(fit1, fit2, data, true, title) {
  params1 <- extract(fit1)
  params2 <- extract(fit2)
  #c_superfine <- c("#8F272705")
  plot(true_realization$x_total, true_realization$f_total, lwd=2, xlab="x", ylab="y", main=title,
       xlim=c(-3, 3), ylim=c(-2, 2))
  for (i in 1:1){
    points(data$x_predict, params1$f_predict[20, ], col="green", pch=6, cex=0.4)
    points(data$x_predict, params2$f_predict[20, ], col="red", pch=6, cex=0.4)
  }
  
  #points(data$x_predict, data$y_predict, col="white", pch=16, cex=0.6)
  #points(data$x_predict, data$y_predict, col=c_mid_teal, pch=16, cex=0.4)
  #points(true$x_total, true$f_total, lwd=2, xlab="x", ylab="y")
  points(data$x, data$y, col="white", pch=16, cex=1.2)
  points(data$x, data$y, col="blue", pch=16, cex=0.8)
}




# Plot Gaussian process predictive realizations
plot_gp_pred_realizations <- function(fit, data, true, title) {
  params <- extract(fit)
  I <- length(params$y_predict[,1])
  
  plot(1, type="n", xlab="x", ylab="y", main=title,
       xlim=c(-4, 4), ylim=c(-4, 4s))
  for (i in 1:3)
    points(data$x_predict, params$y_predict[i,], col=c_superfine)
  
  points(data$x_predict, data$y_predict, col="white", pch=16, cex=0.6)
  points(data$x_predict, data$y_predict, col=c_mid_teal, pch=16, cex=0.4)
  lines(true$x_total, true$f_total, lwd=2, xlab="x", ylab="y")
  points(data$x, data$y, col="white", pch=16, cex=1.2)
  points(data$x, data$y, col="black", pch=16, cex=0.8)
}


