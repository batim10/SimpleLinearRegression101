#Visualize data with scatter plots, histograms and density plots
#table is a dataframe from loading user data
#p is pch. Default is 19
#c is color of dots



Visual <- function(x, y, x_lab, y_lab, p = 19,c = "blue"){


  pdf("dtaGraphs.pdf") #Opens pdf

  par(mfrow = c(1,3)) #Output graphs with 3 graphs per page

  #Jitter spaces out data points with same value
  stripchart(x, method = "jitter", pch = p, col = c, main = paste("Dot plot",x_lab))
  hist(x, freq = FALSE, main = c("Histogram",x_lab))
  lines(density(x), col = "blueviolet")
  boxplot(x)

  stripchart(y, method = "jitter", pch = p, col = c, main = paste("Dot plot",y_lab))
  hist(y, freq = FALSE, main = c("Histogram",y_lab))
  lines(density(y), col = "blueviolet")
  boxplot(y)

  par(mfrow = c(1,1))

  plot(x, y, pch = p, color = c, xlab = x_lab, ylab = y_lab, main = "Scatterplot")
  lmout <- lm(y~x)
  abline(lmout)


  par(mfrow = c(1,3))

  normal <- rnorm(1000)
  gamma <- rgamma(1000, shape = 2, rate = 0.5)
  beta <- rbeta(1000, shape1 = 1, shape2 = 2)

  hist(normal, freq = FALSE, main = "Histogram Normal Data")
  lines(density(y), col = "green")

  hist(gamma, freq = FALSE, main = "Histogram Right Skewed Data")
  lines(density(y), col = "lightblue")

  hist(beta, freq = FALSE, main = "Histogram Left Skewed Data")
  lines(density(y), col = "hotpink")

  #create normal qq plots to assesss if normal distribution

  qqnorm(x, main = paste("Normal Q-Q Plot", x_lab))

  qqnorm(y, main = paste("Normal Q-Q Plot", y_lab))

  dev.off()

  cat("Shaprio test for normality. P value < 0.05 suggest that data is not
        normally distributed. Interpret in context of graphical data")

  shaprio.test(x)
  shapiro.test(y)

}
