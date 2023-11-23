#Visualize data with scatter plots, histograms and density plots
#table is a dataframe from loading user data
#p is pch. Default is 19
#c is color of dots



Visual <- function(x, y, x_lab, y_lab, p = 19,c = "blue"){

  cat("
      *A pdf called dtaGraphs will be created in your current directory

      *It will display graphical representations of your data including
      scatter plots, histograms and boxplots to determine data distribution

      *Shapiro Wilks test will also be performed to test for Normal Distribution \n")


  pdf("dtaGraphs.pdf") #Opens pdf

  par(mfrow = c(1,3)) #Output graphs with 3 graphs per page

  #Jitter spaces out data points with same value
  stripchart(x, method = "jitter", pch = p, col = c, main = paste("Dot plot",x_lab))
  hist(x, freq = FALSE, main = c("Histogram",x_lab))
  lines(density(x), col = "blueviolet")
  boxplot(x, main = c("Boxplot",x_lab))

  stripchart(y, method = "jitter", pch = p, col = c, main = paste("Dot plot",y_lab))
  hist(y, freq = FALSE, main = c("Histogram",y_lab))
  lines(density(y), col = "blueviolet")
  boxplot(y, main = c("Boxplot",y_lab))

  par(mfrow = c(1,1))

  plot(x, y, pch = p, col = c, xlab = x_lab, ylab = y_lab, main = paste("Scatterplot of", y_lab,"vs",x_lab))
  lmout <- lm(y~x)
  abline(lmout) #add line representing lm


  par(mfrow = c(1,3))

  normal <- rnorm(1000)
  gamma <- rgamma(1000, shape = 2, rate = 0.5)
  beta <- rbeta(1000, shape1 = 3, shape2 = 2)

  hist(normal, freq = FALSE, main = "Histogram Normal Data")
  lines(density(normal), col = "blueviolet")

  hist(gamma, freq = FALSE, main = "Histogram Right Skewed Data")
  lines(density(gamma), col = "blueviolet")

  hist(beta, freq = FALSE, main = "Histogram Left Skewed Data")
  lines(density(beta), col = "blueviolet")

  #create normal qq plots with fitline to assesss if normal distribution
  par(mfrow = c(1,2))
  qqnorm(x, main = paste("Normal Q-Q Plot", x_lab))
  qqline(x)

  qqnorm(y, main = paste("Normal Q-Q Plot", y_lab))
  qqline(y)

  dev.off()#close pdf

  cat("
        We will use the Shaprio test to test for normality.
        A P value < 0.05 suggests that data is not
        normally distributed.
        Interpret in context of graphical data \n\n")

  shapX <- shapiro.test(x)
  shapY <- shapiro.test(y)

  cat("Your Shapiro Wilks p-value for",x_lab,"is: ",shapX$p.value,"\n")
  cat("Your Shapiro Wilks p-value for",y_lab,"is: ",shapY$p.value, "\n")

}
