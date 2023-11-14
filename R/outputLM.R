#Perform simple linear regression, provide interpretions of output
#Including questions regarding model validity; may make this a separate function

SimpleLinear <- function(x,y){
  dtaVect <- xyVectors(table)
  x <- dtaVect$x
  y <- dtaVect$y

  plot(x, y, xlab = dtaVect$x_lab, ylab = dtaVect$y_lab)

  n <- length(x)
  x <- cbind(rep(1,n),x) #Add column of 1s to get intercept

  fit <- lm(y~x) #Create linear model
  summary(fit) #Plot residuals

  #Output the coefficients and the p value indicating if this is a significant
  #Relationship

  par(mfrow = c(2,2))
  plot(fit)
  }
