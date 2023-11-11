#Perform simple linear regression, provide interpretions of output
#Including questions regarding model validity; may make this a separate function

SimpleLinear <- function(x,y){
  dtaVect <- xyVectors(table)
  x <- dtaVect$x
  y <- dtaVect$y

  plot(x, y, )

  n <- length(x)
  x <- cbind(rep(1,n),x)

  fit <- lm(y~x)
  summary(fit)

  #Output the coefficients and the p value indicating if this is a significant
  #Relationship

  par(mfrow = c(2,2))
  plot(fit)

  print(" We will be assessing the validity of our model before performing
        more analysis. A valid model must adhere to the assumptions of
        the linear model.
        These assumptions are:
        * Residuals are random
        * Residuals are independent
        * Residuals are have a normal distribution
        * Residials demonstrate homoscedasticity")

  responses = rep(0,NA)

  print("The residuals vs fitted plot should demonstrate a random scatter of
        of residuals on both sides of 0 without a signficant change or trend
        across fitted values indicating homoscedasticity and linearity")

  responses[1] <- readline(
    prompt <- "Do your residuals appear to be randomly scattered? Answer Y or N")

  print("Your Normal Q-Q plol should show that your standardized residals fit
        the predicted line well indicating that they are normally distributed")

  responses[2] <- readline(
    prompt<- "Do your residuals have a normal distribution? Answer Y or N"
  )

  print("Your Scale-Location plot should demonstrate a fitted line that is
        flat. This demonstrates homoscedasticity ")

  responses[3] <- readline(
    prompt = "Do your residuals demonstrate homoscedasticity? Answer Y or N")

  print("Your Residuals vs Leverage plot will indicate if you have outliers
        that have high leverage and residuals whcih can have a disproportionate
        affect on your model. These points will often have large Cook's distance")

  responses[4] <- readline(
    prompt = "Do you have points with high Cook's distance?"
  )

  if (all(responses == "T"){
    print"Your model is valid"
  }

  if (residualAnalysis()){

  }else{

  }

  }

  #Assessing validity of model: Errors with constant variance? Outliers/leverage points?/large sample size

}
