#Perform simple linear regression, provide interpretions of output
#Including questions regarding model validity; may make this a separate function

SimpleLinear <- function(x,y, x_lab,y_lab){

  plot(x, y, xlab = x_lab, ylab = y_lab)

  n <- length(x)
  x <- cbind(rep(1,n),x) #Add column of 1s to get intercept

  fit <- lm(y~x) #Create linear model
  modelFit <-summary(fit) #Plot residuals

  #Output the coefficients and the p value indicating if this is a significant
  #Relationship

  print(modelFit)

  cat("Your linear model is: y = ",model$coefFitficients[1],"+",model$coefficients[2],"x","\n")
  cat("The p-values for the intercept and slope are:",modelFit$coefficients[,"Pr(>|t|)"],"\n")
  cat("The standard errors of your coefficients are: ",modelFit$coefficients[,"Std.Error"],"\n")
  cat("The residual standard error is", modelFit$sigma,"\n")
  cat("The R squared value is:",modelFit$r.squared,"\n")
  cat("F statistic:", modelFit$fstatistic[1],"\n")
  cat("F statistic p value:", modelFit$Fstatistic,"\n")

  print("If your p-values are less than your level of significance this suggests
        that your model demonstrates a statistically significant relationship
        between your variables

        The R squared is what percentage of the variability in the dependent variable
        is explained by the model. The closer it is to 1 the stronger the model

        You can use your standard errors to calculate confidence intervals
        for your coefficients and model")

  par(mfrow = c(2,2))
  plot(fit)

  return(modelFit)
  }
