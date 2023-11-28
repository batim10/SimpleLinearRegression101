#Perform simple linear regression, provide interpretions of output
#Including questions regarding model validity; may make this a separate function
#Need to fix the two comments/output

SimpleLinear <- function(x,y){

  fit <- lm(y~x) #Create linear model
  modelFit <-summary(fit) #Linear model info

  #Output the coefficients and the p value indicating if this is a significant
  #Relationship

  print(modelFit)

  coef_SE <- modelFit$coefficients[,"Std. Error"]

  cat("Your linear model is: y = ",modelFit$coefficients[1],"+",modelFit$coefficients[2],"x","\n")
  cat("The p-values for the intercept and slope are:",modelFit$coefficients[,"Pr(>|t|)"],"\n")
  cat("The standard errors of your intercept and coefficient are: ",
        coef_SE[1],coef_SE[2],"\n")
  cat("The residual standard error is", modelFit$sigma,"\n")
  cat("The R squared value is:",modelFit$r.squared,"\n")
  cat("F statistic:", modelFit$fstatistic[1],"\n")
  cat("F statistic p value:", modelFit$fstatistic[2],"\n\n")

  cat("Note: The F statistic is not relevant for simple linear regression \n\n")

  cat("
        If p-values of your coefficients are less than your level of significance
        this suggests that your model demonstrates a statistically significant
        relationship between your variables

        The R squared is what percentage of the variability in the dependent variable
        is explained by the model. The closer it is to 1 the stronger the model

        You can use your standard errors to calculate confidence intervals
        for your coefficients and model \n\n")

  par(mfrow = c(2,2))
  plot(fit) #Create residuals plots

  return(modelFit)
  }
