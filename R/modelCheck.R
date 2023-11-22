#Check model validity

validity <- function(){
cat(" We will be assessing the validity of our model before performing
        more analysis. A valid model must adhere to the assumptions of
        the linear model.
        These assumptions are:
        * Residuals are random
        * Residuals are independent
        * Residuals are have a normal distribution
        * Residials demonstrate homoscedasticity/constant variance")

responses = rep(NA,4) #Initialize empty vector


cat("The residuals vs fitted plot should demonstrate a random scatter of
        of residuals on both sides of 0 without a signficant change or trend
        across fitted values indicating homoscedasticity and linearity")


responses[1] <- readline(
  prompt <- "Do your residuals appear to be randomly scattered? Answer Y or N")

cat("Your Normal Q-Q plot should show that your standardized residals fit
        the predicted line well indicating that they are normally distributed")

responses[2] <- readline(
  prompt<- "Do your residuals have a normal distribution? Answer Y or N

  Note: If your dataset is large you can answer Y even residials do not
  demonstrate a normal distribution."
)

cat("Your Scale-Location plot should demonstrate a fitted line that is
        flat. This demonstrates homoscedasticity ")

responses[3] <- readline(
  prompt = "Do your residuals demonstrate homoscedasticity? Answer Y or N")

cat("Your Residuals vs Leverage plot will indicate if you have outliers
        that have high leverage which can have a disproportionate
        affect on your model. These points will often have large Cook's distance")

responses[4] <- readline(
  prompt = "Do all data point have Cook's distance smaller than the limit (dotted line)?
  Answer Y or N"
)

#check for appropriate user input
if (any(responses != "Y") & any(responses != "N")){
  stop("Invalid Response")

  } else if (all(responses == "Y")){ #Valid model if all assumptions met
  cat("Your model is valid \n")
  return (TRUE)

  }else if (any(responses) == "N"){
  print("Your model fails to validate one or more assumptions of simple linear regression.")
  response1 <- readline(
    prompt = "Have you already transformed your data to normality? Respond Y or N"
  )

  if (response1 == "N"){
    print("Transform your data and submit transformed data to attempt to address this issue.")
    return(FALSE)

    }else if (response1 == "Y"){
  if (responses[4] == "N"){
    print("You have outliers with high leverage. Examine these data points to
          determine if they should remain in the model.
          * If they can be removed, resubmit your data without these points.
          * If they cannot be removed consider adding an indicator variable to
           your model and resubmit")
    return(FALSE)
  }else{
    if (responses[2] == "N" & all(responses[-2]) == "Y"){
      print("You can use bootstrapping to obtain a linear model")
      return("Boot")
    }
  }
}
}
}
