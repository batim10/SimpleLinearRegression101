#Check model validity
#Transform data if needed
#problem with loop

validity <- function(x,y,x_lab,y_lab){

while(TRUE){

cat(" We will be assessing the validity of our model before performing
        more analysis. A valid model must adhere to the assumptions of
        the linear model.
        These assumptions are:
        * Residuals are random
        * Residuals are independent
        * Residuals are have a normal distribution
        * Residials demonstrate homoscedasticity/constant variance \n")

responses = rep(NA,4) #Initialize empty vector


cat("
        The residuals vs fitted plot should demonstrate a random scatter of
        of residuals on both sides of 0 without a signficant change or trend
        across fitted values indicating homoscedasticity and linearity \n")


responses[1] <- readline(
  prompt <- "Do your residuals appear to be randomly scattered? Answer Y or N: ")

cat("
        Your Normal Q-Q plot should show that your standardized residals fit
        the predicted line well indicating that they are normally distributed")

responses[2] <- readline(
  prompt<- "Do your residuals have a normal distribution? Answer Y or N

  Note: If your dataset is large you can answer Y even residials do not
  demonstrate a normal distribution.: "
)

cat("
        Your Scale-Location plot should demonstrate a fitted line that is
        flat. This demonstrates homoscedasticity \n ")

responses[3] <- readline(
  prompt = "Do your residuals demonstrate homoscedasticity? Answer Y or N: ")

cat("
        Your Residuals vs Leverage plot will indicate if you have outliers
        that have high leverage which can have a disproportionate
        affect on your model. These points will often have large Cook's distance \n")

responses[4] <- readline(
  prompt = "Do all data point have Cook's distance smaller than the limit (dotted line)?
  Answer Y or N: "
)

#check for appropriate user input
#Note: have to use tolower separately from %in% because in looks at entire vector
#while tolower is applied elementwise


#make a vector of all responses in lowercase
responses <- tolower(responses)

if (all(responses %in% c("y","n"))){

  if (all(responses == "y")){ #Valid model if all assumptions met
  cat("Your model is valid \n")
  return (list(x = x, y = y))
    break

  }else if (any(responses == "n")){
  cat("Your model fails to validate one or more assumptions of simple linear regression.\n")

   response1 <- readline(
    prompt = "Have you already transformed your data to normality? Respond Y or N: "
  )

    if (tolower(response1) == "n"){
      trans <- transformation(x,y,x_lab,y_lab)
      x <- trans$x
      y <- trans$y
      SimpleLinear(x,y)

    }else if (tolower(response1) == "y"){
      if (responses[4] == "n"){
      stop(
          "You have outliers with high leverage. Examine these data points to
          determine if they should remain in the model.
          * If they can be removed, resubmit your data without these points.
          * If they cannot be removed consider adding an indicator variable to
           your model and create a multivariable model \n")

      }else if (responses[4] == "y"){
        if (responses[2] == "n" & all(responses[-2] == "y")){
        cat("You can use bootstrapping to obtain a linear model \n")
        return("Boot")
          break
      }else{
          stop("Your data, as is, cannot be used to create a simple linear regression model \n")
        }
    }
    }else{
    cat("Invalid Response \n")
  }
 }
}else{
    cat("Invalid Response \n")
  }
}
}

