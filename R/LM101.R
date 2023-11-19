#Provide user with information regarding linear modeling

SLR101 = function(){
  step1 <- intro()

  step2 <- Load(step1)

  step3 <- xyVectors(step2)

  x <- step3$x
  y <- step3$y
  x_lab <- step3$x_lab
  y_lab <- step3$y_lab

  step4 <- Visual(x,y,x_lab,y_lab)

  step5 <- Correlation(x,y)

  step6 <- SimpleLinear(x,y,x_lab,y_lab)

  step7 <- modelCheck()

  print("If your model is valid, we will use your model to find Confidence Intervals for mean respose and Predictor Intervals
        for an individual response variable given a predictor variable valueat a level of .95")
  userResponse <- readline(prompt <- "At which value of your predictor variable would you like to calculate Confidence Intervals?")

  if (step7){
    CI <- predict(step6, newdata=data.frame(x_lab = userResponse), interval = "confidence", level = .95)
    cat("The Confidence Interval for your response variable at your given predictor variable value
        is",CI,"\n")
    }else if (step7 = "Boot"){

    print("As you require bootstrapping to calculate Confidence intervals for your
          response variable, we will also use bootstrapping to calculate your coefficients")

    B <- 1000

    yhat <- function(x,y){
      boot_indices <- sample(1:nrow(x), replace = TRUE)

      boot_model <- lm(y[boot_indices]~x[boot_indices])

      boot_coef <- coef(boot_model)

      s <- sum(boot_coef*c(1,userResponse))

      return(list (s = s,boot_coef = boot_coef))

    }

    #Apply function yhat to data in table 1000 times using boot library/function
    int_boot <- boot::boot(table,yhat,B)

    #Get confidence intervals with boot.ci function from boot librart
    CI_yhat <- boot::boot.ci(int_boot$s, type = "perc") #Get confidence intervals

    #Use mean betas as estimates for model
    mean_betas <- colMeans(int_boot$boot_coef)


  cat("The Confidence Interval for your response variable given your predictor variable is",CI_yhat)

  cat("Your estimated model is: Y =",mean_betas[1],"+",mean_betas[2],"X")
  }else{
    stop("Your data as it is cannot be modeled using simple linear regression")
  }
}
