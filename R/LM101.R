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
        for an individual response variable given a predictor variable value")
  userResponse <- readline(prompt <- "At which value of your predictor variable would you like to calculate Confidence and Prediction Intervals?")

  if (step7){
    CI <- predict(step6, newdata=data.frame(x_lab = userResponse), interval = "confidence")
    PI <- predict(step6, newdata=data.frame(x_lab = userResponse), interval = "prediction")
  }else if (step7 = "Boot"){
    bootStrap(x,y)
    CI
    PI
  }else{
    stop("Your data cannot be modeled using simple linear regression at this time")
  }
}
