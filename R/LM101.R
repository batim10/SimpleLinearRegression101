#Provide user with information regarding linear modeling

#Source files containing necessary functions

#path for my dataset is "C:\Users\batim\OneDrive\Documents\STAT608\TextbookResources\Data\Data\my_airlines.txt"
#Name rnorm data, then y can be that times 2 plus 5

source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/Correlation.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/dataVectorsTitles.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/dataVisualization.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/Introduction.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/LM101.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/LoadData.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/modelCheck.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/outputLM.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/Transformation.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/y_pred.R")
source("C:/Users/batim/OneDrive/Documents/SimpleLinearRegression101/R/boot_coefs.R")

SLR101 = function(){

  step1 <- intro()

  step2 <- Load(step1)

  step3 <- xyVectors(step2)

  x <- step3$x
  y <- step3$y
  x_lab <- step3$x_lab
  y_lab <- step3$y_lab

  step4 <- Visual(x,y,x_lab,y_lab)

  step5 <- Correlation(x,y,x_lab,y_lab)

  step6 <- SimpleLinear(x,y)

  step7 <- validity(x,y,x_lab,y_lab)

  cat("
        If your model is valid, we will use your model to find Confidence Intervals for mean respose and Predictor Intervals
        for an individual response variable given a predictor variable value at a level of .95")

  userResponse <- readline(
  prompt <- "Value of your predictor variable to calculate Confidence Intervals: ")

  #Check if step 7 output can be used for prediction
  if (length(step7) > 1){

    x <- step7$x
    y <- step7$y

    model <- lm(y~x)


    #use linear model to create confidence interval at given prediction variable
    CI <- predict(model, newdata = data.frame(x = as.numeric(userResponse)), interval = "confidence", level = .95)

    cat("The Confidence Interval for your response variable at your given predictor variable value
        is",CI,"\n")
    }else if (step7 == "Boot"){

    cat("
          As you require bootstrapping to calculate Confidence intervals for your
          response variable, we will also use bootstrapping to calculate your coefficients")

    #Number of bootstrap samples
    B <- 1000

    boot_df <- data.frame(x_lab = x, y_lab= y)
    colnames(boot_df) <- c(x_lab,y_lab)

    #yhat function requires two arguments, often second argument are incides
    indices <- sample(1:length(x), replace = TRUE) #Sample to bootstrap

    #Predicted value of Y based on linear model; will create B bootstraps of this value

    #Apply function yhat to data in table 1000 times using boot library/function
    int_boot <- boot::boot(boot_df,yhat,B)

    #Get confidence intervals with boot.ci function from boot library
    CI_yhat <- boot::boot.ci(int_boot, type = "perc") #Get confidence intervals

    #Use mean betas as estimates for model
    int_boot_coef <-  boot::boot(boot_df,boot_betas,B)


  cat("The Confidence Interval for your response variable given your predictor variable is",CI_yhat)

  cat("Your estimated model coefficients are",int_boot_coef)
  }else{
    stop("Your data as it is cannot be modeled using simple linear regression")
  }
}
