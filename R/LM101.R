
#' LM101
#'
#' This is an interactive function guiding user through creation of a model using simple linear regression or bootstrapping.
#' Confidence intervals for predictions are included as well.
#'
#' @return Linear model and confidence intervals

#'
#' @examples
#'  \dontrun{
#'  initiating function
#'
#'  SLR101()
#'  }
#'
#' @export

SLR101 <- function(){

  #Provide intro;provide path for user data
  step1 <- intro()

  #Load data based on extension
  step2 <- Load(step1)

  #Clearly define dependent and independent variables
  step3 <- xyVectors(step2)

  x <- step3$x
  y <- step3$y
  x_lab <- step3$x_lab
  y_lab <- step3$y_lab

  chk_numeric_x <- all(sapply(x, is.numeric))
  chk_numeric_y <- all(sapply(y, is.numeric))


  #check the loaded data is numeric
  if(chk_numeric_x & chk_numeric_y){

  #Graphs and testing to see if data has normal distribution or not
  step4 <- Visual(x,y,x_lab,y_lab)

  }else{
    stop("Your data must be numeric")
  }

  #Calculating  correlation coefficient for variables
  step5 <- Correlation(x,y,x_lab,y_lab)

  print(step5)

  #Use simple linear regression to create model
  step6 <- SimpleLinear(x,y)

  #Check that model follows assumptions/is valid
  step7 <- validity(x,y,x_lab,y_lab)

  #Calculate confidence intervals from linear model either using predict or bootstrap
  cat("
      We will use your model to find Confidence Intervals for mean respose
      given a predictor variable value at a level of .95 \n")

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
          response variable, we will also use bootstrapping to calculate your coefficients \n")

    #Number of bootstrap samples
    B <- 1000

    #Arrange vectors in dataframe
    boot_df <- data.frame(x_lab = x, y_lab= y)
    colnames(boot_df) <- c(x_lab,y_lab)


    #Predicted value of Y based on linear model; will create B bootstraps of this value
    #Apply function yhat to data in table 1000 times using boot library/function
    int_boot <- boot::boot(boot_df,yhat,B)

    #Get confidence intervals with boot.ci function from boot library
    CI_yhat <- boot::boot.ci(int_boot, type = "perc")

    boot_y <- CI_yhat$t0

    boot_int <- CI_yhat$percent

    #Use boot to obtain bootstrap estimates of model coefficients
    boot_coef <-  boot::boot(boot_df,boot_betas,B)


  cat("
        The Confidence Interval for your response variable given your predictor
        variable is:",boot_int[4,],boot_int[5,],"\n")

  cat("Your predicted response given predictor variable is:",boot_y,"\n")

  cat("Your estimated model coefficients are:",boot_coef[1],boot_coef[2],"\n")

 }else{
    stop("Your data as it is cannot be modeled using simple linear regression")
  }
}
