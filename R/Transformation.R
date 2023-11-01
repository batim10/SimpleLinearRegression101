#Box cox transformation for data without linear relationship
# Perform box-cox transformation if variables do not have linear relationship
#Consider using car::powerTransformation. User needs to interpret and apply

transformation <- function(x,y){

  #Extract X and Y from user data
  dtaVect <- xyVectors(table)
  x <- dtaVect$x
  y <- dtaVect$y

  response <- readlline(
    prompt = "Are either of your datasets normally distributed?
      Respond X for explanatory variable.
      Respond Y for response variable.
      Respond N for neither.
      Respond B for both")

  if (response == "X"){
    bc_x <- MASS::boxcox(x~1) #Use MASS library to do box cox analysis
    lambdax <- bc_x$x[which.max(bc_x$y)] #use lambda that maximizes MLE
    x <- if (lambdax != 0)(x^lambdax - 1)/lambda else (log(x)) #perform transformation

  } else if (response == "Y"){
    bc_y <- MASS::boxcox(y~1)
    lambday <- bc_y$x[which.max(bc_y$y)]
    y <- if (lambday != 0)(x^lambday - 1)/lambda else (log(y))

  } else if (response == "N"){
    bc_x <- MASS::boxcox(x~1)
    lambdax <- bc_x$x[which.max(bc_x$y)]
    x <- if (lambdax != 0)(x^lambdax - 1)/lambda else (log(x))

    bc_y <- MASS::boxcox(y~1)
    lambday <- bc_y$x[which.max(bc_y$y)]
    y <- if (lambday != 0)(x^lambday - 1)/lambda else (log(y))

  } else if(response == "B"){
    stop("Your data must have linear relationship for this analysis")
  } else {
    stop("Invalid response")
  }

  #plot transformed variables to look for linear relationship
  print("Your Data Has Been Transformed Using Box Cox Transformation")
  plot(x, y, pch = p, color = c, xlab = x_lab, ylab = y_lab, main = "Scatterplot")
  lmout <- lm(y~x)
  abline(lmout)
  response2 <- readline(
    prompt = "Do your data have a linear relationship? Respond Y or N")

  #return list of transformed variables or NAs
  if (response == "Y"){
    return (list(x,y))
  } else if (response == "N"){
    return(list(NA,NA))
  } else {
    stop("Invalid Response")
  }

}
