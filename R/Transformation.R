#Box cox transformation for data without linear relationship or for issues with model validity
#Perform box-cox transformation if variables do not have linear relationship or if model assumptions not met
#Consider using car::powerTransformation. User needs to interpret and apply

transformation <- function(x,y,x_lab,y_lab){

  #Extract X and Y from user data; these should be extracted first then
  #input into function, fix later

  while(TRUE){
  response <- readline(
    prompt = "
      Are either of your datasets normally distributed?
      Respond X for explanatory variable.
      Respond Y for response variable.
      Respond N for neither.
      Respond B for both
    ")
  if (tolower(response) %in% c("x","y","n","b")){
    break
  }else{
    cat("Invalid response")
  }
  }
    if (tolower(response) == "x"){
      bc_y <- MASS::boxcox(y~1) #Use MASS library to do box cox analysis
      lambday <- bc_y$x[which.max(bc_y$y)] #use lambda that maximizes MLE
      y <- if (lambday != 0)(y^lambday - 1)/lambday else (log(y)) #perform transformation

      } else if (tolower(response) == "y"){
      bc_x <- MASS::boxcox(x~1)
      lambdax <- bc_x$x[which.max(bc_x$y)]
      x <- if (lambdax != 0)(x^lambdax - 1)/lambdax else (log(x))

    } else if (tolower(response) == "n"){
      bc_x <- MASS::boxcox(x~1)
      lambdax <- bc_x$x[which.max(bc_x$y)]
      x <- if (lambdax != 0)(x^lambdax - 1)/lambdax else (log(x))

      bc_y <- MASS::boxcox(y~1)
      lambday <- bc_y$x[which.max(bc_y$y)]
      y <- if (lambday != 0)(y^lambday - 1)/lambday else (log(y))

       }else if (tolower(response)== "b"){
      stop("Transformation will not improve your model")
    }


  #plot transformed variables to look for linear relationship
  print("Your Data Has Been Transformed Using Box Cox Transformation")
  plot(x, y, pch = 19, col = "blue", xlab = x_lab, ylab = y_lab, main = "Scatterplot")
  lmout <- lm(y~x)
  abline(lmout)

  while(TRUE){
  response2 <- readline(
    prompt = "Do your data have a linear relationship? Respond Y or N ")

  #return list of transformed variables or NAs
  if (tolower(response2) %in% c("y","n")){
    if (tolower(response2) == "y"){
      return (list(x=x,y=y))
    } else{
      return(list(x=NA,y=NA))
      break
    }
  }else{
      cat("Invalid Response")
    }
}
}
