#Box cox transformation for data without linear relationship

transformation <- function(x,y){
  response <- readlline(
    prompt = "Are either of your datasets normally distributed?
      Respond X for explanatory variable.
      Respond Y for response variable.
      Respond N for neither.
      Respond B for both")

  if (response == "X"){
    bc_x <- MASS::boxcox(x~1)
    lambdax <- bc_x$x[which.max(bc_x$y)]
    x <- if (lambdax != 0)(x^lambdax - 1)/lambda else (log(x))

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
    stop("Your Data Is not Appropriate for a Linear Model")
  } else {
    stop("Invalid response")
  }

  plot(x, y, pch = p, color = c, xlab = x_lab, ylab = y_lab, main = "Scatterplot")
  lmout <- lm(y~x)
  abline(lmout)
  response2 <- readline(
    prompt = "Do your data have a linear relationship? Respond Y or N")

  if (response == "Y"){
    return (list(x,y))
  } else if (response == "N"){
    return(list(NA,NA))
  } else {
    stop("Invalid Response")
  }

}
