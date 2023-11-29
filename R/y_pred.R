
#' yhat
#'
#' This function is written to use with boot function to obtain bootstrap estimated linear regression model
#' This function must have two parameters for the boot function to generate random samples
#'
#' @param data a table containing predictor variable in first column and response variable in second column
#' @param idx random indices indicating which values to use in linear model
#'
#' @return Creates estimated response variable for simple linear model to be used in bootstrap function
#'
#' @examples
#' yhat(data,idx)
#'
#' @keywords internal
#'
yhat <- function(data,idx){
  boot_x <- data[,1]

  boot_y <- data[,2]

  boot_model <- lm(boot_y[idx]~ boot_x[idx])

  boot_coef <- coef(boot_model)

  sum(boot_coef*c(1,as.numeric(userResponse)))

}


