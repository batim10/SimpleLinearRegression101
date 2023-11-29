#' boot_betas
#'
#' Function to use with boot function to obtain bootstrap estimates of linear regression model coefficients
#' To use boot function need data and indices
#'
#' @param data User table with predictor variable in 1st colum and response variable in 2nd
#' @param idx indices to use for generation of bootstrap samples
#'
#' @return estimated coefficients using simple linear regression
#'
#' @examples
#' boot_betas(data,idx)
#'
#' @keywords internal
boot_betas <- function(data,idx){

    boot_x <- data[,1]

    boot_y <- data[,2]

    boot_model <- lm(boot_y[idx]~ boot_x[idx])

    coef(boot_model)

  }
