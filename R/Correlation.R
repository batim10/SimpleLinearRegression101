#' Correlation
#'
#' This function checks if data is appropriate for calculation of a correlation coefficient
#' If appropriate, Pearson's (if linear and normal distribution) or Spearman's (linear relationship, not normal distribution)
#'
#' @param x vector of predictor variable data
#' @param y vector of response variable data
#' @param x_lab predictor variable column title
#' @param y_lab response variable column title
#'
#' @return Pearson or Spearman's correlation constant with p values
#'
#' @examples
#' Correlation(x,y,Wages,Travel)
#'
#' @keywords internal
Correlation <- function(x,y, x_lab,y_lab){

  #Use while loop to reprompt user if they use invalid input
  while(TRUE){

  response <- readline(
  prompt <- "Do your variables appear to have a linear relationship? Answer Y or N: "
  )

  if (tolower(response) %in% c("y","n")){
    break
  } else {
    cat("Invalid input")
  }
  }
  #checking for valid input
  if (tolower(response) == "y"){
    while(TRUE){

      response2 <- readline(
        prompt = "Are your variables normally distributed? Answer Y or N: ")

      #checking for valid input to obtain correlation coefficients
       if (tolower(response2) %in% c("y","n")){
         break
       }else {
         cat("Invalid Input")}
    }
      if (tolower(response2) == "y"){
        relationship <- cor.test(x,y,method = "pearson")
      }else{
      relationship <- cor.test(x,y,method = "spearman")
        }

    }else{

      while(TRUE){
      response3 <- readline(
        prompt = "Are your variables normally distributed? Answer Y or N: ")

      if (tolower(response3) %in% c("y","n")){
        break
      }else{
        cat("Invalid input")
      }
      }

        if (tolower(response3) == "y"){
          stop("A correlation coefficient cannot be calculated for your data")
        }else{
          cat("
          *A box cox transformation will be performed on your data
          *If your resulting data has a linear relationship a pearson
          correlation coefficient will be calculated
              ")
          output <- transformation(x,y,x_lab,y_lab)
          if (length(output$x) > 1){
            relationship <- cor.test(output$x, output$y, method = "pearson")
          }else{
            stop("Your Data is Not Appropriate for Correlation Testing")
          }
        }


}

    cat("
           * The strength and direction of correlation are given by rho or cor.

           * The closer rho or cor are to 1 the stronger the correlation

           *The sign of rho or cor indicate the direction of the correlation

           * If your p-value is less than your significance level then this
            correlation is statistically significant.\n")

    print(relationship)
}
