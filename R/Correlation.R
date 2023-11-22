#Checks if correlation can be calculate and in what form
#Pearson's if linear and normal
#Spearman's if linear and not normal

Correlation <- function(x,y){

  #Use while loop to reprompt user if they use invalid input
  while(TRUE){

    response <- readline(
  prompt <- "Do your variables appear to have a linear relationship? Answer Y or N:"
  )
  #checking for valid input
  if (tolower(response) %in% c("y","n")){

    if (tolower(response) == "y"){
      response2 <- readline(
        prompt = "Are your variables normally distributed? Answer Y or N")

      #checking for valid input to obtain correlation coefficients
       if (tolower(response2) %in% c("y","n")){
        if (tolower(response2) == "y"){
          relationship <- cor.test(x,y,method = "pearson")
        }else{
        relationship <- cor.test(x,y,method = "spearman")
        break #Break from loop if valid input
        }
      }else{
        cat("Invalid input")
        }
    }else{
      response3 <- readline(
        prompt = "Are your variables normally distributed? Answer Y or N")

      if (tolower(response3) %in% c("y","n")){
        if (tolower(response3) == "y"){
          stop("A correlation coefficient cannot be calculated for your data")
        }else{
          output <- transformation(x,y)
          if (is.NA(output$x) == FALSE){
            relationship <- cor.test(output$x, output$y, method = "pearson")
            break
          }else{
            stop("Your Data is Not Appropriate for Correlation Testing")
          }
        }
     }else{
        cat("Invalid Input")
     }
    }

    }else{
    cat("Invalid Input")
  }

}
    print(relationship)
    print("The strength and direction of correlation are given by rho or cor.
            If your p-value is less than your significance level then this
            correlation is statistically significant.")
}
