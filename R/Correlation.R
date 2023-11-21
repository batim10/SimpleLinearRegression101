#Calculates Pearson's Coefficient

Correlation <- function(x,y){

  while(TRUE){
  response <- readline(
  prompt <- "Do your variables appear to have a linear relationship? Answer Y or N:"
  )
  if (tolower(response) %in% c("y","n")){

    if (tolower(response) == "y"){
      response2 <- readline(
        prompt = "Are your variables normally distributed? Answer Y or N")

       if (tolower(response2) %in% c("y","n")){
        if (tolower(response2) == "y"){
          relationship <- cor.test(x,y,method = "pearson")
        }else{
        relationship <- cor.test(x,y,method = "spearman")
        break
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
            relationship <- cor.test(output$x, output$y)
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
