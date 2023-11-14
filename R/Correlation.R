#Calculates Pearson's Coefficient

Correlation <- function(x,y){
  dtaVect <- xyVectors(table)
  x <- dtaVect$x
  y <- dtaVect$y

  response <- readline(

    prompt <- "Do your variables appear to have a linear relationship? Answer Y or N:"
  )
  if (response == Y){
    response2 <- readline(
      prompt = "Are your variables normally distributed? Answer Y or N")
    if (response2 == Y){
      relationship <- cor.test(x,y,method = "pearson")
    }else if (response32 == N){
      relationship <- cor.test(x,y,method = "spearman")
    } else {
      stop("Invalid input")
    }
  } else if (response == N){
    output <- transformation(x,y)
    if (is.NA(output$x)){
      relationship <- cor.test(output$x, output$y)
    } else {
      stop("Your Data is Not Appropriate for Correlation Testing")
    }
  } print(relationship)
    print("The strength and direction of correlation are given by rho or cor.
            If your p-value is less than your significance level then this
            correlation is statistically significant.")
}
