#' xyVectors
#'
#' This function separates loaded table into predictor and response variable vectors
#'
#' @param table loaded user table
#'
#' @return list containing predictor and response variables and their column titles
#'
#' @examples
#' \dontrun{
#' xyVectors(table)
#'}
#'
#' @keywords internal
xyVectors <- function(table){
  response <- readline(prompt = "Is your response variable in column 1 or 2? Respond 1 or 2: ")
  if (response == 1){
    y <- table[,1]
    x <- table[,2]
    x_lab <- colnames(table)[2]#use header title for x labels
    y_lab <- colnames(table)[1]#use header title for y labels
  } else if (response == 2){
    y <- table[,2]
    x <- table[,1]
    x_lab <- colnames(table)[1]
    y_lab <- colnames(table)[2]
  } else {
    stop("Invalid response")
  }
  return(list(x=x,y=y, x_lab=x_lab, y_lab=y_lab))
}
