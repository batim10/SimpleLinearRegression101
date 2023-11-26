#Function to obtain bootstrap estimates of linear regression model predicted y
#To use the boot function your statistics (this function) must have two arguments
#Data and indices(these are randomly generated at each bootstrap)

yhat <- function(data,idx){
  boot_x <- data[,1]

  boot_y <- data[,2]

  boot_model <- lm(boot_y[idx]~ boot_x[idx])

  boot_coef <- coef(boot_model)

  sum(boot_coef*c(1,as.numeric(userResponse)))

}


