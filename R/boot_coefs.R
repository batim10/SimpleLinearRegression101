#Function to obtain bootstrap estimates of linear regression model coefficients
#To use the boot function your statistics (this function) must have two arguments
#Data and indices(these are randomly generated at each bootstrap)
#Requires dependent variable be in second column

boot_betas <- function(data,idx){

    boot_x <- data[,1]

    boot_y <- data[,2]

    boot_model <- lm(boot_y[idx]~ boot_x[idx])

    coef(boot_model)

  }
