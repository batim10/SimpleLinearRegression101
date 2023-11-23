#This function helps user decide if their data is appropriate for simple linear regression
#If appropriate will return location of data

intro = function(){
  cat("We will be modeling the relationship between two variables with simple
    linear regression as well as exploring the strength of correlation.

    When building a linear model, the explanatory or independent variable
    is the data that is believed to predict the value of the response or
    dependent variable. In research, the explanatory variable is changed and
    manipulated to cause a change in the response variable.

    Example: Increase calorie consumptions (explanatory variable) to then
    observe changes in weight (response variable)


    There are multiple types of data that can be collected when looking for
    correlation and when building predictive and explanatory models. Here are the
    most common types:\n

    Nominal data: Categories/named groups without inherent order. Examples: Types
    of cars, gender (male/female) , sample of different baseball teams, True/False
    This can be binary  (only two categories) or multinominal  (more than two
    categories)\n

    Ordinal data: Categories with inherent ranks that are not necessarily
    measurable or uniform. Examples: First, second, third.
    Mild, moderate and severe. Rich, middle class, poor.

    Discrete/ Count Categorical data: Counts of events (car accidents, college
    graduates) or things (cars, people, buildings, etc.). These are distinct
    categories without intermediate values (i.e Cannot have .33 cars)

    Continuous:  Numerical data that can take only any value within a given range.
    Examples: Height, weight,time \n"
          )
while(TRUE){
  response = readline(prompt = "Is your response variable continuous? Answer Y or N: ")

  if(tolower(response) %in% c("y","n")){
    if (tolower(response) == "n"){
    stop("Simple linear regression is not a suitable method to model your data.")


    }else{
    response2 = readline(prompt = "Is your explanatory variable binary or continuous? y")

      if (tolower(response2) == "n"){
      stop("Simple linear regression is not a suitable method to model your data.")
      }else if (tolower(response2) %in% c("y","n")){

        cat("Important formatting notes:
        * Your data should be formatted appropriately with your explanatory
          and response variables in a single dataset.
        * If your explanatory variable is binary your two categories
          should be expressed as 0 or 1 in your datase
        * Your data should be saved in one of the following formats for this program:
           csv, xlsx, txt \n")
        response3 = readline(prompt = "Is your data appropriately formatted? Answer Y or N: ")

        if (tolower(response3) == "n"){
        stop("Format your data appropriately then start over")
        }else if (tolower(response3) == "y"){
          cat("Your pathway format should be: Drivename:/path")
          response4 = readline(prompt ="Enter path to the location of your data file.")
          return(response4)
          break
        }else{
        cat("Invalid Response")
      }
    }else{
        cat("Invalid Response")
      }
    }
  }else{
      cat("Invalid Response")
    }
  }
}

