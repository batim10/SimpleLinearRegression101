#This function helps user decide if their data is appropriate for simple linear regression
#If appropriate will return location of data

intro(){
  print("There are multiple types of data that can be collected when looking for
  correlation and when building predictive and explanatory models. Here are the
  most common types:
  Nominal data: Categories/named groups without inherent order. Examples: Types
  of cars, gender (male/female) , sample of different baseball teams, True/False
  This can be binary  (only two categories) or multinominal  (more than two
  categories)

  Ordinal data: Categories with inherent ranks that are not necessarily
  measurable or uniform. Examples: First, second, third.
  Mild, moderate and severe. Rich, middle class, poor.

  Discrete/ Count Categorical data: Counts of events (car accidents, college
  graduates) or things (cars, people, buildings, etc.). These are distinct
  categories without intermediate values (i.eCannot have .33 cars)

  Continuous:  Numerical data that can take only any value within a given range.
  Examples: Height, weight,time"
  )
  response = readline("Is your data continuous? Answer Y or N: ")

  if (response == "N"){
    stop("Simple linear regression is not the best method to analyze your data.")
  } else if (response == "Y") {
    print("When building a linear model, the explanatory or independent variable
    is the data that is believed to predict the value of the response or
    dependent variable. In research, the explanatory variable is changed and
    manipulated to cause a change in the response variable.

    Example: Increase calorie consumptions (explanatory variable) to then
    observe changes in weight (response variable)")
  } else {
  stop("Invalid Reponse")
  }

  response2 = readline("Does your data represent a single explanatory variable
                        and a single response variable in an appropriate format
                        (csv, text,xlxs)?
                       Answer Y or N: ")

  if (response2 == "N"){
    stop("Format data so that response variable data in one column of a
         table and the explanatory variable  data is in the other column ")
  } else if (response == "Y"){
    response3 = readline("Enter path to location of your data file")
    return(reponse3)
  } else {
    stop("Invalid Response")
  }

}
