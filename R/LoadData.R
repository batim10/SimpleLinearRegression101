# This function will load csv, txt and excel files based on extension
# loc is the location of users data file

Load = function(loc){
  #Divide file location into prefix before period and extension
  #Create list containing the user provided data pathway
  #Each element of the list is a character vector containing the date location
  #The vectors contain the characters separated from each other by a period
  #Had to use \\ to escape the period as it is a special regular expression thing
  split_loc = strsplit(loc, "\\.")[[1]]
  if (length(split_loc == 2)){
    suffix = split_loc[2]
  }else{
    stop("Invalid Input")
  }

  #Need to query about using a decimal point for appropriate formatting
  response1 = readline(prompt = "Do you use . as a decimal point? Response Y or N")

  if (tolower(response1) != "y" & tolower(response1) != "n"){
    stop("Invalid Response")
  }

  #use lower case of user input to read file extension
  if (tolower(suffix) == "csv"){
    if (tolower(response1) == "y"){
    dta = read.csv(loc)
    }else if (tolower(response1) == "n"){
      dta = read.csv2(loc)}
  }else if (tolower(suffix) == "txt"){
    response2 = readline(prompt = "Is your data in a table or tab delimited? Respond table or tab: ")
    if (tolower(response2) == "table"){
      dta = read.table(loc, header = TRUE)
  }else if (tolower(response2) == "tab"){
      if (tolower(response1) == "y"){
        dta = read.delim(loc)
      }else if (tolower(response1) == "n"){
      dta = read.delim2(loc)}
      }else if(tolower(suffix) == "xlsx"){
      readx1::read.excel(loc)
  }else{
    stop ("Invalid File Type")
  }
  }
  dta = na.omit(dta) #remove rows with missing values
  return (dta)
  }


