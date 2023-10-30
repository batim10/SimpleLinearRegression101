# This function will load csv, txt and excel files based on extension

Load = function(loc) {

  #Divide file location into prefix before period and extension
  split_loc = strsplit(loc, ".")[[1]]
  if (length(split_loc == 2)){
    suffix = split_loc[2]
  }else{
    stop("Invalid Input")
  }

  #use lower case of user input to read file extension
  if (tolower(suffix) == "csv"){
    response1 = readline(prompt = "Do you use . as a decimal point? Response Y or N")
    if (response1 == "Y"){
    dta = read.csv(loc)
    } else if (response1 == "N"){
      dta = read.csv2(loc)
    } else {
      stop("Invalid response")
    }

  } else if (tolower(suffix) == "txt"){
    response2 = readline(prompt = "Is your data in a table or tab delimited? Respond table or tab: ")
    if (tolower(response2) == "table"){
      dta = read.table(loc, header = TRUE)
    }else if (tolower(response2) == "tab"){
      if (response1 == "Y"){
        dta = read.delim(loc)
      } else if (response1 == "N"){
      dta = read.delim2(loc)
      } else {
      stop("Invalid response")
    }
    } else if(tolower(suffix) == "xlsx"){
      readx1::read.excel(loc)
    } else {
    stop ("Invalid File Type")
    }
  }
  return (dta)
 }
