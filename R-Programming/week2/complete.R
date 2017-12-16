

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  if(grep("specdata",directory) == 1) {
    directory <- "./specdata"
  }
  
  
  all_files <- list.files(directory)
  file_path <- paste(directory, all_files,sep="/")
  
  id_length <- length(id)
  complete_cases <- rep(0,id_length)
  
  i <- 1
  
  for (k in id) {
    file <- read.csv(file_path[k], header=T)
    complete_cases[i] <- sum(complete.cases(file))
    i <- i + 1
  }
  
  res <- data.frame(id=id,nobs=complete_cases)
  return(res)
  
}

complete("specdata",1)
complete("specdata",c(2,4,8,10,12))
complete("specdata",30:25)
complete("specdata",3)
