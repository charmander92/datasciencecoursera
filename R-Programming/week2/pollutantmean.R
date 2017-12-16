setwd("C:/Users/johan.edblom/Desktop/Learning/R")

##data <- read.csv("./specdata/001.csv")


pollutantmean <- function (directory, pollutant='sulfate',id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  if(grep("specdata",directory) == 1) {
    directory <- "./specdata"
  }
  
  pollutant_means <- c()
  all_files <- list.files(directory)
  file_path <- paste(directory, all_files,sep="/")
  
  for (k in id) {
    file <- read.csv(file_path[k], header=T)
    file_noNAs <- file[!is.na(file[, pollutant]),pollutant]
    pollutant_means <- c(pollutant_means,file_noNAs)
  }
  
  res <- mean(pollutant_means)
  return(round(res,3))
  
}

pollutantmean("specdata", "sulfate",1:20)
