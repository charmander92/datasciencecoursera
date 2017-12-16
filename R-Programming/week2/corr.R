corr <- function(directory, threshold = 0) {
 
   ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  if(grep("specdata",directory) == 1) {
    directory <- "./specdata"
  }
  
  all_files <- list.files(directory)
  file_path <- paste(directory, all_files,sep="/")
  
  all_complete_data <- complete(directory,1:332)
  all_nobs <- all_complete_data$nobs
  nobs_ok <- nobs[nobs>threshold]
  
  #get ok ID:s
  ids_ok <- all_complete_data$id[all_nobs > threshold]
  
  
  id_length <- length(ids_ok)
  corr_vector <- rep(0,id_length)
  
  i <- 1
  
  for (k in ids_ok) {
    file <- read.csv(file_path[k], header=T)
    corr_vector[i] <- cor(file$sulfate,file$nitrate, 
                           use="complete.obs")
    i <- i + 1
  }
  res <- corr_vector
  return(res)
  
}

cr <- corr("specdata",150)
