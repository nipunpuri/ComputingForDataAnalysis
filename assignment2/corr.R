corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## @author: Vivek Hariharan
  
  corrVector<-integer(length=0)
  for(index in 1:332)
  {
    formattedId <- formatC(index,width=3,flag="0")
    filePath <- paste(directory,"/",formattedId,".csv",sep="")
    data <- read.csv(filePath)
    validData <-data[complete.cases(data),]
       
    if(length(validData[,1])>threshold)
    {
        corrVector <-append(corrVector,cor(validData[,2],validData[,3]))
    }
  }  
  corrVector
}