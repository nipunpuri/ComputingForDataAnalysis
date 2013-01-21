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
  
  ## @author: Vivek Hariharan
  
  countVector<-c()
  for(index in 1:(length(id)))
  {
    formattedId <- formatC(id[index],width=3,flag="0")
    fileName <- paste(directory,"/",formattedId,".csv",sep="")
    data <- read.csv(fileName)
    countOfObservations <- sum(complete.cases(data))
    
    countVector <- append(countVector,countOfObservations)
  }
  countDataFrame <-data.frame(id=id,nobs=countVector)
  countDataFrame
}