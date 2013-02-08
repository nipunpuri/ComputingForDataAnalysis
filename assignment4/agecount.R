agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
  homicides<-readLines("homicides.txt")
  
  match <- regexec("<dd>.* ([0-9]+) years old</dd>",homicides)
  matchedAge<-regmatches(homicides,match)
  ageRows<-sapply(matchedAge, function(x) x[2])
  
  ageCount<-0
  if(is.null(age))
  {
    stop("Enter age")
  }
  else
  {
    
    ageCount<-length(which(ageRows==age))
    
  }
  ageCount
}