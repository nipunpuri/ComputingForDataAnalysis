count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
  
  homicides <- readLines("homicides.txt")
  causeMatch<-regexec("<dd>[Cc]ause: ([^ ]+)</dd>",homicides)
  causesVector<-regmatches(homicides,causeMatch)
  causesVector<-sapply(causesVector, function(x) x[2])
  numberOfCases<-0
  if(is.null(cause))
  {
    stop("Enter valid cause")
  }
  else if(cause=="asphyxiation")
  {
    numberOfCases<-(length(grep("[Aa]sphyxiation",causesVector)))
  }
  else if(cause=="blunt force")
  {
    numberOfCases<-(length(grep("[Bb]lunt [Ff]orce",causesVector)))
  }
  else if (cause=="other")
  {
    numberOfCases<-(length(grep("[Oo]ther",causesVector)))
  }
  else if(cause =="shooting")
  {
    numberOfCases<-(length(grep("[Ss]hooting",causesVector)))
  }
  else if(cause=="stabbing")
  {
    numberOfCases<-(length(grep("[Ss]tabbing",causesVector)))
  }
  else if (cause=="unknown")
  {
    numberOfCases<-(length(grep("[Uu]nknown",causesVector)))
  }
  else
  {
    stop()
  }
  numberOfCases
}