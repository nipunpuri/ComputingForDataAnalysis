rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcomeData<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcomeData[,11]<-as.numeric(outcomeData[,11])
  outcomeData[,17]<-as.numeric(outcomeData[,17])
  outcomeData[,23]<-as.numeric(outcomeData[,23])
  stateData<-outcomeData[outcomeData$State==state,]
  
  if(nrow(stateData)==0)
  {
    stop("invalid state")
  }
  heartAttackData<-stateData[,11]
  heartFailureData<-stateData[,17]
  pneumoniaData<-stateData[,23]
  
  validData<-complete.cases(heartAttackData,heartFailureData,pneumoniaData)
  validStateData<-stateData[validData,]
  
  if(num=="best")
  {
    num=1
  }
  else if(num=="worst")
  {
    num=nrow(validStateData)
  }
  rankedHospital<-""
  if(outcome=="heart attack")
  {
    sortedData<-validStateData[order(validStateData[,11],validStateData[,2]),]
    rankedHospital<-sortedData[num,2]
  }
  else if(outcome=="heart failure")
  {
    sortedData<-validStateData[order(validStateData[,17],validStateData[,2]),]
    rankedHospital<-sortedData[num,2]
  }
  else if(outcome=="pneumonia")
  {
    sortedData<-validStateData[order(validStateData[,23],validStateData[,2]),]
    rankedHospital<-sortedData[num,2]
  }
  else
  {
    stop("invalid outcome")
  }
  rankedHospital  
}