rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  outcomeData<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcomeData[,11]<-as.numeric(outcomeData[,11])
  outcomeData[,17]<-as.numeric(outcomeData[,17])
  outcomeData[,23]<-as.numeric(outcomeData[,23])
  
  hospitalNames<-outcomeData[,2]
  heartAttackData<-outcomeData[,11]
  heartFailureData<-outcomeData[,17]
  pneumoniaData<-outcomeData[,23]
    
  if(num=="best")
  {
    rank=1
  }
    
  hospitals<-vector(length=0)
  if(outcome=="heart attack")
  {
    validData<-complete.cases(heartAttackData,hospitalNames)
    validOutcomeData<-outcomeData[validData,]
    states <- names(table(validOutcomeData$State))
    
    for(i in 1:length(states))
    {
      currentState<-states[i]
      currentStateData<-validOutcomeData[validOutcomeData$State==currentState,]
      rankedData<-currentStateData[order(currentStateData[,11],currentStateData[,2]),]
      if(num=="worst")
      {
        rank=nrow(currentStateData)
      }
      hospitals<-append(hospitals,rankedData[rank,2])
    }
    
  }
  else if(outcome=="heart failure")
  {
    validData<-complete.cases(heartFailureData,hospitalNames)
    validOutcomeData<-outcomeData[validData,]
    states <- names(table(validOutcomeData$State))
    
    for(i in 1:length(states))
    {
      currentState<-states[i]
      currentStateData<-validOutcomeData[validOutcomeData$State==currentState,]      
      rankedData<-currentStateData[order(currentStateData[,17],currentStateData[,2]),]      
      if(num=="worst")
      {
        rank=nrow(currentStateData)
      }
      hospitals<-append(hospitals,rankedData[rank,2])
      
    }
  }
  else if(outcome=="pneumonia")
  {
    validData<-complete.cases(pneumoniaData,hospitalNames)
    validOutcomeData<-outcomeData[validData,]
    states <- names(table(validOutcomeData$State))
    
    for(i in 1:length(states))
    {
      currentState<-states[i]
      currentStateData<-validOutcomeData[validOutcomeData$State==currentState,]      
      rankedData<-currentStateData[order(currentStateData[,23],currentStateData[,2]),]      
      if(num=="worst")
      {
        rank=nrow(currentStateData)
      }
      hospitals<-append(hospitals,rankedData[rank,2])
    }
  }
  else
  {
    stop("invalid outcome")
  }
  rankedHospitals<-data.frame(hospital=hospitals,state=states)
  rankedHospitals
}