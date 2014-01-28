rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  ## Coerce death rates to numerics from character
  suppressWarnings(outcomeDF[,11] <- as.numeric(outcomeDF[,11]))
  suppressWarnings(outcomeDF[,17] <- as.numeric(outcomeDF[,17]))
  suppressWarnings(outcomeDF[,23] <- as.numeric(outcomeDF[,23]))
    ## Check that state and outcome are valid
  if(nrow(outcomeDF[outcomeDF$State==state, ])==0) {
    stop("invalid state")
  }
  ## “heart attack”, “heart failure”, or “pneumonia” are acceptable outcomes
  if(outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia") {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  # Filter DF to state
  outcomeDF <- outcomeDF[outcomeDF$State==state,]
  
  if(outcome=="heart attack") {
    outcomeDF <- outcomeDF[order(outcomeDF[,11], outcomeDF[,2]),]
  }
  else if(outcome=="heart failure") {
    outcomeDF <- outcomeDF[order(outcomeDF[,17], outcomeDF[,2]),]
  }
  else if(outcome=="pneumonia") {
    outcomeDF <- outcomeDF[order(outcomeDF[,23], outcomeDF[,2]),]
  }
  if(num=="best") {return(outcomeDF[1,2])}
  else if(num=="worst") {
    outcomeDF <- outcomeDF[order(-outcomeDF[,11], outcomeDF[,2]),]
    print(outcomeDF[1,2])
    return(outcomeDF[1,2])
  } else {
  return(outcomeDF[num,2])
  }
}