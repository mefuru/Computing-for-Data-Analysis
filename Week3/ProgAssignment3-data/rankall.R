rankall <- function(outcome, num = "best") {
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
  hospitals <- vector()
  states <- sort(unique(outcomeDF$State)) ## unique returns a vector of state abbreviations
  ## For each state, find the hospital of the given rank
  for (state in states) {
    outcomeDF1 <- outcomeDF[outcomeDF$State==state,] # filter by state
    ## order depending on outcome
    if(outcome=="heart attack") {
      outcomeDF1 <- outcomeDF1[order(outcomeDF1[,11], outcomeDF1[,2]),]
    } else if(outcome=="heart failure") {
      outcomeDF1 <- outcomeDF1[order(outcomeDF1[,17], outcomeDF1[,2]),]
    } else if(outcome=="pneumonia") {
      outcomeDF1 <- outcomeDF1[order(outcomeDF1[,23], outcomeDF1[,2]),]
    }
    ## concatenate vector with result depending on num
    if(num=="best") {
      hospitals <- c(hospitals,outcomeDF1[1,2])
    } else if(num=="worst") {
      outcomeDF1 <- outcomeDF1[order(-outcomeDF1[,11], outcomeDF1[,2]),] # reverse order
      hospitals <- c(hospitals,outcomeDF1[1,2])
    } else {
      hospitals <- c(hospitals,outcomeDF1[num,2])
    }
  }
  ## Return a data frame with the hospital names and the (abbreviated) state name
  result <- data.frame(hospital = hospitals, state = states)
  return(result)
}
