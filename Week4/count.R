count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (class(cause)=="NULL") print("oh no!")
  ## Check that specific "cause" is allowed; else throw error
  if (cause != "asphyxiation" & cause != "blunt force" & cause != "other" & cause != "shooting" & cause != "stabbing" & cause != "unknown") {
    stop("Cause of death is invalid")
  }
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  ## Extract causes of death
  if (cause=="asphyxiation") {
    return(length(grep("Cause: [Aa]sphyxiation", homicides)))
  }
  if (cause=="blunt force") {
    return(length(grep("Cause: [Bb]lunt force", homicides)))
  }
  if (cause=="other") {
    return(length(grep("Cause: [Oo]ther", homicides)))
  }
  if (cause=="unknown") {
    return(length(grep("Cause: [Uu]nknown", homicides)))
  }
  if (cause=="shooting") {
    return(length(grep("Cause: [Ss]hooting", homicides)))
  }
  if (cause=="stabbing") {
    return(length(grep("Cause: [Ss]tabbing", homicides)))
  }
  ## Return integer containing count of homicides for that cause
}
