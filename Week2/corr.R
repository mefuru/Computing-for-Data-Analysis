getmonitor <- function(id, directory, summarize = FALSE) {
	
    dFrame <- read.csv(paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", sep = ""))
    if (summarize) {
        print(summary(dFrame))
    }
    return(dFrame)
}

complete <- function(directory, id = 1:332) {
	notNACount <- vector()
	for(i in id) {
		dFrame <- getmonitor(i, directory, summarize = FALSE)
		dFrame <- dFrame[complete.cases(dFrame),]
		notNACount <- c(notNACount, nrow(dFrame))
	}
	data.frame(id = id, nobs = notNACount)
}

corr <- function(directory, threshold=0) {
	correlations <- vector() # Create empty vector
	nobsDF <- complete(directory) # Get dFrame with complete cases
	nobsDF <- nobsDF[nobsDF$nobs > threshold, ] # only include rows where nobs > threshold
	# iterate over ids where nobs > threshold
	for (i in nobsDF$id) {
		dFrame <- getmonitor(i, directory) #get frame for id
		dFrame <- dFrame[complete.cases(dFrame),]
		corVal <- cor(dFrame$sulfate, dFrame$nitrate, use = "complete.obs")
		correlations <- c(correlations, corVal)
	}
	return(correlations)
}