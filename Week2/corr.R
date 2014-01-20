getmonitor <- function(id, directory, summarize = FALSE) {
	# paste function concatenates args
	# as.numeric(x) casts to a numeric
	# sprintf returns a character vector containing a formatted combination of text and variable values
    dFrame <- read.csv(paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", sep = ""))
	# summary is a generic function used to produce result summaries of the results of various model fitting functions
    if (summarize) {
        print(summary(dFrame))
    }
    return(dFrame)
}

complete <- function(directory, id = 1:332) {
	notNACount <- vector() # initialise empty vector
	for(i in id) {
		dFrame <- getmonitor(i, directory, summarize = FALSE)
		dFrame <- dFrame[complete.cases(dFrame),]
		# complete.cases Return a logical vector indicating which cases are complete, i.e., have no missing values.
		notNACount <- c(notNACount, nrow(dFrame)) # concatenate vector with no of rows in complete data frame
	}
	# create a new data frame with column headers id and nobs with vals from vectors passed
	data.frame(id = id, nobs = notNACount)
}

corr <- function(directory, threshold=0) {
	correlations <- vector() # Create empty vector
	nobsDF <- complete(directory) # Get complete dFrame
	nobsDF <- nobsDF[nobsDF$nobs > threshold, ] # only include rows where nobs > threshold
	# iterate over ids where nobs > threshold
	for (i in nobsDF$id) {
		dFrame <- getmonitor(i, directory) #get frame for id
		dFrame <- dFrame[complete.cases(dFrame),] #remove incomplete cases
		corVal <- cor(dFrame$sulfate, dFrame$nitrate, use ="complete.obs")
		correlations <- c(correlations, corVal)
	}
	return(correlations)
}