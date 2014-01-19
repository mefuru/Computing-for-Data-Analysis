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

