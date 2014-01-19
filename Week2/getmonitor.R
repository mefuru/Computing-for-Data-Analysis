getmonitor <- function(id, directory, summarize = FALSE) {
    dFrame <- read.csv(paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", sep = ""))
    if (summarize) {
        print(summary(dFrame))
    }
    return(dFrame)
}