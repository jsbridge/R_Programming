rankall <- function(outcome, num = 'best') {
	
	data <- read.csv('outcome-of-care-measures.csv')
	
	opt <- c('heart attack', 'heart failure', 'pneumonia')
	if (!(outcome %in% opt)) {
		stop("invalid outcome")
	}
	
	names <- character(0)
	states <- character(0)
	for (state in unique(data$State)) {
	
		ind <- data$State == state
		hosp <- data$Hospital.Name[ind]
		if (outcome == opt[1]) {
			rate <- (data[[11]])[ind]
		} else if (outcome == opt[2]) {
			rate <- (data[[17]])[ind]
		} else {
			rate <- (data[[23]])[ind]
		}

                rate <- suppressWarnings(as.numeric(levels(rate))[rate])
		hosp <- as.character(hosp[order(rate)])

		if (num == 'worst') {
                        newnum <- length(rate[!is.na(rate)])
		} else if (num == 'best') {
                    newnum <- 1
		} else { newnum <- num }
                
		names <- c(names, hosp[newnum])
		states <- c(states, state)
	}
	a <- data.frame(hospitals = names, state = states, row.names = unique(data$State))
	a[order(a$state, a$hospital),] # Error in ordering somewhere
	
}
