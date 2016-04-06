rankhospital <- function(state, outcome, num) {
	
	data <- read.csv('outcome-of-care-measures.csv')
	
	if (!(state %in% data$State)) {
		stop("invalid state")
	}
	opt <- c('heart attack', 'heart failure', 'pneumonia')
	if (!(outcome %in% opt)) {
		stop("invalid outcome")
	}
	
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
	hosp <- as.character(hosp[order(rate, hosp)])	
	
	if (num == 'worst') {
		num <- length(rate[!is.na(rate)])
	} else if (num == 'best') {
		num <- 1
	}
	
	hosp[num]

}