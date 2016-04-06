pollutantmean <- function(directory, pollutant, id = 1:332) {
		
	pol <- numeric(0)
	for (i in id) {
		if (i < 100 & i > 9) {
			f <- paste0(c(directory, '/0', i, '.csv'), collapse='')
		} else if (i < 10) {
			f <- paste0(c(directory, '/00', i, '.csv'), collapse='')
		} else {
			f <- paste0(c(directory, '/', i, '.csv'), collapse='')
		}
		
		data <- read.csv(f)
		new_pol <- data[[pollutant]]
		pol <- c(pol, new_pol)
	}
	mean(pol, na.rm = TRUE)
}