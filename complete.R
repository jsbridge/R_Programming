complete <- function(directory, id = 1:332) {
	
	nn <- numeric(0)
	for (i in id) {
		if (i < 100 & i > 9) {
			f <- paste0(c(directory, '/0', i, '.csv'), collapse='')
		} else if (i < 10) {
			f <- paste0(c(directory, '/00', i, '.csv'), collapse='')
		} else {
			f <- paste0(c(directory, '/', i, '.csv'), collapse='')
		}
	
		data = read.csv(f)
		sul <- data[['sulfate']]
		nit <- data[['nitrate']]
		n <- sum(complete.cases(sul, nit))
		nn <- c(nn, n)
	}
	data.frame(ids = id, nobs = nn)
}