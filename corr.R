corr <- function(directory, threshold = 0) {
	
	id = 1:332
	d <- numeric(0)
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
		if (n >= threshold) {
			try(d <- c(d, cor(sul, nit, use='complete.obs')))
			}	 # Would like to figure out how suppress the error 
	}			 # messages printed to screen in above try()
	d
}