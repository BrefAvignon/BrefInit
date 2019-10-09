#############################################################################################
# Functions used to handle dates.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Takes a date, adds n months, and returns the resulting date.
# This function was written by StackOverflow user Jacob Amos, and is available there:
# https://stackoverflow.com/a/25025767/1254730
#
# date: input date.
# n: number of months to add.
#
# returns: resulting date.
#############################################################################################
addMonth <- function(date, n=1)
{	if (n == 0){return(date)}
	if (n %% 1 != 0){stop("Input Error: argument 'n' must be an integer.")}
	
	# Check to make sure we have a standard Date format
	if (class(date) == "character"){date = as.Date(date)}
	
	# Turn the year, month, and day into numbers so we can play with them
	y = as.numeric(substr(as.character(date),1,4))
	m = as.numeric(substr(as.character(date),6,7))
	d = as.numeric(substr(as.character(date),9,10))
	
	# Run through the computation
	i = 0
	# Adding months
	if (n > 0){
		while (i < n){
			m = m + 1
			if (m == 13){
				m = 1
				y = y + 1
			}
			i = i + 1
		}
	}
	# Subtracting months
	else if (n < 0){
		while (i > n){
			m = m - 1
			if (m == 0){
				m = 12
				y = y - 1
			}
			i = i - 1
		}
	}
	
	# If past 28th day in base month, make adjustments for February
	if (d > 28 & m == 2){
		# If it's a leap year, return the 29th day
		if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){d = 29}
		# Otherwise, return the 28th day
		else{d = 28}
	}
	# If 31st day in base month but only 30 days in end month, return 30th day
	else if (d == 31){if (m %in% c(1, 3, 5, 7, 8, 10, 12) == FALSE){d = 30}}
	
	# Turn year, month, and day into strings and put them together to make a Date
	y = as.character(y)
	
	# If month is single digit, add a leading 0, otherwise leave it alone
	if (m < 10){m = paste('0', as.character(m), sep = '')}
	else{m = as.character(m)}
	
	# If day is single digit, add a leading 0, otherwise leave it alone
	if (d < 10){d = paste('0', as.character(d), sep = '')}
	else{d = as.character(d)}
	
	# Put them together and convert return the result as a Date
	return(as.Date(paste(y,'-',m,'-',d, sep = '')))
}




#############################################################################################
# Takes a date and returns the date of the first day of the same month.
#
# date: input date.
#
# returns: date of the first day of the same month.
#############################################################################################
get.first.day <- function(date)
{	# break down the date
	y = as.numeric(substr(as.character(date),1,4))
	m = as.numeric(substr(as.character(date),6,7))
	d = as.numeric(substr(as.character(date),9,10))
	
	# set the day to first 
	d <- 1
	
	# put together and return
	result <- as.Date(paste(y,'-',m,'-',d,sep=''))
	return(result)
}




#############################################################################################
# Checks whether the specified periods intersect.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: TRUE iff the periods intersect.
#############################################################################################
date.intersect <- function(start1, end1, start2, end2)
{	
	if(start1<start2)
	{	if(end1<start2)
			result <- FALSE
		else
			result <- TRUE
	}
	else
	{	if(start1>end2)
			result <- FALSE
		else
			result <- TRUE
	}
	
	return(result)
}
