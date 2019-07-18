#############################################################################################
# Functions used when verifying the content of the original tables
# 
# 07/2019 Vincent Labatut
#############################################################################################
library("stringdist")


#############################################################################################
# Displays the main properties of the specified numerical column. 
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
#############################################################################################
check.col.numerical <- function(data, col, basename)
{	vals <- data[,col]
	# unique (distinct) values
	uvals <- sort(unique(vals))
	
	# if not too many unique values, show them all
	tlog(4, "Distribution:")
	if(length(uvals)<100)
	{	txt <- capture.output(print(table(vals)))
		for(t in txt)
			tlog(6, t)
	}
	else
		tlog(6, "Too many values")
	
	# discard non-numerical values
	# note: should handle NaN and Inf too, but the files do not contain any of them
	tlog(4, "Look for non-numerical values")
	tmp <- which(is.na(vals))
	tlog(6, "NA: ", length(tmp))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# show signs
	tlog(4, "Signs:")
	tmp <- which(vals<0)
	tlog(6, "Negative: ",length(tmp))
	tmp <- which(vals==0)
	tlog(6, "Zero: ",length(tmp))
	tmp <- which(vals>0)
	tlog(6, "Positive: ",length(tmp))
	
	# show standard statistics
	tlog(4, "Standard statistics")
	t5 <- fivenum(vals) 
	tlog(6, "Min: ",t5[1])
	tlog(6, "1st quartile: ",t5[2])
	tlog(6, "Median: ",t5[3])
	tlog(6, "3rd quartile: ",t5[4])
	tlog(6, "Max: ",t5[5])
	tlog(6, "Mean: ",mean(vals))
	tlog(6, "Stdev: ",sd(vals))
	
	# plot distribution
	file <- file.path(FOLDER_OUT, paste0(basename,"_histo.pdf"))
	pdf(file)
		tlog(4, "Plotting histogram in file \"",file,"\"")
		hist(vals, col="Red", main="Distribution", xlab=col)
	dev.off()
	file <- file.path(FOLDER_OUT, paste0(basename,"_dens.pdf"))
	pdf(file)
		tlog(4, "Plotting density in file \"",file,"\"")
		plot(density(vals), col="Red", main="Kernel density", xlab=col)
	dev.off()
	file <- file.path(FOLDER_OUT, paste0(basename,"_logdens.pdf"))
	pdf(file)
		tlog(4, "Plotting log density (only positive for values) in file \"",file,"\"")
		suppressWarnings(
			plot(density(vals), col="Red", main="Log Kernel density", xlab=col, log="y")
		)
	dev.off()
}




#############################################################################################
# Displays the main properties of the specified categorical column. 
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
#############################################################################################
check.col.categorical <- function(data, col, basename)
{	vals <- data[,col]
	# unique (distinct) values
	uvals <- sort(unique(vals))
	
	# if not too many unique values, show them all
	tlog(4, "Distribution:")
	if(length(uvals)<100)
	{	txt <- capture.output(print(table(vals)))
		for(t in txt)
			tlog(6, t)
	}
	else
		tlog(6, "Too many values")
	
	# discard missing values
	tlog(4, "Look for missing values")
	tmp <- which(is.na(vals))
	tlog(6, "NA: ", length(tmp))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# show standard statistics
	tlog(4, "Standard statistics")
	tlog(6, "Mode(s): ", paste(stat.mode(vals),collapse=", "))
	
	# plot distribution
	file <- file.path(FOLDER_OUT, paste0(basename,"_bar.pdf"))
	pdf(file)
		tlog(4, "Plotting barplot in file \"",file,"\"")
		barplot(table(vals), col="Red", main="Frequencies", xlab=col, las=2)
#		barplot(table(vals), col="Red", main="Frequencies", xlab=col, las=2, xaxt="n")
#		axis(1,cex.axis=0.2)
	dev.off()
}




#############################################################################################
# Displays the main properties of the specified nominal column. 
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
#############################################################################################
check.col.nominal <- function(data, col, basename)
{	vals <- data[,col]
	# unique (distinct) values
	uvals <- sort(unique(vals))
	
	# discard missing values
	tlog(4, "Look for missing values")
	tmp <- which(is.na(vals))
	tlog(6, "NA: ", length(tmp))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# basic stats
	tlog(4, "Basic stats")
	tlog(6, "Number of unique values: ", length(uvals))
	tmp <- sort(table(vals),decreasing=TRUE)
	tlog(6, "Top 10 frequent values: ")
	txt <- capture.output(print(tmp[1:min(10,length(tmp))]))
	for(t in txt)
		tlog(8, t)
	
	# compare strings
	tlog(4, "Computing distances between unique values")
	d <- stringdistmatrix(uvals)
	
	
}
