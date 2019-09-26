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
#
# returns: a vector with the main stats.
#############################################################################################
check.col.numerical <- function(data, col, basename, ...)
{	vals <- data[,col]
	result <- c()
	
	# discard non-numerical values
	# note: should handle NaN and Inf too, but the files do not contain any of them
	tlog(4, "Look for non-numerical values")
	tmp <- which(is.na(vals))
	s <- length(tmp)
	result[COL_STATS_NA] <- s
	tlog(6, "NAs: ",s,"/",length(vals))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# unique (distinct) values
	uvals <- sort(unique(vals))
	s <- length(uvals)
	result[COL_STATS_UNQ] <- s
	
	# if not too many unique values, show them all
	tlog(4, "Distribution:")
	if(s<100)
	{	txt <- capture.output(print(table(vals)))
		for(t in txt)
			tlog(6, t)
	}
	else
		tlog(6, "Too many values (",length(uvals),")")
	
	# show signs
	tlog(4, "Signs:")
	tmp <- which(vals<0)
	s <- length(tmp)
	result[COL_STATS_NEG] <- s
	tlog(6, "Negative: ",s)
	tmp <- which(vals==0)
	s <- length(tmp)
	result[COL_STATS_ZER] <- s
	tlog(6, "Zero: ",length(tmp))
	tmp <- which(vals>0)
	s <- length(tmp)
	result[COL_STATS_POS] <- s
	tlog(6, "Positive: ",length(tmp))
	
	# show standard statistics
	tlog(4, "Standard statistics")
	tlog(6, "Number of unique values: ", length(uvals))
	ds <- unclass(summary(vals)) 
	result[COL_STATS_MIN] <- ds[1]
	tlog(6, "Min: ",ds[1])
	result[COL_STATS_Q1] <- ds[2]
	tlog(6, "1st quartile: ",ds[2])
	result[COL_STATS_MED] <- ds[3]
	tlog(6, "Median: ",ds[3])
	result[COL_STATS_Q3] <- ds[5]
	tlog(6, "3rd quartile: ",ds[5])
	result[COL_STATS_MAX] <- ds[6]
	tlog(6, "Max: ",ds[6])
	result[COL_STATS_AVG] <- ds[4]
	tlog(6, "Mean: ",ds[4])
	s <- sd(vals)
	result[COL_STATS_STD] <- s
	tlog(6, "Stdev: ",s)
	
	# plot histogram
	file <- paste0(basename,"_histo.",PLOT_FORMAT)
	tlog(4, "Plotting histogram in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	hist(vals, col="Red", main="Distribution", xlab=col)
	dev.off()
	# plot density
	file <- paste0(basename,"_dens.",PLOT_FORMAT)
	tlog(4, "Plotting density in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	plot(density(vals), col="Red", main="Kernel density", xlab=col)
	dev.off()
	# plot log-density
	file <- paste0(basename,"_logdens.",PLOT_FORMAT)
	tlog(4, "Plotting log density (only positive for values) in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	suppressWarnings(plot(density(vals), col="Red", main="Log Kernel density", xlab=col, log="y"))
	dev.off()
	
	return(result)
}




#############################################################################################
# Displays the main properties of the specified categorical column. 
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
#
# returns: a vector with the main stats.
#############################################################################################
check.col.categorical <- function(data, col, basename, ...)
{	vals <- data[,col]
	result <- c()
	
	# discard missing values
	tlog(4, "Look for missing values")
	tmp <- which(is.na(vals))
	s <- length(tmp)
	result[COL_STATS_NA] <- s
	tlog(6, "NAs: ",s,"/",length(vals))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# unique (distinct) values
	uvals <- sort(unique(vals))
	s <- length(uvals)
	result[COL_STATS_UNQ] <- s
	
	# if not too many unique values, show them all
	tlog(4, "Distribution:")
	if(s<100)
	{	txt <- capture.output(print(table(vals)))
		for(t in txt)
			tlog(6, t)
	}
	else
		tlog(6, "Too many values (",length(uvals),")")
	
	# show standard statistics
	tlog(4, "Standard statistics")
	tlog(6, "Number of unique values: ", s)
	s <- stat.mode(vals)
	result[COL_STATS_MOD] <- s[1]
	tlog(6, "Mode(s): ", paste(s,collapse=", "))
	
	# plot distribution
	file <- paste0(basename,"_bar.pdf")
	pdf(file)
		tlog(4, "Plotting barplot in file \"",file,"\"")
		barplot(table(vals), col="Red", xlab=col, ylab="Frequency", las=2, cex.names=min(1,20/length(uvals))) # TODO could switch xlab to main for space purposes
	dev.off()
	
	return(result)
}




#############################################################################################
# Displays the main properties of the specified nominal column. 
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
# dist.threhsold: distance threshold, used when comparing strings.
#
# returns: a vector with the main stats.
#############################################################################################
check.col.nominal <- function(data, col, basename, dist.threhsold=3, ...)
{	vals <- data[,col]
	result <- c()
	
	# discard missing values
	tlog(4, "Look for missing values")
	tmp <- which(is.na(vals))
	s <- length(tmp)
	result[COL_STATS_NA] <- s
	tlog(6, "NAs: ",s,"/",length(vals))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# unique (distinct) values
	uvals <- sort(unique(vals))
	s <- length(uvals)
	result[COL_STATS_UNQ] <- s
	
	# basic stats
	tlog(4, "Basic stats")
	tlog(6, "Number of unique values: ", s)
	tmp <- sort(table(vals),decreasing=TRUE)
	tlog(6, "Top 10 frequent values: ")
	txt <- capture.output(print(tmp[1:min(10,length(tmp))]))
	for(t in txt)
		tlog(8, t)
	
#	# compare strings
#	tlog(4, "Computing distances between unique values (may take a while)")
#	cntr <- 0
#	tmp <- lapply(1:length(uvals), function(i)
#		{	cntr <- cntr + 1
#			uval <- uvals[i]
#			d <- stringdist(uval,uvals)
#			idx <- which(d<=dist.threhsold & d!=0)
#			res <- uvals[idx]
#			tlog(6, uvals[i],": ",paste(res,collapse=", "))
#			return(res)
#		})
		
	# d <- stringdistmatrix(uvals) # too much memory needed
	
	return(result)
}




#############################################################################################
# Displays the main properties of the specified date column. 
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
#
# returns: a vector with the main stats.
#############################################################################################
check.col.temporal <- function(data, col, basename, ...)
{	vals <- data[,col]
	result <- c()
	
	# discard missing values
	tlog(4, "Look for missing values")
	tmp <- which(is.na(vals))
	s <- length(tmp)
	result[COL_STATS_NA] <- s
	tlog(6, "NAs: ",s,"/",length(vals))
	if(length(tmp)>0)
		vals <- vals[-tmp]
	
	# unique (distinct) values
	uvals <- sort(unique(vals))
	s <- length(uvals)
	result[COL_STATS_UNQ] <- s
	
	# if not too many unique values, show them all
	tlog(4, "Distribution:")
	if(s<100)
	{	txt <- capture.output(print(table(vals)))
		for(t in txt)
			tlog(6, t)
	}
	else
		tlog(6, "Too many values (",length(uvals),")")
	
	# show standard statistics
	tlog(4, "Standard statistics")
	tlog(6, "Number of unique values: ", s)
	ds <- unclass(summary(vals)) 
	ds <- as.Date(ds, origin="1970-01-01")
	s <- format(ds[1], format="%d/%m/%Y")
	result[COL_STATS_MIN] <- s
	tlog(6, "Min: ",s)
	s <- format(ds[2], format="%d/%m/%Y")
	result[COL_STATS_Q1] <- s
	tlog(6, "1st quartile: ",s)
	s <- format(ds[3], format="%d/%m/%Y")
	result[COL_STATS_MED] <- s
	tlog(6, "Median: ",s)
	s <- format(ds[5], format="%d/%m/%Y")
	result[COL_STATS_Q3] <- s
	tlog(6, "3rd quartile: ",s)
	s <- format(ds[6], format="%d/%m/%Y")
	result[COL_STATS_MAX] <- s
	tlog(6, "Max: ",s)
	s <- format(ds[4], format="%d/%m/%Y")
	result[COL_STATS_AVG] <- s
	tlog(6, "Mean: ",s)
	s <- sd(vals)
	result[COL_STATS_STD] <- s
	tlog(6, "Stdev: ",s, "days")
	
	# convert dates to epoch
	secs <- unclass(vals)
	mnd <- min(secs)
	mxd <- max(secs)
	ticks <- seq(from=mnd,to=mxd,by=(mxd-mnd)/5)
	
	# plot distribution
	file <- paste0(basename,"_histo.pdf") #TODO must fix this date problem
	pdf(file)
		tlog(4, "Plotting histogram in file \"",file,"\"")
		ht <- hist(secs, col="Red", main="Distribution", xlab=col, xaxt="n")
		axis(1, at=ticks, labels=format(as.Date(ticks, origin="1970-01-01"), format="%d/%m/%Y"), cex.axis=.7, las=2)
	dev.off()
	file <- paste0(basename,"_dens.pdf")
	pdf(file)
		tlog(4, "Plotting density in file \"",file,"\"")
		plot(density(secs), col="Red", main="Kernel density", xlab=col, xaxt="n")
		axis(1, at=ticks, labels=format(as.Date(ticks, origin="1970-01-01"), format="%d/%m/%Y"), cex.axis=.7, las=2)
	dev.off()
	file <- paste0(basename,"_logdens.pdf")
	pdf(file)
		tlog(4, "Plotting log density (only positive for values) in file \"",file,"\"")
		suppressWarnings({
			plot(density(secs), col="Red", main="Log Kernel density", xlab=col, log="y", xaxt="n");
			axis(1, at=ticks, labels=format(as.Date(ticks, origin="1970-01-01"), format="%d/%m/%Y"), cex.axis=.7, las=2)
		})
	dev.off()
	
	return(result)
}



#############################################################################################
# Displays the main properties of the specified column. Calls the appropriate function 
# depending on the type of the considered column.
#
# data: table containing the data.
# col: name of the column in the table.
# basename: string used to produce file names.
# tp: type of the column.
#
# returns: a vector with the main stats.
#############################################################################################
check.col <- function(data, col, basename, tp, ...)
{	# check if the column is empty
	if(all(is.na(data[,col])))
	{	tlog(4,"Only NA values: the column is ignored")
		result <- NULL
	}
	
	# otherwise, call the appropriate function
	else
	{	if(tp=="cat")
			result <- check.col.categorical(data=data, col=col, basename=basename, ...)
		else if(tp=="nom")
			result <- check.col.nominal(data=data, col=col, basename=basename, ...)
		else if(tp=="num")
			result <- check.col.numerical(data=data, col=col, basename=basename, ...)
		else if(tp=="dat")
			result <- check.col.temporal(data=data, col=col, basename=basename, ...)
	}
	
	return(result)
}



#############################################################################################
# Displays the main properties of the specified columns, for the specified data.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
check.cols <- function(data, cols, out.folder, ...)
{	# init stats table
	stats <- matrix(nrow=length(cols), ncol=length(COL_STATS_NAMES))
	colnames(stats) <- COL_STATS_NAMES
	rownames(stats) <- sapply(cols, function(col) col$name)
	
	# process each column separately
	for(c in 1:length(cols))
	{	col <- cols[[c]]
		tlog(2,"Considering column \"",col$name,"\"")
		
		# process the column
		res <- check.col(
				data=data, 
				col=col$name, 
				basename=file.path(out.folder,col$basename), 
				tp=col$tp,
				...
			)
		
		# update stats table
		if(length(res)>0)
		{	for(i in 1:length(res))
				stats[c,names(res)[i]] <- res[i]
		}
	}
	
	# record the stats table
	file <- file.path(out.folder,"stats.txt")
	write.table(x=stats, file=file, sep="\t", row.names=TRUE, col.names=TRUE, fileEncoding="UTF8", quote=FALSE)
}
