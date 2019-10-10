#############################################################################################
# Generate plots for each table.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Plots the evolution of the number of people holding a mandate simultaneously as a function
# of time. 
#
# data: table containing the data.
# out.folder: output folder.
#############################################################################################
plot.pers.time <- function(data, out.folder)
{	tlog(2,"Plotting number of mandate occurrences as a function of time")
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	start.date <- get.first.day(start.date)
	tlog(4,"Start date: ",format(start.date))
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- get.first.day(end.date)
	tlog(4,"End date: ",format(end.date))
	
	# loop over all months in the period
	date <- start.date
	dates <- c()
	vals <- c()
	tlog(4,"Looping over time by 1-month increments")
	while(date <= end.date)
	{	next.date <- addMonth(date,1)
		
		idx <- which(sapply(1:nrow(data), function(r)
					date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], date, next.date)
				))
		val <- length(idx)
		tlog(6,"Processing period ",format(date),"--",format(next.date),": ",val," occurrence(s)")
		vals <- c(vals, val)
		
		# update current date
		dates <- c(dates, date)
		date <- next.date
	}
	dates <- as.Date(dates,origin="1970-01-01")
	
	# record data in a text file
	file <- file.path(out.folder,"persons_vs_time.txt")
	tab <- data.frame(Date=format(dates,format="%d/%m/%Y"),Count=vals)
	write.table(x=tab,file=file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	
	# generate plot only starting from 2000
	idx <- which(dates>as.Date("2000/1/1"))
	file <- file.path(out.folder,paste0("persons_vs_time_2001.",PLOT_FORMAT))
	tlog(4, "Generating plot in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	plot(
			x=as.Date(dates[idx], origin="1970-01-01"),
			y=vals[idx], 
			col="Red", 
			xlab="Dates", 
			ylab="Count",
			type="l",
#		las=2, 
#		cex.names=min(1,20/length(uvals))
	)
	dev.off()
	
	# generate plot for all dates
	file <- file.path(out.folder,paste0("persons_vs_time.",PLOT_FORMAT))
	tlog(4, "Generating plot in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	plot(
		x=as.Date(dates, origin="1970-01-01"),
		y=vals, 
		col="Red", 
		xlab="Dates", 
		ylab="Count",
		type="l",
#		las=2, 
#		cex.names=min(1,20/length(uvals))
	)
	dev.off()
}
