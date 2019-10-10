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
plot.pers.time <- function(data, out.folder, daily=FALSE)
{	tlog(2,"Plotting number of mandate occurrences as a function of time")
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)

	# loop over all months in the period
	if(!daily)
	{	start.date <- get.first.day(start.date)
		tlog(4,"Start date: ",format(start.date))
		end.date <- get.first.day(end.date)
		tlog(4,"End date: ",format(end.date))
		month.dates <- c()
		month.vals <- c()
		
		cur.month <- start.date
		tlog(4,"Looping over time by 1-month increments")
		while(cur.month <= end.date)
		{	next.month <- addMonth(cur.month,1)
			
			month.idx <- which(sapply(1:nrow(data), function(r)
						date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.month, next.month-1)
					))
			month.val <- length(month.idx)
			tlog(6,"Processing period ",format(cur.month),"--",format(next.month-1),": ",month.val," occurrence(s)")
			month.vals <- c(month.vals, month.val)
			
			# update current date
			month.dates <- c(month.dates, cur.month)
			cur.month <- next.month
		}
		month.dates <- as.Date(month.dates,origin="1970-01-01")
	}
	# loop over all days in the period
	else
	{	tlog(4,"Start date: ",format(start.date))
		tlog(4,"End date: ",format(end.date))
		day.dates <- c()
		day.vals <- c()
		month.dates <- c()
		month.vals <- c()
		
		cur.day <- start.date
		cur.month <- get.first.day(start.date)
		month.idx <- c() 
		tlog(4,"Looping over time by 1-day increments")
		while(cur.day <= end.date)
		{	day <- as.integer(format(cur.day,format="%d"))
			next.day <- cur.day + 1
			
			day.idx <- which(sapply(1:nrow(data), function(r)
						date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
					))
			day.val <- length(day.idx)
			month.idx <- union(month.idx, day.idx)
			if(as.integer(format(next.day,format="%d"))==1)
			{	tlog(6,"Processing day ",format(next.day),": ",day.val," occurrence(s)")
				month.vals <- c(month.vals, length(month.idx))
				month.dates <- c(month.dates, cur.month)
				month.idx <- c()
				cur.month <- next.day
			}
			day.vals <- c(day.vals, day.val)
			
			# update current date
			day.dates <- c(day.dates, cur.day)
			cur.day <- next.day
		}
		day.dates <- as.Date(day.dates,origin="1970-01-01")
		month.dates <- as.Date(month.dates,origin="1970-01-01")
	}
	
	# record data in a text file
	if(daily)
	{	file <- file.path(out.folder,"persons_by_day.txt")
		tab <- data.frame(Date=format(day.dates,format="%d/%m/%Y"),Count=day.vals)
		write.table(x=tab,file=file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
	# same by month
	file <- file.path(out.folder,"persons_by_month.txt")
	tab <- data.frame(Date=format(month.dates,format="%d/%m/%Y"),Count=month.vals)
	write.table(x=tab,file=file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	
	# generate plot only starting from 2000
	if(daily)
	{	idx <- which(day.dates>as.Date("2000/1/1"))
		file <- file.path(out.folder,paste0("persons_by_day_2001.",PLOT_FORMAT))
		tlog(4, "Generating plot in file \"",file,"\"")
		if(PLOT_FORMAT=="pdf")
			pdf(file)
		else if(PLOT_FORMAT=="png")
			png(file, width=1024, height=1024)
		plot(
			x=as.Date(day.dates[idx], origin="1970-01-01"),
			y=day.vals[idx], 
			col="Red", 
			xlab="Dates", 
			ylab="Count",
			type="l",
#		las=2, 
#		cex.names=min(1,20/length(uvals))
		)
		dev.off()
	}
	# same by month
	idx <- which(month.dates>as.Date("2000/1/1"))
	file <- file.path(out.folder,paste0("persons_by_month_2001.",PLOT_FORMAT))
	tlog(4, "Generating plot in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	plot(
			x=as.Date(month.dates[idx], origin="1970-01-01"),
			y=month.vals[idx], 
			col="Red", 
			xlab="Dates", 
			ylab="Count",
			type="l",
#		las=2, 
#		cex.names=min(1,20/length(uvals))
	)
	dev.off()
	
	# generate plot for all dates
	if(daily)
	{	file <- file.path(out.folder,paste0("persons_by_day.",PLOT_FORMAT))
		tlog(4, "Generating plot in file \"",file,"\"")
		if(PLOT_FORMAT=="pdf")
			pdf(file)
		else if(PLOT_FORMAT=="png")
			png(file, width=1024, height=1024)
		plot(
			x=as.Date(day.dates, origin="1970-01-01"),
			y=day.vals, 
			col="Red", 
			xlab="Dates", 
			ylab="Count",
			type="l",
#		las=2, 
#		cex.names=min(1,20/length(uvals))
		)
		dev.off()
	}
	# same for months
	file <- file.path(out.folder,paste0("persons_by_month.",PLOT_FORMAT))
	tlog(4, "Generating plot in file \"",file,"\"")
	if(PLOT_FORMAT=="pdf")
		pdf(file)
	else if(PLOT_FORMAT=="png")
		png(file, width=1024, height=1024)
	plot(
		x=as.Date(month.dates, origin="1970-01-01"),
		y=month.vals, 
		col="Red", 
		xlab="Dates", 
		ylab="Count",
		type="l",
#		las=2, 
#		cex.names=min(1,20/length(uvals))
	)
	dev.off()
}
