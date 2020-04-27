#############################################################################################
# Generate plots for each table.
# 
# 10/2019 Vincent Labatut
#
# source("src/verification/evolution_plot.R")
#############################################################################################




#############################################################################################
# plot constants
EVOL_COL_MEAS <- rgb(255,0,0,alpha=255,max=255)		# color of the measured values
EVOL_COL_LIM <- rgb(0,0,255,alpha=150,max=255)		# color of the legal limit
EVOL_TCK_MEAS <- 1									# thickness of the measured values
EVOL_TCK_LIM <- 2									# thickness of the legal limit





#############################################################################################
# Plots the evolution of the number of people holding a mandate simultaneously as a function
# of time.
#
# NOTE: this is the old version.
#
# data: table containing the data.
# out.folder: output folder.
#############################################################################################
plot.pers.time0 <- function(data, out.folder, daily=FALSE)
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
			
			month.idx <- which(future_sapply(1:nrow(data), function(r)
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
		{	tlog(6,"Day ",format(cur.day))
			
			day <- as.integer(format(cur.day,format="%d"))
			next.day <- cur.day + 1
			
#			day.idx <- which(
#					future_sapply(1:nrow(data), function(r)
#						date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
#					))
			day.idx <- c()
			for(r in 1:nrow(data))
			{	if(r %% 10000==0)
					tlog(8,r,"/",nrow(data))
				if(date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day))
					day.idx <- c(day.idx, r)
			}
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
	
	# loop over all rows
	day.idx <- start.date:end.date
	day.dates <- as.Date(day.idx,origin="1970-01-01")
	day.vals <- rep(0, length(day.idx))
	for(r in 1:nrow(data))
	{	sdate <- data[r,COL_ATT_MDT_DBT]
		edate <- data[r,COL_ATT_MDT_FIN]
		idx <- match(sdate:edate,day.idx)
		day.vals[idx] <- day.vals[idx] + rep(1,length(idx))
	}
	
	# record data in a text file
	if(daily)
	{	file <- file.path(out.folder,"persons_by_day.txt")
		tab <- data.frame(Date=format(day.dates,format="%d/%m/%Y"),Count=day.vals)
		write.table(x=tab, file=file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
		)
	}
	# same by month
	file <- file.path(out.folder,"persons_by_month.txt")
	tab <- data.frame(Date=format(month.dates,format="%d/%m/%Y"),Count=month.vals)
	write.table(x=tab,file=file,
#			fileEncoding="UTF-8",
			row.names=FALSE,
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
	)
	
	# generate plot only starting from 2000
	if(daily)
	{	idx <- which(day.dates>as.Date("2000/1/1"))
		for(plot.format in PLOT_FORMATS)
		{	file <- file.path(out.folder,paste0("persons_by_day_2001.",plot.format))
			tlog(4, "Generating plot in file \"",file,"\"")
			if(plot.format=="pdf")
				pdf(file)
			else if(plot.format=="png")
				png(file, width=1024, height=1024)
			plot(
				x=as.Date(day.dates[idx], origin="1970-01-01"),
				y=day.vals[idx], 
				col=EVOL_COL_MEAS, 
				xlab="Dates", 
				ylab="Count",
				type="l",
#				las=2, 
#				cex.names=min(1,20/length(uvals))
			)
			dev.off()
		}
	}
	# same by month
	idx <- which(month.dates>as.Date("2000/1/1"))
	for(plot.format in PLOT_FORMATS)
	{	file <- file.path(out.folder,paste0("persons_by_month_2001.",plot.format))
		tlog(4, "Generating plot in file \"",file,"\"")
		if(plot.format=="pdf")
			pdf(file)
		else if(plot.format=="png")
			png(file, width=1024, height=1024)
		plot(
			x=as.Date(month.dates[idx], origin="1970-01-01"),
			y=month.vals[idx], 
			col=EVOL_COL_MEAS, 
			xlab="Dates", 
			ylab="Count",
			type="l",
#			las=2, 
#			cex.names=min(1,20/length(uvals))
		)
		dev.off()
	}
	
	# generate plot for all dates
	if(daily)
	{	for(plot.format in PLOT_FORMATS)
		{	file <- file.path(out.folder,paste0("persons_by_day.",plot.format))
			tlog(4, "Generating plot in file \"",file,"\"")
			if(plot.format=="pdf")
				pdf(file)
			else if(plot.format=="png")
				png(file, width=1024, height=1024)
			plot(
				x=as.Date(day.dates, origin="1970-01-01"),
				y=day.vals, 
				col=EVOL_COL_MEAS, 
				xlab="Dates", 
				ylab="Count",
				type="l",
#				las=2, 
#				cex.names=min(1,20/length(uvals))
			)
			dev.off()
		}
	}
	# same for months
	for(plot.format in PLOT_FORMATS)
	{	file <- file.path(out.folder,paste0("persons_by_month.",plot.format))
		tlog(4, "Generating plot in file \"",file,"\"")
		if(plot.format=="pdf")
			pdf(file)
		else if(plot.format=="png")
			png(file, width=1024, height=1024)
		plot(
			x=as.Date(month.dates, origin="1970-01-01"),
			y=month.vals, 
			col=EVOL_COL_MEAS, 
			xlab="Dates", 
			ylab="Count",
			type="l",
#			las=2, 
#			cex.names=min(1,20/length(uvals))
		)
		dev.off()
	}
}




#############################################################################################
# Adds to an existing plots some vertical lines representing election dates. If this information
# is not available, the plot is not changed.
#
# type: category of the data table (CD, CM, etc.).
# start.date: earliest date covered by the plot.
# end.date: latest date covered by the plot.
# max.val: maximal plotted value (y-axis).
#############################################################################################
plot.election.dates <- function(type, start.date, end.date, max.val)
{	# get the election dates
	if(type=="CD")
		election.table <- load.election.data(data, election.file=FILE_VERIF_DATES_CD, series.file=FILE_VERIF_SERIES_CD)$election.table
	else if(type=="CM" || type=="EPCI" || type=="M")
		election.table <- load.election.data(data, election.file=FILE_VERIF_DATES_CM)$election.table
	else if(type=="CR")
		election.table <- load.election.data(data, election.file=FILE_VERIF_DATES_CR)$election.table
	else if(type=="D")
		election.table <- load.election.data(data, election.file=FILE_VERIF_DATES_D)$election.table
	else if(type=="DE")
		election.table <- load.election.data(data, election.file=FILE_VERIF_DATES_DE)$election.table
	else if(type=="S")
	{	election.table <- load.election.data(data, election.file=FILE_VERIF_DATES_S, series.file=FILE_VERIF_SERIES_S)$election.table
		election.table[which(election.table[,3]=="c1,c2"),3] <- "c"
		election.table[which(election.table[,3]=="a,b,c1,c2"),3] <- "_"
	}
	
	# plot them as vertical lines
	ticks <- c()
	labs <- c()
	for(i in 1:nrow(election.table))
	{	pos <- election.table[i,1]
		if(pos>=start.date)
		{	# plot the first round
			abline(v=pos, col="BLACK", lty=3)
			# possibly plot the second round
			if(!is.na(election.table[i,2]))
			{	pos <- election.table[i,2]
				abline(v=pos, col="BLACK", lty=3)
			}
			# possibly plot the electoral series
			if(ncol(election.table)>2 && !is.na(election.table[i,3]))
			{	#text(pos,max.val,election.table[i,3],pos=4,col="DARKGRAY")
				ticks <- c(ticks, pos)
				labs <- c(labs, election.table[i,3])
			}
		}
	}
	
	# add the top axis with the series
	if(ncol(election.table)>2)
	{	axis(side=3, 			# top axis
			at=ticks, 			# where to put text
			labels=labs, 		# text to show
			cex.axis=0.75,		# smaller text		
			tick=FALSE, 		# hide tick marks
			col="DARKGRAY", 	# text color
			mgp=c(3, .25, 0)	# closer to axis
		)
	}
}




#############################################################################################
# Adds to an existing plot some lines representing the legal maximal number of positions for
# the specified type, if the information is available. Other wise, the function does nothing.
#
# type: category of the data table (CD, CM, etc.).
# start.date: earliest date covered by the plot.
# end.date: latest date covered by the plot.
#############################################################################################
plot.position.limit <- function(type, start.date, end.date)
{	limit.dates <- c()
	limit.vals <- c()
	
	# setup limit changes
	if(type=="CD")
	{	# old values
		#limit.dates <- c(as.Date("1789/12/22"),as.Date("1793/12/4"),
		#		as.Date("1800/2/17"),as.Date("1871/8/10"),as.Date("2015/3/22"))
		limit.dates <- c(as.Date("1967/09/24"), as.Date("1976/03/07"), as.Date("1985/03/10"),
			as.Date("2015/03/22"), as.Date("2015/12/16"), as.Date("2018/01/01"))
		limit.vals <- c(3988,4048,4055,4177,4113,4061)
	}
	else if(type=="CM")
	{	limit.dates <- c(as.Date("1884/4/5"))
		limit.vals <- c(503305)
	}
	else if(type=="CR")
	{	limit.dates <- c(as.Date("1986/3/10"),as.Date("2015/12/5"))
		limit.vals <- c(1821,1757)
	}
	else if(type=="EPCI")
	{	limit.dates <- c(as.Date("1992/2/6"))
		limit.vals <- c(67159)
	}
	else if(type=="D")
	{	limit.dates <- c(as.Date("1958/11/23"), as.Date("1962/11/18"),
			as.Date("1967/3/5"), as.Date("1968/6/23"), as.Date("1973/3/4"),
			as.Date("1978/3/12"), as.Date("1981/6/14"), as.Date("1986/3/16"),
			as.Date("1988/6/5"), as.Date("1993/3/21"), as.Date("1997/5/25"),
			as.Date("2002/6/9"), as.Date("2007/6/10"), as.Date("2012/6/10"),
			as.Date("2017/6/11"))
		limit.vals <- c(579, 482, 487, 487, 490, 491, 491, 577, 577, 577, 
				577, 577, 577, 577, 577)
	}
	else if(type=="DE")
	{	limit.dates <- c(as.Date("1979/06/07"), as.Date("1994/06/12"), 
			as.Date("2004/06/13"), as.Date("2009/06/07"), as.Date("2011/12/07"),
			as.Date("2020/2/1"))
		limit.vals <- c(81, 87, 78, 72, 74, 79)
	}
	else if(type=="M")
	{	limit.dates <- c(as.Date("1962/1/1"), 
			as.Date("1968/1/1"), as.Date("1975/1/1"),
			as.Date("1982/1/1"), as.Date("1985/1/1"), 
			as.Date("1990/1/1"), as.Date("1994/1/1"), 
			seq(as.Date("1999/1/1"),as.Date("2019/1/1"),"year"))
		limit.vals <- c(38076, 37823, 36407, 36547, 36614, 36664, 36673, 36679,
			36680, 36677, 36679, 36678, 36682, 36684, 36685, 36683, 36681,
			36682, 36682, 36680, 36700, 36681, 36681, 36658, 35885, 35416,
			35357, 34968)
	}
	else if(type=="S")
	{	limit.dates <- c(as.Date("1959/4/26"), as.Date("1962/9/23"), 
			as.Date("1965/9/26"), as.Date("1968/9/22"), as.Date("1971/9/26"), 
			as.Date("1974/9/22"), as.Date("1977/9/25"), as.Date("1980/9/28"), 
			as.Date("1983/9/25"), as.Date("1986/9/28"), as.Date("1989/9/24"), 
			as.Date("1992/9/27"), as.Date("1995/9/24"), as.Date("1998/9/27"), 
			as.Date("2001/9/23"), as.Date("2004/9/26"), as.Date("2008/9/21"), 
			as.Date("2011/9/25"), as.Date("2014/9/28"), as.Date("2017/9/24"))
		limit.vals <- c(307, 274, 274, 283, 283, 283, 295, 305, 317, 319,
				321, 321, 321, 321, 321, 331, 343, 348, 348, 348)
	}
	
	# plot each segment
	if(length(limit.vals)>0)
	{	for(i in 1:length(limit.vals))
		{	# get start date
			sd <- limit.dates[i]
			# get end date
			if(i==length(limit.vals))
				ed <- max(end.date, sd+1)
			else
				ed <- limit.dates[i+1]
			# plot main segment
			segments(x0=sd, y0=limit.vals[i], 
				x1=ed, y1=limit.vals[i], 
				col=EVOL_COL_LIM,
				lwd=EVOL_TCK_LIM)
			# possibly prepare next change
			if(i<length(limit.vals))
				segments(x0=ed, y0=limit.vals[i], 
					x1=ed, y1=limit.vals[i+1], 
					col=EVOL_COL_LIM,
					lwd=EVOL_TCK_LIM)
		}
	}
}



#############################################################################################
# Plots the evolution of the number of people holding a mandate simultaneously as a function
# of time. 
#
# data: table containing the data.
# out.folder: output folder.
# type: table processed (CD, CM, CR, etc.)
#############################################################################################
plot.pers.time <- function(data, out.folder, type)
{	tlog(2,"Plotting number of mandate occurrences as a function of time")
	
	# remove functions in order to ignore them
	if(COL_ATT_FCT_CODE %in% colnames(data) || COL_ATT_FCT_NOM %in% colnames(data))
	{	# set all function info to NA
		if(COL_ATT_FCT_CODE %in% colnames(data)) data[,COL_ATT_FCT_CODE] <- rep(NA, nrow(data))
		if(COL_ATT_FCT_NOM %in% colnames(data)) data[,COL_ATT_FCT_NOM] <- rep(NA, nrow(data))
		data[,COL_ATT_FCT_DBT] <- rep(NA, nrow(data))
		data[,COL_ATT_FCT_FIN] <- rep(NA, nrow(data))
		if(COL_ATT_FCT_MOTIF %in% colnames(data)) data[,COL_ATT_FCT_MOTIF] <- rep(NA, nrow(data))
		# merge overlapping mandates to collapse rows that were only differing by their function
		data <- merge.overlapping.mandates(data, type="D", log=FALSE)
	}
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
## forcing the start date to be less than it should, to go faster (debug)
#start.date <- max(start.date, as.Date("1998/1/1"))	
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Period: ",format(start.date),"--",format(end.date))
	
	# loop over all rows
	day.idx <- start.date:end.date
	day.dates <- as.Date(day.idx,origin="1970-01-01")
	day.vals <- rep(0, length(day.idx))
	tlog.start.loop(4,nrow(data),"Looping over the data row by row")
	for(r in 1:nrow(data))
	{	if(r %% 10000==0)
			tlog.loop(8,r,r,"/",nrow(data)," rows processed")
		sdate <- data[r,COL_ATT_MDT_DBT]
		edate <- data[r,COL_ATT_MDT_FIN]
		if(!(is.na(sdate) && is.na(edate)))
		{	if(is.na(sdate))
				sdate <- start.date
			if(is.na(edate))
				edate <- end.date
		
			idx <- match(sdate:edate,day.idx)
## when forcing the start date to be less than it should, to go faster (debug)
#idx <- idx[!is.na(idx)]
#if(length(idx)>0)
			day.vals[idx] <- day.vals[idx] + rep(1,length(idx))
		}
	}
	tlog.end.loop(4,"Loop over")
	
	# record data in a text file
	file <- file.path(out.folder,"persons_by_day.txt")
	tab <- data.frame(Date=format(day.dates,format="%d/%m/%Y"),Count=day.vals)
	write.table(x=tab,file=file,
#		fileEncoding="UTF-8",
		row.names=FALSE,
		col.names=TRUE,
#		quote=TRUE,
		sep="\t"
	)
	
	# set up parameters to cover various time periods
	focus.date <- as.Date("2000/1/1")
	if(type=="CD" || type=="DE")
		focus.date <- as.Date("1978/1/1")
	else if(type=="S")
		focus.date <- as.Date("1957/1/1")
	start.dates <- c(
		min(as.Date(day.dates, origin="1970-01-01")),	# earliest date in the data
		as.Date("2000/1/1"),							# right before RNE creation
		focus.date										# depends on the mandate type
	)
	end.dates <- c(
		max(as.Date(day.dates, origin="1970-01-01")),	# latest date in the data
		as.Date("2018/7/31"),							# RNE extraction date
		as.Date("2020/5/1")								# (approximately) current date
	)
	file.names <- c(
		"persons_by_day_all",
		"persons_by_day_2001",
		"persons_by_day_focus"
	)
	
	# generate several plots focusing on different periods
	for(i in 1:length(start.dates))
	{	start.year <- get.year(start.dates[i])
		start.date <- as.Date(paste0(start.year,"/1/1"))
		end.year <- as.integer(get.year(end.dates[i])) + 1
		end.date <- as.Date(paste0(end.year,"/1/1"))
		idx <- which(day.dates>=start.date & day.dates<=end.date)
		
		for(plot.format in PLOT_FORMATS)
		{	file <- file.path(out.folder,paste0(file.names[i],".",plot.format))
			tlog(4, "Generating plot in file \"",file,"\"")
			if(plot.format=="pdf")
				pdf(file, width=11, height=7)
			else if(plot.format=="png")
				png(file, width=1024, height=1024)
			par(mar=c(5, 4, 1.5, 0)+0.1)	# B L T R
			# create plot
			plot(x=NULL,
				xlab="Date", 
				ylab="Nombre de mandats",
				xaxt="n", yaxt="n",
				xlim=c(start.date, end.date),
				ylim=range(day.vals[idx])
#				cex.names=min(1,20/length(uvals))
			)
			# setup x axis
			if(as.integer(end.year) - as.integer(start.year) > 20)
				unit <- "2 year"
			else 
				unit <- "year"
			ticks <- seq(start.date, end.date, unit)
			axis(side=1, at=ticks, labels=get.year(ticks), las=2)
			# setup y axis
			ticks <- axTicks(2)
			axis(side=2, at=ticks, labels=format(ticks,scientific=FALSE))
			# plot election dates as vertical lines
			plot.election.dates(type,
				start.date=start.date, 
				end.date=end.date,
				max.val=max(day.vals[idx]))
			# plot theoretical limit
			plot.position.limit(type, 
				start.date=start.date, 
				end.date=end.date)
			# plot main data
			lines(x=as.Date(day.dates[idx], origin="1970-01-01"),
				y=day.vals[idx], 
				col=EVOL_COL_MEAS, 
				lwd=EVOL_TCK_MEAS,
				type="l"
			)
			# add legend
			legend(x="bottomright", 
				legend=c("Valeur mesuree", "Valeur theorique"), 
				fill=c(EVOL_COL_MEAS, EVOL_COL_LIM), 
				bg="WHITE")
			# restaure options
			par(mar=c(5, 4, 4, 2)+0.1)	# B L T R
			dev.off()
		}
	}
}
