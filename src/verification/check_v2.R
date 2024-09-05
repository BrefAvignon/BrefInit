# Generate some stats for the v2 of the BRÉF
# 
# 08/2024 Vincent Labatut
# 
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/BrefInit")
# setwd("C:/Users/Vincent/Documents/Travail/Recherche/_Projets/Trajectoires pol - Noémie Fevrat/05. Données/02. Données BRÉF v1/6. Code source + stats/BrefInit")
# source("src/verification/check_v2.R")
###############################################################################
library("devEMF")

source("src/common/include.R")
source("src/verification/evolution_plot.R")




###############################################################################
# inititalization

# start logging
start.rec.log(text=paste0("v2Evolution"))

# create output folder
in.folder <- file.path(FOLDER_IN, "bref_v2")
out.folder <- file.path(FOLDER_OUT, "bref_v2")
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)




###############################################################################
# load data tables

# load the mandates
tab.file <- file.path(in.folder, "Mandate.csv")
tab.mandates <- read.table(
	file=tab.file, 				# name of the data file
	header=TRUE, 				# look for a header
	sep=",", 					# character used to separate columns 
	check.names=FALSE, 			# don't change the column names from the file
	comment.char="", 			# ignore possible comments in the content
	row.names=NULL, 			# don't look for row names in the file
#	quote="", 					# don't expect double quotes "..." around text fields
	as.is=TRUE,					# don't convert strings to factors
	colClasses="character"		# all columns originally read as characters, then converted later if needed
#	fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
)
# replace NULLs by NAs
for(c in 1:ncol(tab.mandates))
{	idx <- which(tab.mandates[,c]=="NULL")
	tab.mandates[idx,c] <- NA
}
# convert dates
for(col in c("StartDateMandate","EndDateMandate"))
{	vals <- as.Date(tab.mandates[,col], "%Y-%m-%d")
	idx <- which(colnames(tab.mandates)==col)
	tab.mandates <- data.frame(tab.mandates[,1:(idx-1)], vals, tab.mandates[,(idx+1):ncol(tab.mandates)])
	colnames(tab.mandates)[idx] <- col
}
#print(colnames(tab.mandates))
#sapply(1:ncol(tab.mandates), function(col) class(tab.mandates[,col]))

## load the functions
#tab.file <- file.path(in.folder, "Function.csv")
#tab.functions <- read.table(
#	file=tab.file, 				# name of the data file
#	header=TRUE, 				# look for a header
#	sep=",", 					# character used to separate columns 
#	check.names=FALSE, 			# don't change the column names from the file
#	comment.char="", 			# ignore possible comments in the content
#	row.names=NULL, 			# don't look for row names in the file
##	quote="", 					# don't expect double quotes "..." around text fields
#	as.is=TRUE,					# don't convert strings to factors
#	colClasses="character"		# all columns originally read as characters, then converted later if needed
##	fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
#)
## replace NULLs by NAs
#for(c in 1:ncol(tab.functions))
#{	idx <- which(tab.functions[,c]=="NULL")
#	tab.functions[idx,c] <- NA
#}
## convert dates
#tab.functions <- cbind(rep(1,nrow(tab.functions)), tab.functions)	# dummy column, dirty workaround (removed later)
#for(col in c("StartDateFunction","EndDateFunction"))
#{	vals <- as.Date(tab.functions[,col], "%Y-%m-%d")
#	idx <- which(colnames(tab.functions)==col)
#	tab.functions <- data.frame(tab.functions[,1:(idx-1)], vals, tab.functions[,(idx+1):ncol(tab.functions)])
#	colnames(tab.functions)[idx] <- col
#}
#tab.functions <- tab.functions[,-1]
##print(colnames(tab.mandates))
##sapply(1:ncol(tab.functions), function(col) class(tab.functions[,col]))




###############################################################################
# produce a plot for each mandate type

# map to convert mandate types to int values as in the table
mandate.type.map <- c(
	"CD"=2,
	"EPCI"=3,
	"CM"=4,
	"CR"=5,
	"D"=6,
	"DE"=8,
	"S"=9
)

###############################################################################
# Adaptation of the original plot.pers.time (designed for the v1 of the database),
# to the v2.
#
# data: table containing the mandates.
# out.folder: output folder.
# type: targete type of mandate (CD, CM, CR, etc.)
###############################################################################
plot.pers.time2 <- function(data, out.folder, type)
{	tlog(2,"Plotting number of mandate occurrences as a function of time")
	
	# keep only the rows matching the mandate type
	idx <- which(data[,"TypeMandate"]==mandate.type.map[type])
	data <- data[idx,]
	
	# set up start/end dates
	start.date <- min(c(data[,"StartDateMandate"], data[,"EndDateMandate"]), na.rm=TRUE)
## forcing the start date to be less than it should, to go faster (debug)
#start.date <- max(start.date, as.Date("1998/1/1"))	
	end.date <- max(c(data[,"StartDateMandate"], data[,"EndDateMandate"]), na.rm=TRUE)
	tlog(4,"Period: ",format(start.date),"--",format(end.date))
	
	# loop over all rows
	day.idx <- start.date:end.date
	day.dates <- as.Date(day.idx, origin="1970-01-01")
	day.vals <- rep(0, length(day.idx))
	tlog.start.loop(4,nrow(data),"Looping over the data row by row")
	for(r in 1:nrow(data))
	{	if(r %% 10000==0)
			tlog.loop(8,r,r,"/",nrow(data)," rows processed")
		sdate <- data[r,"StartDateMandate"]
		edate <- data[r,"EndDateMandate"]
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
	file <- file.path(out.folder, paste0(type, "_persons_by_day.txt"))
	tab <- data.frame(Date=format(day.dates,format="%d/%m/%Y"), Count=day.vals)
	write.table(x=tab,file=file,
#		fileEncoding="UTF-8",
		row.names=FALSE,
		col.names=TRUE,
#		quote=TRUE,
		sep="\t"
	)
	
	# set up parameters to cover various time periods
	focus.date <- as.Date("2000/1/1")
	{	if(type=="DE")
			focus.date <- as.Date("1978/1/1")
		else if(type=="S")
			focus.date <- as.Date("1957/1/1")
	}
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
		
		for(plot.format in c(PLOT_FORMATS, "emf"))
		{	file <- file.path(out.folder, paste0(type, "_", file.names[i], ".", plot.format))
			tlog(4, "Generating plot in file \"",file,"\"")
			if(plot.format=="pdf")
				pdf(file, width=11, height=7)
			else if(plot.format=="png")
				png(file, width=1024, height=1024)
			else if(plot.format=="emf")
				emf(file, width=11, height=7)
			
			par(mar=c(5, 4, 1.5, 0)+0.1)	# B L T R
			# create plot
			plot(x=NULL,
				xlab="Date", 
				ylab="Number of representatives",
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
				legend=c("Described in the database", "Available positions"), 
				fill=c(EVOL_COL_MEAS, EVOL_COL_LIM), 
				bg="WHITE")
			# restore options
			par(mar=c(5, 4, 4, 2)+0.1)	# B L T R
			dev.off()
		}
	}
}

# apply to all types of mandates
for(type in names(mandate.type.map))
	plot.pers.time2(data=tab.mandates, out.folder=out.folder, type=type)




###############################################################################
# assess data completeness over all tables

# list of tables
table.names <- c("Area", "Function", "Inclusion", "Individual", "Mandate", "PoliticalNuance", "Profession", "TypeFunction")

# load each one and assess completeness
for(table.name in table.names)
{	cat("Computing stats for table \"", table.name, "\"\n", sep="")
	tab.file <- file.path(in.folder, paste0(table.name, ".csv"))
	
	# read table
	tt <- read.table(
		file=tab.file, 				# name of the data file
		header=TRUE, 				# look for a header
		sep=",", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
#		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
		colClasses="character"		# all columns originally read as characters, then converted later if needed
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	
	# replace NULLs by NAs
	for(c in 1:ncol(tt))
	{	idx <- which(tt[,c]=="NULL")
		tt[idx,c] <- NA
	}
	
	# count missing values
	cn <- c("Filled", "Empty", "Total", "UniqueVals")
	stats <- matrix(NA, nrow=ncol(tt), ncol=length(cn))
	colnames(stats) <- cn
	rownames(stats) <- colnames(tt)
	for(col in colnames(tt))
	{	stats[col,"Filled"] <- length(which(!is.na(tt[,col]) & tt[,col]!=""))
		stats[col,"Empty"] <- length(which(is.na(tt[,col]) | tt[,col]==""))
		stats[col,"Total"] <- nrow(tt)
		stats[col,"UniqueVals"] <- length(unique(tt[, col]))
	}
	
	# record stats
	tab.file <- file.path(out.folder, paste0("stats_", table.name, ".csv"))
	cat("..Recording stats in file \"", tab.file, "\"\n", sep="")
	write.csv(stats, file=tab.file, row.names=TRUE)
}

# TODO plot evolution of incompleteness over (DB) time?




###############################################################################
# plots comparing raw data and BRÉF v2

# redefine colors
EVOL_COL_OLD <- rgb(255,0,0,alpha=255,max=255)		# color of the v0 measured values
EVOL_COL_MEAS <- "#13cf13"							# color of the v2 measured values
EVOL_COL_LIM <- rgb(0,0,0,alpha=150,max=255)		# color of the legal limit

# loop over mandate types
for(type in names(mandate.type.map))
{	# load v0 data
	tab.file <- file.path(out.folder, "v0", paste0(type, "_persons_by_day.txt"))
	tab0 <- read.table(
		file=tab.file,
#		fileEncoding="UTF-8",
		header=TRUE,
#		quote=TRUE,
		sep="\t"
	)
	tab0[,"Date"] <- as.Date(tab0[,"Date"], format("%d/%m/%Y"))
		
	# load v2 data
	tab.file <- file.path(out.folder, "v2", paste0(type, "_persons_by_day.txt"))
	tab2 <- read.table(
		file=tab.file,
#		fileEncoding="UTF-8",
		header=TRUE,
#		quote=TRUE,
		sep="\t"
	)
	tab2[,"Date"] <- as.Date(tab2[,"Date"], format("%d/%m/%Y"))
	
	# set up parameters to cover various time periods
	focus.date <- as.Date("2000/1/1")
	{	if(type=="DE")
			focus.date <- as.Date("1978/1/1")
		else if(type=="S")
			focus.date <- as.Date("1957/1/1")
	}
	start.dates <- c(
		min(c(tab0[,"Date"], tab2[,"Date"])),		# earliest date in the data
		as.Date("2000/1/1"),						# right before RNE creation
		focus.date									# depends on the mandate type
	)
	end.dates <- c(
		max(c(tab0[,"Date"], tab2[,"Date"])),		# latest date in the data
		as.Date("2018/7/31"),						# RNE extraction date
		as.Date("2020/5/1")							# (approximately) current date
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
		idx0 <- which(tab0[,"Date"]>=start.date & tab0[,"Date"]<=end.date)
		idx2 <- which(tab2[,"Date"]>=start.date & tab2[,"Date"]<=end.date)
		ylim <- range(c(tab0[idx0,"Count"], tab2[idx2,"Count"]))
		
		for(plot.format in c(PLOT_FORMATS, "emf"))
		{	file <- file.path(out.folder, paste0(type, "_", file.names[i], ".", plot.format))
			tlog(4, "Generating plot in file \"",file,"\"")
			if(plot.format=="pdf")
				pdf(file, width=11, height=7)
			else if(plot.format=="png")
				png(file, width=1024, height=1024)
			else if(plot.format=="emf")
				emf(file, width=11, height=7)
			
			par(mar=c(5, 4, 1.5, 0)+0.1)	# B L T R
			# create plot
			plot(x=NULL,
				xlab="Date", 
				ylab="Number of representatives",
				xaxt="n", yaxt="n",
				xlim=c(start.date, end.date),
				ylim=ylim
#				cex.names=min(1,20/length(uvals))
			)
			# setup x axis
			{	if(as.integer(end.year) - as.integer(start.year) > 20)
				unit <- "2 year"
			else 
				unit <- "year"
			}
			ticks <- seq(start.date, end.date, unit)
			axis(side=1, at=ticks, labels=get.year(ticks), las=2)
			# setup y axis
			ticks <- axTicks(2)
			axis(side=2, at=ticks, labels=format(ticks,scientific=FALSE))
			# plot election dates as vertical lines
			plot.election.dates(type,
				start.date=start.date, 
				end.date=end.date,
				max.val=ylim[2])
			# plot theoretical limit
			plot.position.limit(
				type, 
				start.date=start.date, 
				end.date=end.date)
			# plot v0 stats
			xs0 <- as.Date(tab0[idx0,"Date"], origin="1970-01-01")
			polygon(
				x=c(xs0, max(xs0), min(xs0)),
				y=c(tab0[idx0, "Count"], 0, 0),
				border=NA,
				col=make.color.transparent(color=EVOL_COL_OLD, transparency=75)
			)
			lines(
				x=xs0,
				y=tab0[idx0, "Count"], 
				col=EVOL_COL_OLD, 
				lwd=EVOL_TCK_MEAS,
				type="l"
			)
			# plot v2 stats
			xs2 <- as.Date(tab2[idx2,"Date"], origin="1970-01-01")
			polygon(
				x=c(xs2, max(xs2), min(xs2)),
				y=c(tab2[idx2, "Count"], 0, 0),
				border=NA,
				col=make.color.transparent(color=EVOL_COL_MEAS, transparency=75)
			)
			lines(
				x=xs2,
				y=tab2[idx2, "Count"], 
				col=EVOL_COL_MEAS, 
				lwd=EVOL_TCK_MEAS,
				type="l"
			)
			# add legend
#			legend(x="bottomright", 
#				legend=c("Raw data", "BRÉF data", "Legal limit"), 
#				fill=c(EVOL_COL_OLD, EVOL_COL_MEAS, EVOL_COL_LIM), 
#				bg="WHITE"
#			)
			# restore options
			par(mar=c(5, 4, 4, 2)+0.1)	# B L T R
			dev.off()
		}
	}
}	

# plot legend separately
for(plot.format in c(PLOT_FORMATS, "emf"))
{	file <- file.path(out.folder, paste0("legend.", plot.format))
	if(plot.format=="pdf")
		pdf(file, width=11, height=7)
	else if(plot.format=="png")
		png(file, width=1024, height=1024)
	else if(plot.format=="emf")
		emf(file, width=11, height=7)
	plot(NULL, xaxt="n", yaxt="n", bty="n", ylab=NA, xlab=NA, xlim=0:1, ylim=0:1)
	legend(x="center", 
		legend=c("Legal limit", "Raw data", "BRÉF data"), 
		fill=c(EVOL_COL_LIM, EVOL_COL_OLD, EVOL_COL_MEAS), 
		bg="WHITE"
	)
	dev.off()
}
