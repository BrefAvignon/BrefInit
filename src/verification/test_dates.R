#############################################################################################
# Functions performing various tests on date columns.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Performs a series of generic tests on date columns, and records the detected problems in 
# text files: date too early or too late compared to the studied period, starting dates later
# than ending date, person born after his mandate/function, function not included in the 
# mandate period.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.generic <- function(data, out.folder)
{	tlog(2,"Trying to detect problems in date columns")
	
	# checking whether each individual date is too early or too late
	tlog(4,"Detecting early/late dates")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		col.basename <- BASENAMES[col.name]
		if(col.type=="dat")
		{	tlog(6,"Processing column \"",col.name,"\"")
			
			# too early
			idx <- which(data[,col.name]<as.Date("1900/01/01"))
			tlog(8,"Found ",length(idx)," date(s) that seem too early")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col.basename,"_problems_too_early.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp,file=tab.file,
#						fileEncoding="UTF-8",
						row.names=FALSE,col.names=TRUE)
			}
			
			# too late
			idx <- which(data[,col.name]>=as.Date("2020/01/01"))			
			tlog(8,"Found ",length(idx)," date(s) that seem too late")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col.basename,"_problems_too_late.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp,file=tab.file,
#						fileEncoding="UTF-8",
						row.names=FALSE,col.names=TRUE)
			}
		}
	}
	
	# comparing mandate boundaries
	{	tlog(4,"Comparing start/end dates for mandates")
		
		# no mandate dates at all
		idx <- which(is.na(data[,COL_ATT_MDT_DBT]) & is.na(data[,COL_ATT_MDT_FIN]))
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_missing.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,col.names=TRUE)
		}
		
		# start after end, or no start at all (=NA)
		idx <- which(data[,COL_ATT_MDT_DBT]>data[,COL_ATT_MDT_FIN] 
						| is.na(data[,COL_ATT_MDT_DBT]) & !is.na(data[,COL_ATT_MDT_FIN]))
		tlog(6,"Found ",length(idx)," mandate(s) starting after they end")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_bounds.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,col.names=TRUE)
		}
		
		# born after mandate begins
		idx <- which(data[,COL_ATT_ELU_DDN]>=data[,COL_ATT_MDT_DBT])			
		tlog(6,"Found ",length(idx)," mandate(s) starting before birthdate")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_birthdate.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,col.names=TRUE)
		}
	}
	
	# comparing function boundaries
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	tlog(4,"Comparing start/end dates for functions")
		
		# start after end, or end but no start (=NA)
		idx <- which(data[,COL_ATT_FCT_DBT]>data[,COL_ATT_FCT_FIN] |			
					is.na(data[,COL_ATT_FCT_DBT]) & !is.na(data[,COL_ATT_FCT_FIN]))
		tlog(6,"Found ",length(idx)," function(s) starting after they end")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_bounds.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,col.names=TRUE)
		}
		
		# not included in the mandate period
		idx <- which(data[,COL_ATT_FCT_DBT]<data[,COL_ATT_MDT_DBT] | data[,COL_ATT_FCT_FIN]>data[,COL_ATT_MDT_FIN])			
		tlog(6,"Found ",length(idx)," function(s) starting out of the corresponding mandate period")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_mandate.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,col.names=TRUE)
		}
		
		# born after function begins
		idx <- which(data[,COL_ATT_ELU_DDN]>=data[,COL_ATT_FCT_DBT])
		tlog(6,"Found ",length(idx)," function(s) starting before birthdate")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_birthdate.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,col.names=TRUE)
		}
	}
	else
	{	tlog(4,"No function dates found in the table")
	}
}




#############################################################################################
# Tests whether some mandate or function started/ended before 2001, which should not happen.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.pre.rne <- function(data, out.folder)
{	tlog(2,"Identifying pre-2001 mandates and functions")
	limit <- as.Date("2001/01/01")
	
	# retrieve all rows with a date earlier than 2001
	tmp <- (!is.na(data[,COL_ATT_MDT_DBT]) & data[,COL_ATT_MDT_DBT]<limit
			| !is.na(data[,COL_ATT_MDT_FIN]) & data[,COL_ATT_MDT_FIN]<limit)
	if(COL_ATT_FCT_DBT %in% colnames(data))
		tmp <- (tmp
		| !is.na(data[,COL_ATT_FCT_DBT]) & data[,COL_ATT_FCT_DBT]<limit
		| !is.na(data[,COL_ATT_FCT_FIN]) & data[,COL_ATT_FCT_FIN]<limit)
	idx <- which(tmp)
	tlog(4,"Found ",length(idx)," date(s) earlier than 2001")
	
	if(length(idx)>0)
	{	# get the ids of the concerned persons
		ids <- sort(unique(data[idx,COL_ATT_ELU_ID]))
		tlog(4,"These dates correspond to ",length(ids)," distinct persons")
		# get all rows involving these ids
		idx <- which(data[,COL_ATT_ELU_ID] %in% ids)
		tlog(4,"These persons appear in ",length(idx)," different rows")
		# order these rows by id
		idx <- idx[order(data[idx,COL_ATT_ELU_ID])]
		tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		
		# record table
		tab.file <- file.path(out.folder,"mandatfonction_dates_problems_bef2001.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, col.names=TRUE)
	}
}




#############################################################################################
# Detects rows where the function is specified, but without any associated dates, or the mandate
# has an end date but not the function.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.nofun <- function(data, out.folder)
{	tlog(2,"Identifying functions without dates")
	
	# retrieve all rows with a function name but no start date
	idx <- which(!is.na(data[,COL_ATT_FCT_NOM])
					& is.na(data[,COL_ATT_FCT_DBT]))
	tlog(4,"Found ",length(idx)," rows with a function name but no start date")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_debut_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, col.names=TRUE)
	}
	
	# retrieve all rows with a function name and a mandate end, but no function end
	idx <- which(!is.na(data[,COL_ATT_FCT_NOM])
					& is.na(data[,COL_ATT_FCT_FIN]) & !is.na(data[,COL_ATT_MDT_FIN]))
	tlog(4,"Found ",length(idx)," rows with a function name and mandate end, but no function end")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_fin_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, col.names=TRUE)
	}
}




#############################################################################################
# Detects rows where the mandate dates do not match election dates. In other words, the dates 
# of a given period span two distinct mandates according to the official election dates.
#
# data: table containing the data.
# out.folder: folder where to output the results.
# election.file: name of the file containing the election dates.
#############################################################################################
test.col.dates.election <- function(data, out.folder, election.file)
{	tlog(2,"Identifying mandates whose bounds are incompatible with election dates")
	
	# load election dates
	tlog(4,"Loading the table containing election dates: \"",election.file,"\"")
	election.table <- read.table(
		file=election.file,		# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses=c("Date","Date")
	)
	
	# possibly complete second round dates
	tlog(4,"Complete missing dates in the table")
	idx <- which(is.na(election.table[,COL_VERIF_DATE_TOUR2]))
	if(length(idx)>0)
		election.table[idx,COL_VERIF_DATE_TOUR2] <- election.table[idx,COL_VERIF_DATE_TOUR1]
	
	# compare mandate and election dates
	tlog(4,"Check mandate dates against election dates")
	idx <- which(apply(data, 1, function(data.row)
		{	any(apply(election.table, 1, function(election.row)
				{	(data.row[COL_ATT_MDT_DBT]<election.row[COL_VERIF_DATE_TOUR1] 
						&& (is.na(data.row[COL_ATT_MDT_FIN]) || data.row[COL_ATT_MDT_FIN]>=election.row[COL_VERIF_DATE_TOUR2])) #TODO
					#date.intersect(
					#		start1=election.row[COL_VERIF_DATE_TOUR1], 
					#		end1=election.row[COL_VERIF_DATE_TOUR2], 
					#		start2=data.row[COL_ATT_MDT_DBT], 
					#		end2=data.row[COL_ATT_MDT_FIN]
					#)
				}
			))
		}))
	tlog(6,"Found ",length(idx)," rows with election-related issues")
	
	if(length(idx)>0)
	{	# build the table and write it
		tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_election.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
			row.names=FALSE, col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the departmental tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cd <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.nofun(data, out.folder)
	
	# election dates
#	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_CD)
	# TODO multiple coexisting election dates due to the existence of several distinct cohorts: 
	# performing this test requires identifying each unique position, in order to determine 
	# which cohort it belongs to. This is possible for departmental counsilors.
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
	# mandate duration not in the legal interval
	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("1985/1/1") & durations>6
					| data[,COL_ATT_MDT_DBT]>=as.Date("1985/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1992/1/1") & durations>7
					| data[,COL_ATT_MDT_DBT]>=as.Date("1992/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2001/1/1") & durations>6
					| data[,COL_ATT_MDT_DBT]>=as.Date("2001/1/1") & durations>7)
	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
	if(length(idx)>0)
	{	tmp <- cbind(idx,data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the municipal tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cm <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.nofun(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_CM)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
	# mandate duration not in the legal interval
	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
	idx <- which(durations > 6)
	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
	if(length(idx)>0)
	{	tmp <- cbind(idx,data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the regional tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cr <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.nofun(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_CR)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
	# mandate duration not in the legal interval
	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("2010/1/1") & durations>6
					| data[,COL_ATT_MDT_DBT]>=as.Date("2010/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2015/1/1") & durations>5
					| data[,COL_ATT_MDT_DBT]>=as.Date("2015/1/1") & durations>6)
	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
	if(length(idx)>0)
	{	tmp <- cbind(idx,data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the MP tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.d <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.nofun(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_D)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
	# mandate duration not in the legal interval
	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("1962/1/1") & durations>4
			| data[,COL_ATT_MDT_DBT]>=as.Date("1962/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1967/1/1") & durations>5
			| data[,COL_ATT_MDT_DBT]>=as.Date("1967/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1968/1/1") & durations>1
			| data[,COL_ATT_MDT_DBT]>=as.Date("1968/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1978/1/1") & durations>5
			| data[,COL_ATT_MDT_DBT]>=as.Date("1978/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1981/1/1") & durations>3
			| data[,COL_ATT_MDT_DBT]>=as.Date("1981/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1986/1/1") & durations>5
			| data[,COL_ATT_MDT_DBT]>=as.Date("1986/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1988/1/1") & durations>2
			| data[,COL_ATT_MDT_DBT]>=as.Date("1988/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1993/1/1") & durations>5
			| data[,COL_ATT_MDT_DBT]>=as.Date("1993/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1997/1/1") & durations>4
			| data[,COL_ATT_MDT_DBT]>=as.Date("1997/1/1") & durations>5)
	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
	if(length(idx)>0)
	{	tmp <- cbind(idx,data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the European tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.de <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_DE)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
	# mandate duration not in the legal interval
	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
	idx <- which(durations > 5)
	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
	if(length(idx)>0)
	{	tmp <- cbind(idx,data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the EPCI tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.epci <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.nofun(data, out.folder)
}




#############################################################################################
# Performs a series of tests on date columns, for the senatorial tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.s <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.nofun(data, out.folder)
	
	# election dates
#	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_S)
	# TODO multiple coexisting election dates due to the existence of several distinct cohorts: 
	# performing this test requires identifying each unique position, in order to determine 
	# which cohort it belongs to. But that is not possible for senators...
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
	# mandate duration not in the legal interval
	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("1998/1/1") & durations>9
					| data[,COL_ATT_MDT_DBT]>=as.Date("1998/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2014/1/1") & durations>10
					| data[,COL_ATT_MDT_DBT]>=as.Date("2014/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2017/1/1") & durations>9
					| data[,COL_ATT_MDT_DBT]>=as.Date("2017/1/1") & durations>6)
	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
	if(length(idx)>0)
	{	tmp <- cbind(idx,data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
}
