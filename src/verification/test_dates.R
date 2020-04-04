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
# tolerance: minimal duration of a mandate.
#############################################################################################
test.col.dates.generic <- function(data, out.folder, tolerance=7)
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
				write.table(x=tmp, file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,
					col.names=TRUE,
#					quote=TRUE,
					sep="\t"
				)
			}
			
			# too late
			idx <- which(data[,col.name]>=Sys.Date())	#as.Date("2020/01/01"))	# NOTE could alernatively be the date of extraction of the DB
			tlog(8,"Found ",length(idx)," date(s) that seem too late")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col.basename,"_problems_too_late.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp, file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,
					col.names=TRUE,
#					quote=TRUE,
					sep="\t"
				)
			}
		}
	}
	
	# comparing mandate boundaries
	{	tlog(4,"Comparing start/end dates for mandates")
		
		# no mandate dates at all			# NOTE now quite redundant with the new tests in test.col.incomplete.mandates
		idx <- which(is.na(data[,COL_ATT_MDT_DBT]) & is.na(data[,COL_ATT_MDT_FIN]))
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_missing.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp, file=tab.file,
#				fileEncoding="UTF-8",
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
			write.table(x=tmp, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		
		# born after mandate begins
		idx <- which(data[,COL_ATT_ELU_NAIS_DATE]>=data[,COL_ATT_MDT_DBT])			
		tlog(6,"Found ",length(idx)," mandate(s) starting before birthdate")
		if(length(idx)>0)
		{	tmp <- cbind(idx, data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_birthdate.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		
		# duration in number of days
		idx <- which(!is.na(data[,COL_ATT_MDT_DBT]) & !is.na(data[,COL_ATT_MDT_FIN]))
		if(length(idx)>0)
		{	# compute mandate duration
			durations <- as.integer(data[idx,COL_ATT_MDT_FIN] - data[idx,COL_ATT_MDT_DBT])
			
			# record duration distribution, for information
			val.file <- file.path(out.folder,paste0("mandat_duration_unique_vals.txt"))
			tt <- table(durations, useNA="always")
			write.table(x=tt, file=val.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=FALSE,
#				quote=TRUE,
				sep="\t"
			)
			
			# compare to tolerance
			if(!is.na(tolerance))
				idx <- idx[durations<tolerance]
			tlog(6,"Found ",length(idx)," mandate(s) which are too short (<",tolerance," days)")
			if(length(idx)>0)
			{	tmp <- cbind(idx,durations[idx],data[idx,])
				colnames(tmp)[1:2] <- c("Ligne","Duree mandat")
				tmp <- tmp[order(durations[idx]),]
				tab.file <- file.path(out.folder,paste0("mandat_dates_problems_short.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp, file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,
					col.names=TRUE,
#					quote=TRUE,
					sep="\t"
				)
			}
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
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
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
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		
		# born after function begins
		idx <- which(data[,COL_ATT_ELU_NAIS_DATE]>=data[,COL_ATT_FCT_DBT])
		tlog(6,"Found ",length(idx)," function(s) starting before birthdate")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_birthdate.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		
		# duration in number of days
		idx <- which(!is.na(data[,COL_ATT_FCT_DBT]) & !is.na(data[,COL_ATT_FCT_FIN]))
		if(length(idx)>0)
		{	durations <- as.integer(data[idx,COL_ATT_FCT_FIN] - data[idx,COL_ATT_FCT_DBT])
			if(!is.na(tolerance))
				idx <- idx[durations<tolerance]
			tlog(6,"Found ",length(idx)," function(s) which are too short (<",tolerance," days)")
			if(length(idx)>0)
			{	tmp <- cbind(idx,durations[idx],data[idx,])
				colnames(tmp)[1:2] <- c("Ligne","Duree fonction")
				tmp <- tmp[order(durations[idx]),]
				tab.file <- file.path(out.folder,paste0("fonction_dates_problems_short.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp, file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE,
					col.names=TRUE,
#					quote=TRUE,
					sep="\t"
				)
			}
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
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
		)
	}
}




#############################################################################################
# Detects rows where the function is specified, but misses certain fields, such as no start 
# date, or no name, or no end date, or no end motive.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.incomplete.funtions <- function(data, out.folder)
{	tlog(2,"Identifying rows with incomplete function info")
	
	# the mandate has ended if there is a mandate end date or end motive
	ended.mdt <- (!is.na(data[,COL_ATT_MDT_FIN]) | !is.na(data[,COL_ATT_MDT_MOTIF]))
	
	# we have a function if there is a name, start date, end date, or end motive
	exist.fct <- (!is.na(data[,COL_ATT_FCT_NOM]) | !is.na(data[,COL_ATT_FCT_MOTIF])
				| !is.na(data[,COL_ATT_FCT_DBT]) | !is.na(data[,COL_ATT_FCT_FIN]))
	# the function has ended if there is a function end date or end motive, or if the function exists and the mandate has ended
	ended.fct <- (!is.na(data[,COL_ATT_FCT_FIN]) | !is.na(data[,COL_ATT_FCT_MOTIF])
				| (exist.fct & ended.mdt))
	
	# retrieve all rows missing the function name
	idx <- which(exist.fct & is.na(data[,COL_ATT_FCT_NOM]))
	tlog(4,"Found ",length(idx)," rows where the function name is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_lib_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	# retrieve all rows missing the function start date
	idx <- which(exist.fct & is.na(data[,COL_ATT_FCT_DBT]))
	tlog(4,"Found ",length(idx)," rows where the function start date is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_debut_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	# retrieve all rows missing the function end date
	idx <- which(ended.fct & is.na(data[,COL_ATT_FCT_FIN]))
	tlog(4,"Found ",length(idx)," rows where the function end date is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_fin_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	# retrieve all rows missing the function end motive
	idx <- which(ended.fct & is.na(data[,COL_ATT_FCT_MOTIF]))
	tlog(4,"Found ",length(idx)," rows where the function end motive is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_motif_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Detects rows where the mandate misses certain fields, such as no start date, or no name, or 
# no end date, or no end motive.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.incomplete.mandates <- function(data, out.folder)
{	tlog(2,"Identifying rows with incomplete mandate info")
	
	# the mandate has ended if there is a mandate end date or end motive
	ended.mdt <- (!is.na(data[,COL_ATT_MDT_FIN]) | !is.na(data[,COL_ATT_MDT_MOTIF]))
	
	# retrieve all rows missing the mandate name
	if(COL_ATT_MDT_NOM %in% colnames(data))
	{	idx <- which(is.na(data[,COL_ATT_MDT_NOM]))
		tlog(4,"Found ",length(idx)," rows where the mandate name is missing")
		# build the table and write it
		if(length(idx)>0)
		{	tmp <- cbind(idx, data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,"mandat_lib_problems_missing.txt")
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
	}
	
	# retrieve all rows missing the mandate start date
	idx <- which(is.na(data[,COL_ATT_MDT_DBT]))
	tlog(4,"Found ",length(idx)," rows where the mandate start date is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"mandat_debut_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	# retrieve all rows missing the mandate end date
	idx <- which(ended.mdt & is.na(data[,COL_ATT_MDT_FIN]))
	tlog(4,"Found ",length(idx)," rows where the mandate end date is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"mandat_fin_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	# retrieve all rows missing the mandate end motive
	idx <- which(ended.mdt & is.na(data[,COL_ATT_MDT_MOTIF]))
	tlog(4,"Found ",length(idx)," rows where the mandate end motive is missing")
	# build the table and write it
	if(length(idx)>0)
	{	tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"mandat_motif_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Detects rows where the mandate dates do not match election dates. In other words, the dates 
# of a given period span two distinct mandates according to the official election dates.
#
# data: table containing the data.
# out.folder: folder where to output the results.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
#############################################################################################
test.col.dates.election <- function(data, out.folder, election.file, series.file)
{	tlog(2,"Identifying mandates whose bounds are incompatible with election dates")
	series.present <- hasArg(series.file)
	
	# load election-related data
	tmp <- load.election.data(data, election.file, series.file)
	election.table <- tmp$election.table
	if(series.present)
	{	series.table <- tmp$series.table
		series.list <- tmp$series.list
	}
	
	# compare mandate and election dates
	tlog(4,"Check mandate dates against election dates")
	idx <- which(sapply(1:nrow(data), function(r)
	{	tlog(6,"Processing row ",r,"/",nrow(data),": ",format.row.dates(data[r,]))
		
		# get election dates
		election.dates <- election.table
		if(series.present)
		{	# CD table
			if(COL_ATT_CANT_CODE %in% colnames(series.table))
				idx <- which(series.table[,COL_ATT_DPT_CODE]==data[r,COL_ATT_DPT_CODE]
						& series.table[,COL_ATT_CANT_NOM]==data[r,COL_ATT_CANT_NOM])
			# S table
			else 
			{	# very ugly ad hoc fix for Senators representing people leaving abroad
				if(data[r,COL_ATT_DPT_NOM]=="FRANCAIS DE L ETRANGER")
				{	series <- get.adhoc.senator.series.fix(name=data[r,COL_ATT_ELU_NOM])
				}
				# retrieve the series corresponding to the position
				else
				{	idx <- which(series.table[,COL_ATT_DPT_CODE]==data[r,COL_ATT_DPT_CODE])
					series <- series.table[idx,COL_VERIF_SERIE]
				}
			}
			# retrieve the election dates corresponding to the series
			idx <- sapply(series.list, function(s) is.na(series) || series %in% s)
			election.dates <- election.table[idx,]
		}
		
		# compare with mandate dates
		tests <- sapply(1:nrow(election.dates), function(e)
		{	(data[r,COL_ATT_MDT_DBT]<election.dates[e,COL_VERIF_DATE_TOUR1] 
				&& (is.na(data[r,COL_ATT_MDT_FIN]) 
					|| data[r,COL_ATT_MDT_FIN]>=election.dates[e,COL_VERIF_DATE_TOUR2]))
			#date.intersect(
			#		start1=election.dates[e,COL_VERIF_DATE_TOUR1], 
			#		end1=election.dates[e,COL_VERIF_DATE_TOUR2], 
			#		start2=data.row[COL_ATT_MDT_DBT], 
			#		end2=data.row[COL_ATT_MDT_FIN]
			#)
		})
		res <- any(tests)
		if(res)
		{	
#			tlog(8,paste(data[r,],colapse=","))
#			print(data[r,])
#			print(cbind(election.dates,tests))
			idx.tests <- which(tests)
			if(length(idx.tests)>1)
# TODO			stop("Problem: several rows match")
				idx.tests <- idx.tests[1]
#			else
#			{	tlog(8,format.row.dates(data[r,]), " vs. ", format(election.dates[idx.tests,1]), "--", format(election.dates[idx.tests,2]))
#				readline() #stop()
#			}
		}
		
		# ignored case
		else
			res <- FALSE
		
		return(res)
	}))
	tlog(6,"Found ",length(idx)," rows with election-related issues")
	
	if(length(idx)>0)
	{	# build the table and write it
		tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_election.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_CD, series.file=FILE_VERIF_SERIES_CD)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
# NOTE this is subsumed by the tests on election dates, no use anymore
#	# mandate duration not in the legal interval
#	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
#	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("1985/1/1") & durations>6
#					| data[,COL_ATT_MDT_DBT]>=as.Date("1985/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1992/1/1") & durations>7
#					| data[,COL_ATT_MDT_DBT]>=as.Date("1992/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2001/1/1") & durations>6
#					| data[,COL_ATT_MDT_DBT]>=as.Date("2001/1/1") & durations>7)
#	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
#	if(length(idx)>0)
#	{	tmp <- cbind(idx,data[idx,])
#		colnames(tmp)[1] <- "Ligne"
#		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
#		tlog(6,"Recording in file \"",tab.file,"\"")
#		write.table(x=tmp,file=tab.file,
##				fileEncoding="UTF-8",
#				row.names=FALSE,
#				col.names=TRUE,
##				quote=TRUE,
#				sep="\t"
#		)
#	}
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_CM)
	
# NOTE this is subsumed by the tests on election dates, no use anymore
#	# specific tests
#	tlog(2,"Checking mandate durations")
#	
#	# mandate duration not in the legal interval
#	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
#	idx <- which(durations > 6)
#	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
#	if(length(idx)>0)
#	{	tmp <- cbind(idx,data[idx,])
#		colnames(tmp)[1] <- "Ligne"
#		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
#		tlog(6,"Recording in file \"",tab.file,"\"")
#		write.table(x=tmp,file=tab.file,
##				fileEncoding="UTF-8",
#				row.names=FALSE,
#				col.names=TRUE,
##				quote=TRUE,
#				sep="\t"
#		)
#	}
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_CR)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
# NOTE this is subsumed by the tests on election dates, no use anymore
#	# mandate duration not in the legal interval
#	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
#	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("2010/1/1") & durations>6
#					| data[,COL_ATT_MDT_DBT]>=as.Date("2010/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2015/1/1") & durations>5
#					| data[,COL_ATT_MDT_DBT]>=as.Date("2015/1/1") & durations>6)
#	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
#	if(length(idx)>0)
#	{	tmp <- cbind(idx,data[idx,])
#		colnames(tmp)[1] <- "Ligne"
#		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
#		tlog(6,"Recording in file \"",tab.file,"\"")
#		write.table(x=tmp,file=tab.file,
##				fileEncoding="UTF-8",
#				row.names=FALSE,
#				col.names=TRUE,
##				quote=TRUE,
#				sep="\t"
#		)
#	}
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_D)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
# NOTE this is subsumed by the tests on election dates, no use anymore
#	# mandate duration not in the legal interval
#	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
#	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("1962/1/1") & durations>4
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1962/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1967/1/1") & durations>5
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1967/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1968/1/1") & durations>1
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1968/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1978/1/1") & durations>5
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1978/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1981/1/1") & durations>3
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1981/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1986/1/1") & durations>5
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1986/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1988/1/1") & durations>2
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1988/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1993/1/1") & durations>5
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1993/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("1997/1/1") & durations>4
#			| data[,COL_ATT_MDT_DBT]>=as.Date("1997/1/1") & durations>5)
#	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
#	if(length(idx)>0)
#	{	tmp <- cbind(idx,data[idx,])
#		colnames(tmp)[1] <- "Ligne"
#		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
#		tlog(6,"Recording in file \"",tab.file,"\"")
#		write.table(x=tmp,file=tab.file,
##				fileEncoding="UTF-8",
#				row.names=FALSE,
#				col.names=TRUE,
##				quote=TRUE,
#				sep="\t"
#		)
#	}
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	#test.col.incomplete.funtions(data, out.folder)	# no functions described in DE
	
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_DE)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
# NOTE this is subsumed by the tests on election dates, no use anymore
#	# mandate duration not in the legal interval
#	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
#	idx <- which(durations > 5)
#	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
#	if(length(idx)>0)
#	{	tmp <- cbind(idx,data[idx,])
#		colnames(tmp)[1] <- "Ligne"
#		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
#		tlog(6,"Recording in file \"",tab.file,"\"")
#		write.table(x=tmp,file=tab.file,
##				fileEncoding="UTF-8",
#				row.names=FALSE,
#				col.names=TRUE,
##				quote=TRUE,
#				sep="\t"
#		)
#	}
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
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
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
		
	# election dates
	test.col.dates.election(data, out.folder, election.file=FILE_VERIF_DATES_S, series.file=FILE_VERIF_SERIES_S)
	
	# specific tests
	tlog(2,"Checking mandate durations")
	
# NOTE this is subsumed by the tests on election dates, no use anymore
#	# mandate duration not in the legal interval
#	durations <- as.numeric(difftime(data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], unit="weeks"))/52.25
#	idx <- which(data[,COL_ATT_MDT_DBT]<as.Date("1998/1/1") & durations>9
#					| data[,COL_ATT_MDT_DBT]>=as.Date("1998/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2014/1/1") & durations>10
#					| data[,COL_ATT_MDT_DBT]>=as.Date("2014/1/1") & data[,COL_ATT_MDT_FIN]<as.Date("2017/1/1") & durations>9
#					| data[,COL_ATT_MDT_DBT]>=as.Date("2017/1/1") & durations>6)
#	tlog(4,"Found ",length(idx)," mandate(s) longer than expected")
#	if(length(idx)>0)
#	{	tmp <- cbind(idx,data[idx,])
#		colnames(tmp)[1] <- "Ligne"
#		tab.file <- file.path(out.folder,paste0("mandat_dates_problems_duration.txt"))
#		tlog(6,"Recording in file \"",tab.file,"\"")
#		write.table(x=tmp,file=tab.file,
##				fileEncoding="UTF-8",
#				row.names=FALSE,
#				col.names=TRUE,
##				quote=TRUE,
#				sep="\t"
#		)
#	}
}




#############################################################################################
# Performs a series of tests on date columns, for the the merged table, and records the 
# detected problems in text files.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.all <- function(data, out.folder)
{	# generic tests
	test.col.dates.generic(data, out.folder, tolerance=7)
	test.col.dates.pre.rne(data, out.folder)
	test.col.incomplete.mandates(data, out.folder)
	test.col.incomplete.funtions(data, out.folder)
}
