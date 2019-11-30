#############################################################################################
# Functions performing various tests on date columns.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Performs a series of generic tests on date columns, and record the detected problems in 
# text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.generic <- function(data, cols, out.folder)
{	tlog(2,"Trying to detect problems in date columns")
	
	# checking whether each individual date is too early or too late
	tlog(4,"Detecting early/late dates")
	for(col in cols)
	{	if(col$tp=="dat")
		{	tlog(6,"Processing column \"",col$name,"\"")
			
			# too early
			idx <- which(data[,col$name]<as.Date("1900/01/01"))
			tlog(8,"Found ",length(idx)," date(s) that seem too early")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col$basename,"_problems_too_early.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp,file=tab.file,
#						fileEncoding="UTF-8",
						row.names=FALSE,col.names=TRUE)
			}
			
			# too late
			idx <- which(data[,col$name]>=as.Date("2020/01/01"))			
			tlog(8,"Found ",length(idx)," date(s) that seem too late")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col$basename,"_problems_too_late.txt"))
				tlog(8,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp,file=tab.file,
#						fileEncoding="UTF-8",
						row.names=FALSE,col.names=TRUE)
			}
		}
	}
	
	# comparing mandate boundaries
	{	tlog(4,"Comparing start/end dates for mandates")
		
		# start after end, or no start at all (=NA)
		idx <- which(data[,COL_ATT_MDT_DBT]>data[,COL_ATT_MDT_FIN] | is.na(data[,COL_ATT_MDT_DBT]))
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
# Detects rows where the function is specified, but without any associated dates.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.absence <- function(data, out.folder)
{	tlog(2,"Identifying functions without dates")
	
	# retrieve all rows with a non-NA function and no dates
	idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) 
				& is.na(data[,COL_ATT_FCT_DBT]) & is.na(data[,COL_ATT_FCT_FIN]))
	tlog(4,"Found ",length(idx)," functions with missing dates")
	
	if(length(idx)>0)
	{	# build the table and write it
		tmp <- cbind(idx, data[idx,])
		colnames(tmp)[1] <- "Ligne"
		tab.file <- file.path(out.folder,"fonction_dates_problems_missing.txt")
		tlog(4,"Recording in file \"",tab.file,"\"")
		write.table(x=tmp,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, col.names=TRUE)
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the departmental tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cd <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.absence(data, out.folder)
		
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
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cm <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.absence(data, out.folder)
	
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
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cr <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.absence(data, out.folder)
	
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
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.d <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.absence(data, out.folder)
	
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
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.de <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	
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
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.epci <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.absence(data, out.folder)
}





#############################################################################################
# Performs a series of tests on date columns, for the senatorial tables, and records the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.s <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	test.col.dates.pre.rne(data, out.folder)
	test.col.dates.absence(data, out.folder)
	
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
