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
{	tlog(0,"Trying to detect problems in date columns")
	
	# checking whether each individual date is too early or too late
	tlog(2,"Detecting early/late dates")
	for(col in cols)
	{	if(col$tp=="dat")
		{	tlog(4,"Processing column \"",col$name,"\"")
			
			# too early
			idx <- which(data[,col$name]<as.Date("1900/01/01"))
			tlog(6,"Found ",length(idx)," date(s) that seem too early")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col$basename,"_problems_too_early.txt"))
				tlog(6,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
			}
			
			# too late
			idx <- which(data[,col$name]>=as.Date("2020/01/01"))			
			tlog(6,"Found ",length(idx)," date(s) that seem too late")
			if(length(idx)>0)
			{	tmp <- cbind(idx,data[idx,])
				colnames(tmp)[1] <- "Ligne"
				tab.file <- file.path(out.folder,paste0(col$basename,"_problems_too_late.txt"))
				tlog(6,"Recording in file \"",tab.file,"\"")
				write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
			}
		}
	}
	
	# comparing mandate boundaries
	{	tlog(2,"Comparing start/end dates for mandates")
		
		# start after end, or no start at all (=NA)
		idx <- which(data[,COL_ATT_MDT_DBT]>data[,COL_ATT_MDT_FIN] | is.na(data[,COL_ATT_MDT_DBT]))
		tlog(4,"Found ",length(idx)," mandate(s) starting after they end")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_bounds.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		}
		
		# born after mandate begins
		idx <- which(data[,COL_ATT_ELU_DDN]>=data[,COL_ATT_MDT_DBT])			
		tlog(4,"Found ",length(idx)," mandate(s) starting before birthdate")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("mandat_dates_problems_birthdate.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		}
	}
	
	# comparing function boundaries
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	tlog(2,"Comparing start/end dates for functions")
		
		# start after end, or end but no start (=NA)
		idx <- which(data[,COL_ATT_FCT_DBT]>data[,COL_ATT_FCT_FIN] |			
					is.na(data[,COL_ATT_FCT_DBT]) & !is.na(data[,COL_ATT_FCT_FIN]))
		tlog(4,"Found ",length(idx)," function(s) starting after they end")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_bounds.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		}
		
		# not included in the mandate period
		idx <- which(data[,COL_ATT_FCT_DBT]<data[,COL_ATT_MDT_DBT] | data[,COL_ATT_FCT_FIN]>data[,COL_ATT_MDT_FIN])			
		tlog(4,"Found ",length(idx)," function(s) starting out of the corresponding mandate period")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_mandate.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		}
		
		# born after function begins
		idx <- which(data[,COL_ATT_ELU_DDN]>=data[,COL_ATT_FCT_DBT])
		tlog(4,"Found ",length(idx)," function(s) starting before birthdate")
		if(length(idx)>0)
		{	tmp <- cbind(idx,data[idx,])
			colnames(tmp)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0("fonction_dates_problems_birthdate.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		}
	}
	else
	{	tlog(2,"No function dates found in the table")
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the departmental tables, and record the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cd <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	
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
		write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the municipal tables, and record the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cm <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	
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
		write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the regional tables, and record the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.cr <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	
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
		write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the MP tables, and record the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.d <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	
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
		write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
}




#############################################################################################
# Performs a series of tests on date columns, for the European tables, and record the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.de <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	
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
		write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
}





#############################################################################################
# Performs a series of tests on date columns, for the senatorial tables, and record the 
# detected problems in text files.
#
# data: table containing the data.
# cols: list describing how to handle each column in the table.
# out.folder: folder where to output the results.
#############################################################################################
test.col.dates.s <- function(data, cols, out.folder)
{	# generic tests
	test.col.dates.generic(data, cols, out.folder)
	
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
		write.table(x=tmp,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
	}
}
