#############################################################################################
# Functions used to load each table, and apply specific corrections to them.
# 
# 09/2019 Vincent Labatut
#
# source("src/load/load_rne.R")
#############################################################################################




#############################################################################################
# Reads a standard conversion table from a text file (for the integration of secondary sources
# such as the Senate DB or National Assembly DB).
#
# file: path and name of the file containing the table.
#
# returns: the read conversion table.
#############################################################################################
load.conversion.file <- function(file)
{	tlog(6,"Loading conversion file ",file)
	result <- read.table(
		file=file,					# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
#		colClasses="character"		# all column originally read as characters, then converted later if needed
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	
	return(result)
} 




#############################################################################################
# Read a cached file corresponding to a cleaned data table.
# 
# cache.file: path of the file to load.
# 
# returns: the read data table.
#############################################################################################
read.cached.table <- function(cache.file)
{	tlog(0,"Reading cached file \"",cache.file,"\"")
	
	# first read only the column names
	tmp <- read.table(
		file=cache.file,			# name of the data file
		nrows=1,					# only read the header
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
		colClasses="character"		# force column types
	)
	
	# set up columns data types
	col.names <- colnames(tmp)
	types <- rep("character",length(col.names))
	types[which(COL_TYPES[col.names]=="dat")] <- "Date"
	types[which(COL_TYPES[col.names]=="num")] <- "integer"
	types[col.names %in% c(COL_ATT_CORREC_DATE,COL_ATT_CORREC_INFO)] <- "logical"
	
	# read the full table normally
	data <- read.table(
		file=cache.file,			# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
		colClasses=types			# force column types
	)
	tlog(2,"Read ",nrow(data)," rows and ",ncol(data)," columns")
	
	return(data)
}




#############################################################################################
# Record a table in a cache file, for later use.
#
# data: data table to record.
# cache.file: path of the file to record to.
#############################################################################################
write.cached.table <- function(data, cache.file)
{	tlog(0,"Caching data in file \"",cache.file,"\"")
	
	# possibly create folder
	folder <- dirname(cache.file)
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	write.table(x=data,				# data to record
		file=cache.file,			# name of file containing the new table
		quote=FALSE,				# put double quotes around strings
		sep="\t",					# use tabulations as separators
#		fileEncoding="UTF-8",		# character encoding
		row.names=FALSE,			# no names for rows
		col.names=TRUE				# record table headers
	)
	tlog(2,"Recording over")
}




#############################################################################################
# Loads the specified tables, binds them if there are several ones, convert certain values
# to appropriate types (dates, integers), and returns the resulting data frame.
#
# filenames: list of files to read.
# col.map: how to convert column names.
# correc.file: file containing the corrections.
# correct.data: whether or not to apply the correction on the data read.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
# type: type of mandate (CD, CM, etc.).
#
# returns: data frame made of the cleaned data contained in the files.
#############################################################################################
load.data <- function(filenames, col.map, correc.file, correct.data, election.file, series.file, type)
{	# load and normalize the data as strings
	data <- retrieve.normalize.data(filenames, col.map, correct.data)
	
	# possibly apply the corrections
	if(correct.data)
	{	# fix id duplicates and other id-related issues
		data <- fix.id.problems(data)
		# apply ad hoc corrections
		data <- apply.adhoc.corrections(data, col.map, correc.file)
		# apply systematic corrections
		data <- apply.systematic.corrections(data, type)
	}
	# ad hoc minimal corrections needed to perform later tests
	else
		data <- apply.minimal.adhoc.corrections(data, type)
	
	# convert date and numeric columns
	data <- convert.col.types(data)
	
	# add missing columns
	data <- add.missing.columns(data)
	
	# normalize columns order
	data <- normalize.col.order(data)
	
	# possibly perform additional corrections
	if(correct.data)
	{	# merge rows considered as compatible
		data <- merge.similar.rows(data)
		# fix mandate/function dates
		data <- fix.mdtfct.dates(data, election.file, series.file, type, strict=TRUE)
	}
	
	# count the number of modified rows
	idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
	tlog(0,"Total number of modified rows: ", length(idx))
	
	return(data)
}




#############################################################################################
# Loads the table for departmental counsilors (first extraction).
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_CD))
		data <- read.cached.table(cache.file=FILE_CACHE_CD)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code du département"] <- COL_ATT_DPT_CODE
		col.map["Libellé du département"] <- COL_ATT_DPT_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code du canton"] <- COL_ATT_CANT_CODE
		col.map["Libellé du canton"] <- COL_ATT_CANT_NOM
		col.map["Nuance politique (C. Gén.)"] <- COL_ATT_ELU_NUANCE
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		
		# load the data
		data <- load.data(filenames=FILES_TAB_CD, col.map=col.map, 
				correc.file=FILE_CORREC_CD, correct.data,
				election.file=FILE_VERIF_DATES_CD, series.file=FILE_VERIF_SERIES_CD,
				type="CD")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# manually constituted table
			data <- manual.integrate.data.cd(data)
			
			# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="CD", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no CD data in the assembly database
			
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_CD, series.file=FILE_VERIF_SERIES_CD,
						type="CD", strict=TRUE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# cache the data
			write.cached.table(data=data, cache.file=FILE_CACHE_CD)
		}
	
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}
#############################################################################################
# Loads the table for departmental counsilors (second extraction).
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd2.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_CD2))
		data <- read.cached.table(cache.file=FILE_CACHE_CD2)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code du département"] <- COL_ATT_DPT_CODE
		col.map["Libellé du département"] <- COL_ATT_DPT_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code du canton"] <- COL_ATT_CANT_CODE
		col.map["Libellé du canton"] <- COL_ATT_CANT_NOM
		col.map["Nuance politique (C. Gén.)"] <- COL_ATT_ELU_NUANCE
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		
		# load the data
		data <- load.data(filenames=FILES_TAB_CD2, col.map=col.map, 
				correc.file=FILE_CORREC_CD2, correct.data,
				election.file=FILE_VERIF_DATES_CD, series.file=FILE_VERIF_SERIES_CD,
				type="CD") 
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="CD", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no CD data in the assembly database
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_CD2)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the tables for municipal counsilors (first extraction).
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_CM))
		data <- read.cached.table(cache.file=FILE_CACHE_CM)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code du département (Maire)"] <- COL_ATT_DPT_CODE
		col.map["Libellé de département (Maires)"] <- COL_ATT_DPT_NOM
		col.map["Code Insee de la commune"] <- COL_ATT_COM_CODE
		col.map["Libellé de la commune"] <- COL_ATT_COM_NOM
		col.map["Population de la commune"] <- COL_ATT_COM_POP
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance politique (C. Mun.)"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_CM, col.map=col.map, 
				correc.file=FILE_CORREC_CM, correct.data,
				election.file=FILE_VERIF_DATES_CM,
				type="CM")
		
		# add mandate name
		vals <- rep("CONSEILLER MUNICIPAL",nrow(data))
		data <- cbind(data, vals)
		colnames(data)[ncol(data)] <- COL_ATT_MDT_NOM
		data <- normalize.col.order(data)
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# manually constituted table
			data <- manual.integrate.data.cm(data)
			
			# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="CM", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no CM data in the assembly database
	
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_CM,
						type="CM", strict=TRUE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_CM)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the tables for municipal counsilors (second extraction).
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm2.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_CM2))
		data <- read.cached.table(cache.file=FILE_CACHE_CM2)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Nationalité de l'élu"] <- COL_ATT_ELU_NAT
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance mandat"] <- COL_ATT_ELU_NUANCE
		col.map["Code Insee de la commune"] <- COL_ATT_COM_CODE
		col.map["Libellé de la commune"] <- COL_ATT_COM_NOM
		col.map["Code du département (Maire)"] <- COL_ATT_DPT_CODE
		col.map["Libellé de département (Maires)"] <- COL_ATT_DPT_NOM
		
		# load the data
		data <- load.data(filenames=FILES_TAB_CM2, col.map=col.map, 
				correc.file=FILE_CORREC_CM2, correct.data,
				election.file=FILE_VERIF_DATES_CM,
				type="CM")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="CM", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no CM data in the assembly database
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_CM2)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for regional counsilors (first extraction).
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_CR))
		data <- read.cached.table(cache.file=FILE_CACHE_CR)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code région"] <- COL_ATT_REG_CODE
		col.map["Libellé de la région"] <- COL_ATT_REG_NOM
		col.map["Code du département"] <- COL_ATT_DPT_CODE
		col.map["Libellé de département"] <- COL_ATT_DPT_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance mandat"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_CR, col.map=col.map, 
				correc.file=FILE_CORREC_CR, correct.data,
				election.file=FILE_VERIF_DATES_CR,
				type="CR")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# manually constituted table
			data <- manual.integrate.data.cr(data)
			
			# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="CR", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no CR data in the assembly database
			
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_CR,
						type="CR", strict=TRUE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_CR)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for regional counsilors (first extraction).
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr2.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_CR2))
		data <- read.cached.table(cache.file=FILE_CACHE_CR2)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Région"] <- COL_ATT_REG_NOM
		col.map["Circonscription électorale"] <- COL_ATT_DPT_CODE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Nationalité de l'élu"] <- COL_ATT_ELU_NAT
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Fonction pour un mandat (Code)"] <- COL_ATT_FCT_CODE
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance mandat"] <- COL_ATT_ELU_NUANCE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_CR2, col.map=col.map, 
				correc.file=FILE_CORREC_CR2, correct.data,
				election.file=FILE_VERIF_DATES_CR,
				type="CR")
		
		# split region name column
		reg.code <- sapply(data[,COL_ATT_REG_NOM], function(x) substr(x,1,2))
		reg.name <- sapply(data[,COL_ATT_REG_NOM], function(x) substr(x,3,nchar(x)))
		data[,COL_ATT_REG_NOM] <- reg.name
		data[,COL_ATT_REG_CODE] <- reg.code
		data <- normalize.col.order(data)
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="CR", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no CR data in the assembly database
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_CR2)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for members of the parliament.
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.d.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_D))
		data <- read.cached.table(cache.file=FILE_CACHE_D)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code du département"] <- COL_ATT_DPT_CODE
		col.map["Libellé du département"] <- COL_ATT_DPT_NOM
		col.map["Code de la cir.législative"] <- COL_ATT_CIRC_CODE
		col.map["Libellé de la cir.législative"] <- COL_ATT_CIRC_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance politique (Député)"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_D, col.map=col.map, 
				correc.file=FILE_CORREC_D, correct.data,
				election.file=FILE_VERIF_DATES_D,
				type="D")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# manually constituted table
			data <- manual.integrate.data.d(data)
			
			# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="D", cache=TRUE, compare=FALSE)
			
			# assembly database
			data <- assembly.integrate.data(data, cache=FALSE, compare=FALSE)
			
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_D,
						type="D", strict=FALSE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_D)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for European counsilors.
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.de.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_DE))
		data <- read.cached.table(cache.file=FILE_CACHE_DE)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["CodeCirER"] <- COL_ATT_CIRCE_CODE
		col.map["LibelléCirER"] <- COL_ATT_CIRCE_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Nuance politique (Rep. P.E.)"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_DE, col.map=col.map, 
				correc.file=FILE_CORREC_DE, correct.data,
				election.file=FILE_VERIF_DATES_DE,
				type="DE")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# manually constituted table
			data <- manual.integrate.data.de(data)
			
			# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="DE", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no DE data in the assembly database
			
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_DE,
						type="DE", strict=TRUE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_DE)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for EPCI counsilors.
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.epci.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_EPCI))
		data <- read.cached.table(cache.file=FILE_CACHE_EPCI)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code département EPCI"] <- COL_ATT_EPCI_DPT
		col.map["Code département commune rattachée"] <- COL_ATT_DPT_CODE
		col.map["Code de la commune"] <- COL_ATT_COM_CODE
		col.map["Libellé commune rattachée"] <- COL_ATT_COM_NOM
		col.map["N° SIREN"] <- COL_ATT_EPCI_SIREN
		col.map["Libellé de l'EPCI"] <- COL_ATT_EPCI_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance mandat"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_EPCI, col.map=col.map, 
				correc.file=FILE_CORREC_EPCI, correct.data,
				election.file=FILE_VERIF_DATES_CM,
				type="EPCI")
		
		# add mandate name
		vals <- rep("CONSEILLER EPCI",nrow(data))
		data <- cbind(data, vals)
		colnames(data)[ncol(data)] <- COL_ATT_MDT_NOM
		data <- normalize.col.order(data)
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# senate database
			# no EPCI data in the senate database
			
			# assembly database
			# no EPCI data in the assembly database
			
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_CM,
						type="EPCI", strict=TRUE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_EPCI)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for mayors.
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.m.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_M))
		data <- read.cached.table(cache.file=FILE_CACHE_M)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code du département (Maire)"] <- COL_ATT_DPT_CODE
		col.map["Libellé de département (Maires)"] <- COL_ATT_DPT_NOM
		col.map["Code Insee de la commune"] <- COL_ATT_COM_CODE
		col.map["Libellé de la commune"] <- COL_ATT_COM_NOM
		col.map["Population de la commune"] <- COL_ATT_COM_POP
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance politique (C. Mun.)"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_M, col.map=col.map, 
				correc.file=FILE_CORREC_M, correct.data,
				election.file=FILE_VERIF_DATES_CM,
				type="M")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# manually constituted table
			data <- manual.integrate.data.m(data)
			
			# senate database
			# NOTE Senate data eventually not used
			#data <- senate.integrate.data(data, type="M", cache=TRUE, compare=FALSE)
			
			# assembly database
			# no M data in the assembly database
	
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_CM,
						type="M", strict=TRUE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_M)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}




#############################################################################################
# Loads the table for senators.
#
# out.folder: output folder, used to record various runtime stats.
# correct.data: whether or not to apply the correction on the data read.
# complete.data: whether or not to use secondary sources to correct/complete the RNE.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.s.data <- function(out.folder, correct.data, complete.data)
{	# retrieve cached file
	if(CACHE_DATA && file.exists(FILE_CACHE_S))
		data <- read.cached.table(cache.file=FILE_CACHE_S)
	
	# cache disabled or no cached file
	else
	{	# init stats table
		init.stat.table(out.folder)
		
		# names of the columns
		col.map <- c()
		col.map["Code du département"] <- COL_ATT_DPT_CODE
		col.map["Libellé du département"] <- COL_ATT_DPT_NOM
		col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
		col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
		col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
		col.map["Code sexe"] <- COL_ATT_ELU_SEXE
		col.map["Code profession"] <- COL_ATT_PRO_CODE
		col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
		col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
		col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
		col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
		col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
		col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
		col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
		col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
		col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
		col.map["Nuance politique (Sénateur)"] <- COL_ATT_ELU_NUANCE
		col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
		
		# load the data
		data <- load.data(filenames=FILES_TAB_S, col.map=col.map, 
				correc.file=FILE_CORREC_S, correct.data,
				election.file=FILE_VERIF_DATES_S, series.file=FILE_VERIF_SERIES_S,
				type="S")
		
		# correct/complete with secondary sources
		if(complete.data)
		{	# senate database
			data <- senate.integrate.data(data, type="S", cache=FALSE, compare=TRUE)
			
			# assembly database
			# no S data in the assembly database
			
			# clean another time
			if(correct.data)
			{	data <- merge.similar.rows(data)
				data <- fix.mdtfct.dates(data, 
						election.file=FILE_VERIF_DATES_S, series.file=FILE_VERIF_SERIES_S,
						type="S", strict=FALSE)
			}
			
			# count the number of modified rows
			idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
			tlog(0,"Total number of modified rows: ", length(idx))
		}
		
		if(CACHE_DATA)
		{	# record the data
			write.cached.table(data=data, cache.file=FILE_CACHE_S)
		}
		
		# close the stat file
		finalize.stat.table()
	}
	
	return(data)
}

