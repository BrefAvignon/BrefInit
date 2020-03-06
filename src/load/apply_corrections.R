#############################################################################################
# Functions used to correct the RNE data.
# 
# 03/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Loads the raw RNE data as string, normalizes and performs basic cleaning, then returns a proper
# data frame. The column names are also normalized.
#
# filenames: list of files to read.
# col.map: how to convert column names.
#
# returns: data frame containing only (clean) strings.
#############################################################################################
retrieve.normalize.data <- function(filenames, col.map)
{	# load the data table(s)
	data <- NULL
	for(filename in filenames)
	{	tlog(0,"Loading table file \"",filename,"\"")
		# read the partial table
		temp <- read.table(
			file=filename, 				# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			skip=1,						# ignore the first line of the file ("Titre du rapport")
			as.is=TRUE,					# don't convert strings to factors
			colClasses="character"		# all column originally read as characters, then converted later if needed
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		tlog(2,"Read ",nrow(temp)," rows and ",ncol(temp)," columns")
		
		# add to the main table
		tlog(0,"Adding to main table")
		if(all(is.null(data)))
			data <- temp
		else
			data <- rbind(data,temp)
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	tlog(2,"Columns: ",paste(colnames(data),collapse=","))
	
	# normalize data table column names
	norm.names <- col.map[colnames(data)]
	if(any(is.na(norm.names)) || length(norm.names)!=ncol(data))
		stop("Problem with the number of columns (or their names) when loading the table, after normalization")
	else
		colnames(data) <- norm.names
	
	# setting appropriate encoding of string columns, replace "" by NAs, and normalize proper nouns
	tlog(0,"Cleaning/encoding/normalizing strings")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		
		# the column is an actual string
		if(col.type %in% c("cat","nom"))
		{	tlog(2,"Processing column \"",col.name,"\" (",c,"/",ncol(data),")")
			# convert encoding
			##			data[,c] <- iconv(x=data[,c], from="Latin1", to="UTF8")
#			data[,c] <- iconv(x=data[,c], to="UTF8")
			
			# remove diacritics
			data[,c] <- remove.diacritics(data[,c])
			
			# normalize proper nouns
			if(colnames(data)[c] %in% COLS_ATT_PROPER_NOUNS)
				data[,c] <- normalize.proper.nouns(data[,c])
			if(colnames(data)[c] %in% COLS_ATT_LOCATION_NOUNS)
				data[,c] <- normalize.location.nouns(data[,c])
		}
		
		# the column is not a string
		else
			tlog(2,"Col. \"",colnames(data)[c],"\": not a string")
		
		# trim leading/ending whitespace
		data[,c] <- trimws(data[,c])
		
		# replace empty cells by explicit NAs
		data[which(data[,c]==""),c] <- NA
		
		# replace "NA"s by actual NAs
		data[which(data[,c]=="NA"),c] <- NA	
	}
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	
	return(data)
}




#############################################################################################
# Applies various ad hoc corrections to the raw RNE data.
#
# data: raw data, before fixing the ids.
#
# returns: table resulting from the id corrections.
#############################################################################################
fix.id.problems <- function(data)
{	# load table of equivalent ids
	tlog(0,"Loading the table of equivalent ids (",FILE_CONV_IDS,")")
	equiv.table <- read.table(
		file=FILE_CONV_IDS,			# name of the equivalence file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
		colClasses="character"		# all column originally read as characters
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	
	# fix dupplicate ids
	if(nrow(equiv.table)>0)
	{	# convert to map
		tlog(0,"Convert table of equivalent ids to map")
		unique.ids <- unique(data[,COL_ATT_ELU_ID_RNE])
		conv.map <- c()
		for(r in 1:nrow(equiv.table))
		{	main.id <- equiv.table[r,1]
			other.ids <- strsplit(x=equiv.table[r,2], split=",", fixed=TRUE)[[1]]
			other.ids <- intersect(other.ids,unique.ids)
			if(length(other.ids)>0)
				conv.map[other.ids] <- rep(main.id, length(other.ids))
		}
		if(length(conv.map)>0)
		{	# substitute correct ids
			tlog(0,"Fixing duplicate ids")
			data[,COL_ATT_ELU_ID_RNE] <- future_sapply(data[,COL_ATT_ELU_ID_RNE], function(id)
					{	new.id <- conv.map[id]
						if(is.na(new.id))
							return(id)
						else
							return(new.id)
					})
		}
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	return(data)
}




#############################################################################################
# Loads the correction table, a file containing a set of manually defined corrections to apply
# to the raw RNE data right after loading.
#
# col.map: how to convert column names.
# correc.file: file containing the corrections.
#
# returns: data frame made of the content of the correction file.
#############################################################################################
load.correction.table <- function(col.map, correc.file)
{	# load the corrections
	tlog(0,"Loading correction file \"",correc.file,"\"")
	correc.table <- read.table(
		file=correc.file,			# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
		colClasses="character"		# all column originally read as characters, then converted later if needed
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	tlog(2,"Read ",nrow(correc.table)," rows and ",ncol(correc.table)," columns")
	
	# remove commented rows
	idx <- which(startsWith(correc.table[,1],"#"))
	if(length(idx)>0)
		correc.table <- correc.table[-idx,]
	tlog(2,"Kept ",nrow(correc.table)," rows")
	
	# normalize the correction table
	if(nrow(correc.table)>0)
	{	for(r in 1:nrow(correc.table))
		{	# normalize name of corrected column
			att.name <- correc.table[r,COL_CORREC_ATTR]
			norm.name <- col.map[att.name]
			if(!is.na(norm.name))
				correc.table[r,COL_CORREC_ATTR] <- norm.name
			
			# remove diacritics
			correc.table[r,colnames(correc.table)!=COL_CORREC_COMM] <- remove.diacritics(correc.table[r,colnames(correc.table)!=COL_CORREC_COMM])
			
			# normalize proper nouns
			correc.table[r,c(COL_CORREC_NOM,COL_CORREC_PRENOM)] <- normalize.proper.nouns(correc.table[r,c(COL_CORREC_NOM,COL_CORREC_PRENOM)])
			if(correc.table[r,COL_CORREC_ATTR] %in% COLS_ATT_PROPER_NOUNS)
				correc.table[r,c(COL_CORREC_VALAVT,COL_CORREC_VALAPR)] <- normalize.proper.nouns(correc.table[r,c(COL_CORREC_VALAVT,COL_CORREC_VALAPR)])
			if(correc.table[r,COL_CORREC_ATTR] %in% COLS_ATT_LOCATION_NOUNS)
				correc.table[r,c(COL_CORREC_VALAVT,COL_CORREC_VALAPR)] <- normalize.location.nouns(correc.table[r,c(COL_CORREC_VALAVT,COL_CORREC_VALAPR)])
			
			# trim ending/starting whitespace
			correc.table[r,] <- trimws(correc.table[r,])
			
			# replace "NA"s by actual NAs
			correc.table[r,which(correc.table[r,]=="NA")] <- NA	
		}
	}
	
	return(correc.table)
}




#############################################################################################
# Applies various ad hoc corrections to the raw RNE data.
#
# data: raw data, before applying the corrections.
# col.map: how to convert column names.
# correc.file: file containing the corrections.
#
# returns: data frame after the corrections.
#############################################################################################
apply.adhoc.corrections <- function(data, col.map, correc.file)
{	# load the correction table
	correc.table <- load.correction.table(col.map, correc.file)
	
	# apply ad hoc corrections
	if(nrow(correc.table)>0)	
	{	tlog(0,"Applying ad hoc corrections")
		
		# apply each correction one after the other
		idx.rm <- c()
		for(r in 1:nrow(correc.table))
		{	correc.attr <- correc.table[r,COL_CORREC_ATTR]
			row <- as.integer(correc.table[r,COL_CORREC_ROW])
			
			# general correction
			if(all(is.na(correc.table[r,c(COL_CORREC_ID,COL_CORREC_NOM,COL_CORREC_PRENOM)])))
			{	# identify the targeted rows in the data table
				idx <- which(data[,correc.table[r,COL_CORREC_ATTR]]==correc.table[r,COL_CORREC_VALAVT]
								| is.na(data[,correc.table[r,COL_CORREC_ATTR]]) & is.na(correc.table[r,COL_CORREC_VALAVT]))
				
				# there should be at least one
				if(length(idx)<1)
				{	tlog(4,"Could not find a correction: ",paste(correc.table[r,],collapse=";"))
					stop(paste0("Could not find a correction: ",paste(correc.table[r,],collapse=";")))
				}
				# if several, try to check the specified row
				else 
				{	if(!is.na(row))
					{	if(length(idx)==1 && idx!=row)
						{	tlog(4,"Row ",idx," matches the criteria but not the specified row (",row,")")
							stop(paste0("Row ",idx," matches the criteria but not the specified row (",row,")"))
						}
						else if(length(idx)>1)
						{	tlog(4,"Found several rows matching the criteria when there's only one specified row: ",row," vs. ",paste(idx,collapse=","))
							stop(paste0("Found several rows matching the criteria when there's only one specified row: ",row," vs. ",paste(idx,collapse=",")))
						}
					}
					tlog(4,"Replacing ",correc.table[r,COL_CORREC_VALAVT]," by ",correc.table[r,COL_CORREC_VALAPR])
					data[idx,correc.table[r,COL_CORREC_ATTR]] <- correc.table[r,COL_CORREC_VALAPR]
				}
			}
			
			# correction of a specific row
			else
			{	# identify the targeted row in the data table
				idx <- which(data[,COL_ATT_ELU_ID_RNE]==correc.table[r,COL_CORREC_ID]
							& data[,COL_ATT_ELU_NOM]==correc.table[r,COL_CORREC_NOM]
							& data[,COL_ATT_ELU_PRENOM]==correc.table[r,COL_CORREC_PRENOM]
							& (data[,correc.table[r,COL_CORREC_ATTR]]==correc.table[r,COL_CORREC_VALAVT]
								| is.na(data[,correc.table[r,COL_CORREC_ATTR]]) & is.na(correc.table[r,COL_CORREC_VALAVT]))
				)
				
				# there should be exactly one
				if(length(idx)<1)
				{	tlog(4,"Could not find a correction: ",paste(correc.table[r,],collapse=";"))
					stop(paste0("Could not find a correction: ",paste(correc.table[r,],collapse=";")))
				}
				else if(length(idx)>1)
				{	if(is.na(row))
					{	tlog(4,"A correction matches several cases (",paste(idx,collapse=","),"), but no row is specified: ",paste(correc.table[r,],collapse=";"))
						stop(paste0("A correction matches several cases (",paste(idx,collapse=","),"), but no row is specified: ",paste(correc.table[r,],collapse=";")))
					}
#						tlog(4,"WARNING: A correction matches several cases: ",paste(correc.table[r,],collapse=";"))
#						data[idx,correc.table[r,COL_CORREC_ATTR]] <- correc.table[r,COL_CORREC_VALAPR]
					else
					{	if(row %in% idx)
						{	if(correc.table[r,COL_CORREC_ATTR]==COL_ATT_ELU_ID_RNE && is.na(correc.table[r,COL_CORREC_VALAPR]))
							{	tlog(4, "Row ",row," marked for removal")
								idx.rm <- c(idx.rm, row)
							}
							else
							{	tlog(4,"Correcting entry: ",paste(correc.table[r,],collapse=";"))
								data[row,correc.table[r,COL_CORREC_ATTR]] <- correc.table[r,COL_CORREC_VALAPR]
							}
						}
					}
				}
				else
				{	if(!is.na(row) && row!=idx)
					{	tlog(4,"The specified row (",row,") does not correspond to the one matching the criteria (",idx,")")
						stop(paste0("The specified row (",row,") does not correspond to the one matching the criteria (",idx,")"))
					}
					else
					{	if(correc.table[r,COL_CORREC_ATTR]==COL_ATT_ELU_ID_RNE && is.na(correc.table[r,COL_CORREC_VALAPR]))
						{	tlog(4, "Row ",idx," marked for removal")
							idx.rm <- c(idx.rm, idx)
						}
						else
						{	if(is.na(row))
								tlog(4,"Correcting entry (",idx,"): ",paste(correc.table[r,],collapse=";"))
							else
								tlog(4,"Correcting entry: ",paste(correc.table[r,],collapse=";"))
							data[idx,correc.table[r,COL_CORREC_ATTR]] <- correc.table[r,COL_CORREC_VALAPR]
						}
					}
				}
			}
		}
		
		# remove the marked rows
		if(length(idx.rm)>0)
		{	tlog(2,"Removing ",length(idx.rm)," from the table")
			data <- data[-idx.rm,]
		}
		
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	return(data)
}




#############################################################################################
# Applies various systematic corrections to the raw RNE data.
#
# data: raw data, before applying the corrections.
#
# returns: data frame after the corrections.
#############################################################################################
apply.systematic.corrections <- function(data)
{	# possibly normalize municipality ids
	if(COL_ATT_COM_CODE %in% colnames(data))
	{	tlog(0,"Normalizing municipality ids")
		# we simply keep the first three characters of the id
		data[,COL_ATT_COM_CODE] <- substr(x=data[,COL_ATT_COM_CODE], start=1, stop=3)
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	# replace the NC political nuance by proper NAs
	if(COL_ATT_ELU_NUANCE %in% colnames(data))
	{	tlog(0,"Normalizing political nuance labels")
		idx <- which(data[,COL_ATT_ELU_NUANCE]=="NC")
		if(length(idx)>0)
			data[idx,COL_ATT_ELU_NUANCE] <- NA
		tlog(2,"Fixed ",length(idx)," rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	# remove rows without mandate dates and without function dates
	tlog(0,"Removing rows with no mandate and no function date")
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	idx <- which(is.na(data[,COL_ATT_MDT_DBT]) & is.na(data[,COL_ATT_MDT_FIN]) 
						& is.na(data[,COL_ATT_FCT_DBT]) & is.na(data[,COL_ATT_FCT_FIN]))
		if(length(idx)>0)
			data <- data[-idx, ]
		tlog(2,"Removed ",length(idx)," incomplete rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	# use mandate start date when function start date is missing 
	if(COL_ATT_FCT_NOM %in% colnames(data))
	{	tlog(0,"Completing missing function start dates using mandate start dates")
		idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) & is.na(data[,COL_ATT_FCT_DBT]))
		if(length(idx)>0)
			data[idx,COL_ATT_FCT_DBT] <- data[idx,COL_ATT_MDT_DBT]
		tlog(2,"Fixed ",length(idx)," rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	# use mandate end date when function end date is missing 
	if(COL_ATT_FCT_NOM %in% colnames(data))
	{	tlog(0,"Completing missing function end dates using mandate end dates")
		idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) & is.na(data[,COL_ATT_FCT_FIN]) & !is.na(data[,COL_ATT_MDT_FIN]))
		if(length(idx)>0)
			data[idx,COL_ATT_FCT_FIN] <- data[idx,COL_ATT_MDT_FIN]
		tlog(2,"Fixed ",length(idx)," rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	}
	
	return(data)
}




#############################################################################################
# Convert the content of certain columns to the appropriate type: date or numbers.
#
# data: raw data, before performing the type conversion.
#
# returns: same data frame, but with converted columns.
#############################################################################################
convert.col.types <- function(data)
{	tlog(0,"Converting date and numeric columns")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		
		# dealing with dates
		if(col.type=="dat")
		{	tlog(2,"Col. \"",col.name,"\": converting to date")
			vals <- as.Date(data[,col.name], "%d/%m/%Y")
			
			#format(x, format="%Y/%m/%d")
			if(c==1)
				data <- cbind(vals,data[,2:ncol(data),drop=FALSE])
			else if(c<ncol(data))
				data <- cbind(data[,1:(c-1),drop=FALSE],vals,data[,(c+1):ncol(data),drop=FALSE])
			else
				data <- cbind(data[,1:(c-1),drop=FALSE],vals)
			colnames(data)[c] <- col.name
		}
		
		# dealing with numbers
# actually, this is done later if needed 
# (as there's only one such column which requires more specific processing)
#		else if(col$tp=="num")
#		{	tlog(2,"Col. \"",col$name,"\": converting to numbers")
#			vals <- suppressWarnings(as.numeric(data[,col$name]))
#			if(c==1)
#				data <- cbind(vals,data[,2:ncol(data),drop=FALSE])
#			else if(c<ncol(data))
#				data <- cbind(data[,1:(c-1),drop=FALSE],vals,data[,(c+1):ncol(data),drop=FALSE])
#			else
#				data <- cbind(data[,1:(c-1),drop=FALSE],vals)
#			colnames(data)[c] <- col.name
#		
#		}
		
		# the other columns stay strings
		else
			tlog(2,"Col. \"",col.name,"\": simple string, no conversion")
	}
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	
	
	# convert population numbers to actual integers
	if(COL_ATT_COM_POP %in% colnames(data))
	{	tlog(0,"Converting population to integer values")
		vals <- data[,COL_ATT_COM_POP]
		vals <- gsub(" ", "",  vals)	# \\s matches all whitespaces
		vals <- gsub(",00", "",  vals)
#		vals <- suppressWarnings(as.integer(vals))
		vals <- as.integer(vals)
		data <- data[, names(data)!=COL_ATT_COM_POP]
		data <- cbind(data,vals)
		names(data)[ncol(data)] <- COL_ATT_COM_POP
	}
	
	return(data)
}




#############################################################################################
# Adds missing column to the normalized table.
#
# data: RNE table missing columns.
#
# returns: same table with the extra columns.
#############################################################################################
add.missing.columns <- function(data)
{	# add data source column
	src.col <- data.frame(rep("RNE",nrow(data)), stringsAsFactors=FALSE)
	data <- cbind(data, src.col)
	colnames(data)[ncol(data)] <- COL_ATT_SOURCES
	
	# add universal id column
	ids.col <- data.frame(paste("RNE",sprintf("%07d", as.integer(data[,COL_ATT_ELU_ID_RNE])),sep="_"), stringsAsFactors=FALSE)
	data <- cbind(data, ids.col)
	colnames(data)[ncol(data)] <- COL_ATT_ELU_ID
	
	# possibly add unique department id column
	if(COL_ATT_DPT_NOM %in% colnames(data))
	{	# load table of department ids
		tlog(0,"Loading the table of department ids (",FILE_CONV_DPT,")")
		dpt.table <- read.table(
			file=FILE_CONV_DPT,			# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE					# don't convert strings to factors
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		
		# get the appropriate unique ids
		dpt.idx <- match(data[,COL_ATT_DPT_NOM], dpt.table[,COL_ATT_DPT_NOM])
		
		# insert in the table
		dpt.ids <- data.frame(dpt.table[dpt.idx, COL_ATT_DPT_ID], stringsAsFactors=FALSE)
		colnames(dpt.ids) <- COL_ATT_DPT_ID
		data <- cbind(data, dpt.ids)
	}
	
	# possibly add unique canton id column		
	if(COL_ATT_CANT_NOM %in% colnames(data))
	{	# load table of canton ids
		tlog(0,"Loading the table of canton ids (",FILE_CONV_CANTONS,")")
		equiv.table <- read.table(
			file=FILE_CONV_CANTONS,		# name of the id file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE,					# don't convert strings to factors
			colClasses="character"		# all column originally read as characters
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		
		# get the appropriate unique ids
		data.code <- paste(data[,COL_ATT_DPT_CODE],data[,COL_ATT_CANT_NOM],sep=":")
		conv.code <- paste(equiv.table[,COL_ATT_DPT_CODE],equiv.table[,COL_ATT_CANT_NOM],sep=":")
		idx <- match(data.code, conv.code)
		if(any(is.na(idx)))
		{	idx0 <- which(is.na(idx))
			print(cbind(data.code[idx0]))
			stop("Problem when inserting canton ids: cound not find some cantons")
		}
		
		# insert in the table, at the correct location
		cant.ids <- as.data.frame(x=equiv.table[idx,COL_ATT_CANT_ID], stringsAsFactors=FALSE)
		colnames(cant.ids) <- COL_ATT_CANT_ID
		data <- cbind(data, cant.ids)
	}
	
	return(data)
}




#############################################################################################
# Detect the rows that are considered as similar in the table, and merge them. Generally 
# speaking, two rows are similar if they have the same values for certain columns of interest,
# and compatible values for the rest (i.e. one cell empty in a row whereas it has a proper
# value in the other). The merging is performed by keeping the most complete row and setting
# its missing values with those found in the other row.
#
# data: the data table.
#
# returns: the table with the merged similar rows.
#############################################################################################
merge.similar.rows <- function(data)
{	comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_NAIS_DATE)
	tlog(0,"Merging compatible rows for compulsory columns \"",paste(comp.cols,collapse="\",\""),"\"")
	rm.col <- which(colnames(data) %in% comp.cols)
	
	# identify redundant rows
	concat <- apply(data[,comp.cols], 1, function(row) paste(row, collapse=":"))
	tt <- table(concat)
	codes <- names(tt)[which(tt>1)]
	tlog(2,"Looking for possibly redundant rows: found ",length(codes))
	
	# identify compatible rows amongst redundant ones
	tlog(2,"Looking for compatible rows among them")
	tmp <- lapply(codes, function(code)
			{	res <- c()
				rs <- which(concat==code)
				while(length(rs)>=2)
				{	r1 <- rs[1]
					rs <- rs[-1]
					r2 <- 1
					while(r2<=length(rs))
					{	if(all(is.na(data[r1,-rm.col]) | is.na(data[rs[r2],-rm.col]) | data[r1,-rm.col]==data[rs[r2],-rm.col]))
						{	idx <- which(is.na(data[r1,]))
							if(length(idx)>0)
								data[r1,idx] <<- data[rs[r2],idx]
							res <- c(res, rs[r2])
							rs <- rs[-r2]
						}
						else
							r2 <- r2 + 1
					}
				}
				return(res)
			})
	
	# actually remove deleted rows
	removed.nbr <- 0
	if(length(tmp)>0)
	{	idx <- unlist(tmp)
		if(length(idx)>0)
		{	removed.nbr <- length(idx)
			data <- data[-idx,]
		}
	}
	
	tlog(2,"Done merging compatible rows, removed ",removed.nbr," rows")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	return(data)
}




#############################################################################################
# Changes the order of the columns of the specified table, in order to match a predefined order
# and ease table comparison.
#
# data: the data table.
#
# returns: same table, but with reordered columns.
#############################################################################################
normalize.col.order <- function(data)
{	tlog(0,"Normalizing column order")
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when loading the table, after reordering")
	else
		data <- data[,norm.cols]
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	
	return(data)
}




#############################################################################################
# Changes the start/end of functions so that they are contained in the corresponding mandate
# dates. If the function start/end is too early or too late, it is set to the mandate start/end
# date.
#
# data: original table.
#
# return: same table, with function dates adjusted.
#############################################################################################
adjust.function.dates <- function(data)
{	tlog(0,"Adjusting function dates to be contained into mandate periods")
	nbr.corr <- 0
	
	# process each row
	for(r in 1:nrow(data))
	{	str <- paste0("Considering (",r,"/",nrow(data),") ",
				format(data[r,COL_ATT_MDT_DBT]),"--", format(data[r,COL_ATT_MDT_FIN]), " vs. ",
				format(data[r,COL_ATT_FCT_DBT]), "--", format(data[r,COL_ATT_FCT_FIN]))
		changed.start <- FALSE
		changed.end <- FALSE
		
		# problem with the start date
		if(!is.na(data[r,COL_ATT_FCT_DBT]) && !is.na(data[r,COL_ATT_MDT_DBT]) 
				&& data[r,COL_ATT_FCT_DBT]<data[r,COL_ATT_MDT_DBT])
		{	data[r,COL_ATT_FCT_DBT] <- data[r,COL_ATT_MDT_DBT]
			nbr.corr <- nbr.corr + 1
			changed.start <- TRUE
		}
		# problem with the end date
		if(!is.na(data[r,COL_ATT_FCT_FIN]) && !is.na(data[r,COL_ATT_MDT_FIN]) 
				&& data[r,COL_ATT_FCT_FIN]>data[r,COL_ATT_MDT_FIN])
		{	data[r,COL_ATT_FCT_FIN] <- data[r,COL_ATT_MDT_FIN]
			nbr.corr <- nbr.corr + 1
			changed.end <- TRUE
		}
		
		# log changes
		if(changed.start || changed.end)
		{	#print(data[r,])
			tlog(2,str)
			tlog(2,data[r,COL_ATT_DPT_NOM])
			if(changed.start)
				tlog(4,"Modifying function start")
			if(changed.end)
				tlog(4,"Modifying function end")
		}
	}
	tlog(2, "Total number of corrected function dates: ",nbr.corr)
	
	tlog(2, "Number of rows remaining: ",nrow(data))
	return(data)
}




#############################################################################################
# Loads election-related data and returns the corresponding tables as a list.
#
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
#
# return: a list containing at least election.table, and possibly (in case of series) series.table
#		  and series.list.
#############################################################################################
load.election.data <- function(election.file, series.file)
{	series.present <- hasArg(series.file)
	
	# load election dates
	tlog(4,"Loading the table containing election dates: \"",election.file,"\"")
	col.classes <- c("Date", "Date")
	if(series.present)
		col.classes <- c("Date", "Date", "character")
	election.table <- read.table(
		file=election.file,			# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses=col.classes
	)
	
	# possibly complete second round dates
	tlog(4,"Complete missing dates in the table")
	idx <- which(is.na(election.table[,COL_VERIF_DATE_TOUR2]))
	if(length(idx)>0)
		election.table[idx,COL_VERIF_DATE_TOUR2] <- election.table[idx,COL_VERIF_DATE_TOUR1]
	
	# deal with series
	if(series.present)
	{	# break down series
		series.list <- strsplit(x=election.table[,COL_VERIF_SERIES], split=",", fixed=TRUE)
		
		# prepare classes
		if(COL_ATT_CANT_CODE %in% colnames(series.table))						# CD table
			col.classes <- c("character","integer","character","character")
		else 																	# S table
			col.classes <- c("character","character")
		
		# load the table
		series.table <- read.table(
			file=series.file,			# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE,					# don't convert strings to factors
#			fileEncoding="Latin1",		# original tables seem to be encoded in Latin1 (ANSI)
			colClasses=col.classes
		)
		
		# for debug
		# idx <- match(cant.table[,COL_ATT_CANT_NOM],series.table[,COL_ATT_CANT_NOM])
		# cant.table[which(is.na(idx)),]
	
		# set up result
		res <- list(election.table=election.table, series.table=series.table, series.list=series.list)
	}
	
	# if no series
	else
		res <- list(election.table=election.table)
	
	return(result)
}




#############################################################################################
# Adjusts the start/end of mandates/functions so that they match election dates whenever possible.
# If the start/end date of a mandate is approximately equal to the closest election date, it is
# set to this date. If it contains function dates, these are set to the same date. (Only mandates
# of the same person, obviously).
#
# data: original table.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
#
# return: same table, with mandate/function dates rounded.
#############################################################################################
round.mdtfct.dates <- function(data, election.file, series.file)
{	tlog(0,"Rounding start/end dates when approximately equal to election dates")
	series.present <- hasArg(series.file)
	
	# load election-related data
	tmp <- load.election.data(election.file, series.file)
	election.table <- tmp$election.table
	if(series.present)
	{	series.table <- tmp$series.table
		series.list <- tmp$series.list
	}
	
	
	
	# TODO
	# compare mandate and election dates
	tlog(4,"Check mandate dates against election dates")
	idx <- which(sapply(1:nrow(data), function(r)
					{	tlog(6,"Processing row ",r,"/",nrow(data))
						# get election dates
						election.dates <- election.table
						if(series.present)
						{	# CD table
							if(COL_ATT_CANT_CODE %in% colnames(series.table))
								idx <- which(series.table[,COL_ATT_DPT_CODE]==data[r,COL_ATT_DPT_CODE]
												& series.table[,COL_ATT_CANT_NOM]==data[r,COL_ATT_CANT_NOM])
							# S table
							else 
								idx <- which(series.table[,COL_ATT_DPT_CODE]==data[r,COL_ATT_DPT_CODE])
							# retrieve the series corresponding to the position
							series <- series.table[idx,COL_VERIF_SERIE]
							# and the election dates corresponding to the series
							idx <- sapply(series.list, function(s) is.na(series) || series %in% s)
							election.dates <- election.table[idx,]
						}
						
						# compare with mandate dates
						tests <- apply(election.dates, 1, function(election.row)
								{	(data[r,COL_ATT_MDT_DBT]<election.row[COL_VERIF_DATE_TOUR1] 
												&& (is.na(data[r,COL_ATT_MDT_FIN]) || data[r,COL_ATT_MDT_FIN]>=election.row[COL_VERIF_DATE_TOUR2]))
									#date.intersect(
									#		start1=election.row[COL_VERIF_DATE_TOUR1], 
									#		end1=election.row[COL_VERIF_DATE_TOUR2], 
									#		start2=data.row[COL_ATT_MDT_DBT], 
									#		end2=data.row[COL_ATT_MDT_FIN]
									#)
								}
						)
						res <- any(tests)
						if(res)
						{	
#				tlog(8,paste(data[r,],colapse=","))
#				print(data[r,])
#				print(cbind(election.dates,tests))
#				idx.tests <- which(tests)
							if(length(idx.tests)>1)
								stop("Problem: several rows match")
#				else
#				{	tlog(8,format(data[r,COL_ATT_MDT_DBT]),"--", format(data[r,COL_ATT_MDT_FIN]), " vs. ",
#							format(election.dates[idx.tests,1]), "--", format(election.dates[idx.tests,2]))
#					readline() #stop()
#				}
						}	
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
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#			quote=TRUE,
				se="\t"
		)
	}
	
	return(data)
}




#############################################################################################
# Merges the mandate that strictly overlap and have compatible function data. This concerns
# mandates of a single person, obviously. Compatible function data means that one of the
# rows has no function information, or that both have such information but that the function
# name (or code) is the same.
#
# data: original table.
#
# return: same table, with merged overlapping mandates.
#############################################################################################
merge.overlapping.mandates <- function(data)
{	tlog(0,"Merging overlapping rows of the same person (provided their functions are compatible)")
	
	# set the attribute used to compare functions
	if(COL_ATT_FCT_NOM %in% colnames(data))
		fct.att <- COL_ATT_FCT_NOM
	else if(COL_ATT_FCT_CODE %in% colnames(data))
		fct.att <- COL_ATT_FCT_CODE
	else
		fct.att <- NA
	
	# process each id present in the table
	unique.ids <- sort(unique(data[,COL_ATT_ELU_ID]))
	nbr.corr <- 0
	idx.rmv <- c()
	for(i in 1:length(unique.ids))
	{	idx <- which(data[,COL_ATT_ELU_ID]==unique.ids[i])
		tlog(2,"Processing id ",unique.ids[i]," (",i,"/",length(unique.ids),"): found ",length(idx)," rows")
		
		# nothing to do
		if(i %in% idx.rmv)
			tlog(6,"Row already removed, nothing to compare to")
		else if(length(idx)<2)
			tlog(6,"Unique row, nothing to compare to")
		
		# comparing the to the subsequent rows
		else
		{	for(j in 1:(length(idx)-1))
			{	tlog(4,"Processing row ",j,"/",length(idx))
				for(k in 2:length(idx))
				{	tlog(6,"Comparing to row ",k,"/",length(idx))
					if(date.intersect(start1=data[idx[j],COL_ATT_MDT_DBT], end1=data[idx[j],COL_ATT_MDT_FIN], 	# overlapping mandates
									start2=data[idx[k],COL_ATT_MDT_DBT], end2=data[idx[k],COL_ATT_MDT_FIN])
							&& (is.na(fct.att) || 																# either no function specified at all
								(is.na(data[idx[j],fct.att]) || is.na(data[idx[k],fct.att]) 					# or compatible functions
									|| data[idx[j],fct.att]==data[idx[k],fct.att])))
					{	print(data[c(idx[j],idx[k]),])
						tlog(8, "Overlap detected between")
						tlog(10, format(data[idx[j],COL_ATT_MDT_DBT]),"--",format(data[idx[j],COL_ATT_MDT_FIN])," <<>> ",format(data[idx[j],COL_ATT_FCT_DBT]),"--",format(data[idx[j],COL_ATT_FCT_FIN])," (",data[idx[j],fct.att],")")
						tlog(10, format(data[idx[k],COL_ATT_MDT_DBT]),"--",format(data[idx[k],COL_ATT_MDT_FIN])," <<>> ",format(data[idx[k],COL_ATT_FCT_DBT]),"--",format(data[idx[k],COL_ATT_FCT_FIN])," (",data[idx[k],fct.att],")")
#						readline() #stop()
						tlog(8, "After merge:")
						
						# merge the rows
						# TODO
						
						# update counters
						nbr.corr <- nbr.corr + 1
						idx.rmv <- c(idx.rmv, j)
					}
				}
			}
		}
	}
	tlog(2, "Total number of corrected overlapping rows: ",nbr.corr)
	
	data <- data[-idx.rmv,]
	tlog(2, "Number of rows remaining: ",nrow(data))
	return(data)
}




#############################################################################################
# Some rows span several consecutive mandates. This function splits them in order to respect
# the election dates. The functions dates are updated accordingly.
#
# data: original table.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
#
# return: same table, with split long mandates.
#############################################################################################
split.long.mandates <- function(data, election.file, series.file)
{	tlog(0,"Split rows spanning several mandates")
	series.present <- hasArg(series.file)
	
	# load election-related data
	tmp <- load.election.data(election.file, series.file)
	election.table <- tmp$election.table
	if(series.present)
	{	series.table <- tmp$series.table
		series.list <- tmp$series.list
	}
	
	# TODO
	
	return(data)
}

	
	
	
#############################################################################################
# Deletes the rows corresponding to micro-mandates, i.e. mandates of only a few days, which
# correspond to errors (or sometimes missing mandates impossible to recover through alternative
# means).
#
# data: original table.
#
# return: same table, without the micro-mandates.
#############################################################################################
remove.micro.mandates <- function(data)
{	
	# TODO
	
	return(data)
}




#############################################################################################
# Performs various corrections on the dates defining mandates and functions: 
# 1) Adjusts function dates so that they are contained inside the corresponding mandate period.
# 2) Rounds mandate and function dates to match election dates, when approximately equal.
# 3) Merges rows corresponding to overlapping mandates, provided they have compatible functions.
# 4) Splits rows containing election dates (other than as a start date).
# 5) Removes micro-mandates.
# 
# data: the data table.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
#
# returns: same table, but with corrected dates.
#############################################################################################
fix.mdtfct.dates <- function(data, election.file, series.file)
{	# adjust function dates so that they are contained inside the corresponding mandate period
	data <- adjust.function.dates(data)
	
	# round mandate and function dates to match election dates, when approximately equal
	if(hasArg(election.file))
		data <- round.mdtfct.dates(data, election.file, series.file)
	
	# merge rows corresponding to overlapping mandates of the same person, provided they have compatible functions
	data <- merge.overlapping.mandates(data)
	
	# splits rows containing election dates (other than as a start date)
	if(hasArg(election.file))
		data <- split.long.mandates(data, election.file, series.file)
	
	# removes micro-mandates
	data <- remove.micro.mandates(data)
	
	stop()
	return(data)
}
