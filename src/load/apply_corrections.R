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
	tlog(2,"Columns: ",paste(colnames(data), collapse=","))
	
	# normalize data table column names
	norm.names <- col.map[colnames(data)]
	if(any(is.na(norm.names)) || length(norm.names)!=ncol(data))
		stop("Problem with the number of columns (or their names) when loading the table, after normalization")
	else
		colnames(data) <- norm.names
	
	# EPCI-specific cleaning
	if(COL_ATT_EPCI_NOM %in% colnames(data))
	{	# clean CC names
		data[,COL_ATT_EPCI_NOM] <- gsub(pattern=" (archivé)",replacement="",x=data[,COL_ATT_EPCI_NOM],fixed=TRUE)
		data[,COL_ATT_EPCI_NOM] <- gsub(pattern=" - archivé",replacement="",x=data[,COL_ATT_EPCI_NOM],fixed=TRUE)
	}
	
	# setting appropriate encoding of string columns, replace "" by NAs, and normalize proper nouns
	tlog(0,"Cleaning/encoding/normalizing strings")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		tlog(2,"Processing column \"",col.name,"\" (",c,"/",ncol(data),")")
		
		# the column is an actual string
		if(col.type %in% c("cat","nom"))
		{	tlog(4,"Column \"",col.name,"\" treated as a string")
			
			# convert encoding
			##			data[,c] <- iconv(x=data[,c], from="Latin1", to="UTF8")
#			data[,c] <- iconv(x=data[,c], to="UTF8")
			
			# remove diacritics
			data[,c] <- remove.diacritics(data[,c])
			
			# normalize proper nouns
			if(col.name %in% COLS_ATT_PROPER_NOUNS)
				data[,c] <- normalize.proper.nouns(data[,c])
			if(col.name %in% COLS_ATT_LOCATION_NOUNS)
				data[,c] <- normalize.location.nouns(data[,c])
		}
		
		# the column is not a string
		else
			tlog(4,"Column \"",col.name,"\": not a string")
		
		# trim leading/ending whitespace
		data[,c] <- trimws(data[,c])
		
		# replace empty cells by explicit NAs
		data[which(data[,c]==""),c] <- NA
		
		# replace "NA"s by actual NAs
		data[which(data[,c]=="NA"),c] <- NA
	}
	tlog(2,"CHECKPOINT 0: Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	
	# add columns to store correction flags
	correc.date <- rep(FALSE, nrow(data))
	correc.info <- rep(FALSE, nrow(data))
	data <- cbind(data, correc.date, correc.info)
	colnames(data)[(ncol(data)-1):ncol(data)] <- c(COL_ATT_CORREC_DATE, COL_ATT_CORREC_INFO) 
	
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
			mat <- t(future_sapply(1:nrow(data), function(i)
			{	id <- data[i,COL_ATT_ELU_ID_RNE]
				state <- data[i,COL_ATT_CORREC_INFO]
				new.id <- conv.map[id]
				if(is.na(new.id))
					res <- c(id,state)
				else
					res <- c(new.id,TRUE)
				return(res)
			}))
			corrected.ids <- which(data[,COL_ATT_ELU_ID_RNE]!=mat[,1])
			data[,COL_ATT_ELU_ID_RNE] <- mat[,1]
			data[,COL_ATT_CORREC_INFO] <- as.logical(mat[,2])
			tlog(2,"CHECKPOINT 1: Fixed ",length(corrected.ids)," rows (",(100*length(corrected.ids)/nrow(data)),"%)")
		}
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
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
	corrected.rows <- c()
	if(nrow(correc.table)>0)	
	{	tlog(0,"Applying ad hoc corrections")
		
		# apply each correction one after the other
		idx.rm <- c()
		for(r in 1:nrow(correc.table))
		{	correc.attr <- correc.table[r,COL_CORREC_ATTR]
			if(correc.attr %in% c(COL_ATT_MDT_DBT, COL_ATT_MDT_FIN, COL_ATT_FCT_DBT, COL_ATT_FCT_FIN))
				correc.col <- COL_ATT_CORREC_DATE
			else
				correc.col <- COL_ATT_CORREC_INFO
			row <- as.integer(correc.table[r,COL_CORREC_ROW])
			
			# general correction
			if(all(is.na(correc.table[r,c(COL_CORREC_ID,COL_CORREC_NOM,COL_CORREC_PRENOM)])))
			{	# identify the targeted rows in the data table
				idx <- which(data[,correc.attr]==correc.table[r,COL_CORREC_VALAVT]
								| is.na(data[,correc.attr]) & is.na(correc.table[r,COL_CORREC_VALAVT]))
				
				# there should be at least one
				if(length(idx)<1)
				{	tlog(4,"Could not find a correction: ",paste(correc.table[r,], collapse=";"))
					stop(paste0("Could not find a correction: ",paste(correc.table[r,], collapse=";")))
				}
				# if several, try to check the specified row
				else 
				{	if(!is.na(row))
					{	if(length(idx)==1 && idx!=row)
						{	tlog(4,"Row ",idx," matches the criteria but not the specified row (",row,")")
							stop(paste0("Row ",idx," matches the criteria but not the specified row (",row,")"))
						}
						else if(length(idx)>1)
						{	tlog(4,"Found several rows matching the criteria when there's only one specified row: ",row," vs. ",paste(idx, collapse=","))
							stop(paste0("Found several rows matching the criteria when there's only one specified row: ",row," vs. ",paste(idx, collapse=",")))
						}
					}
					tlog(4,"Replacing ",correc.table[r,COL_CORREC_VALAVT]," by ",correc.table[r,COL_CORREC_VALAPR])
					data[idx,correc.attr] <- correc.table[r,COL_CORREC_VALAPR]
					data[idx,correc.col] <- TRUE
					corrected.rows <- union(corrected.rows,idx)
				}
			}
			
			# correction of a specific row
			else
			{	# identify the targeted row in the data table
				idx <- which(data[,COL_ATT_ELU_ID_RNE]==correc.table[r,COL_CORREC_ID]
							& data[,COL_ATT_ELU_NOM]==correc.table[r,COL_CORREC_NOM]
							& data[,COL_ATT_ELU_PRENOM]==correc.table[r,COL_CORREC_PRENOM]
							& (data[,correc.attr]==correc.table[r,COL_CORREC_VALAVT]
								| is.na(data[,correc.attr]) & is.na(correc.table[r,COL_CORREC_VALAVT]))
				)
				
				# there should be exactly one
				if(length(idx)<1)
				{	tlog(4,"Could not find a correction: ",paste(correc.table[r,], collapse=";"))
					stop(paste0("Could not find a correction: ",paste(correc.table[r,], collapse=";")))
				}
				else if(length(idx)>1)
				{	if(is.na(row))
					{	tlog(4,"A correction matches several cases (",paste(idx, collapse=","),"), but no row is specified: ",paste(correc.table[r,], collapse=";"))
						stop(paste0("A correction matches several cases (",paste(idx, collapse=","),"), but no row is specified: ",paste(correc.table[r,], collapse=";")))
					}
#						tlog(4,"WARNING: A correction matches several cases: ",paste(correc.table[r,], collapse=";"))
#						data[idx,correc.attr] <- correc.table[r,COL_CORREC_VALAPR]
					else
					{	if(row %in% idx)
						{	if(correc.attr==COL_ATT_ELU_ID_RNE && is.na(correc.table[r,COL_CORREC_VALAPR]))
							{	tlog(4, "Row ",row," marked for removal")
								idx.rm <- c(idx.rm, row)
							}
							else
							{	tlog(4,"Correcting entry: ",paste(correc.table[r,], collapse=";"))
								data[row,correc.attr] <- correc.table[r,COL_CORREC_VALAPR]
								data[idx,correc.col] <- TRUE
								corrected.rows <- union(corrected.rows,idx)
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
					{	if(correc.attr==COL_ATT_ELU_ID_RNE && is.na(correc.table[r,COL_CORREC_VALAPR]))
						{	tlog(4, "Row ",idx," marked for removal")
							idx.rm <- c(idx.rm, idx)
						}
						else
						{	if(is.na(row))
								tlog(4,"Correcting entry (",idx,"): ",paste(correc.table[r,], collapse=";"))
							else
								tlog(4,"Correcting entry: ",paste(correc.table[r,], collapse=";"))
							data[idx,correc.attr] <- correc.table[r,COL_CORREC_VALAPR]
							data[idx,correc.col] <- TRUE 
							corrected.rows <- union(corrected.rows,idx)
						}
					}
				}
			}
		}
		tlog(2,"CHECKPOINT 2: Corrected ",length(corrected.rows)," rows in total (",(100*length(corrected.rows)/nrow(data)),"%)")
		
		# remove the marked rows
		if(length(idx.rm)>0)
		{	tlog(2,"Removing ",length(idx.rm)," rows from the table (",(100*length(idx.rm)/nrow(data)),"%)")
			data <- data[-idx.rm,]
		}
		
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	}
	
	return(data)
}




#############################################################################################
# Applies the minimal ad hoc corrections to the raw RNE data, so that later test can
# still be performed.
#
# data: raw data, before applying the corrections.
#
# returns: data frame after the corrections.
#############################################################################################
apply.minimal.adhoc.corrections <- function(data, type)
{	# only for the CD table
	if(type=="CD")
	{	cant.map <- c()
		cant.map["LA COTE VERMEILLE"] <- "COTE VERMEILLE"
		cant.map["LE GOND PONTOUVRE"] <- "GOND PONTOUVRE"
		cant.map["LE GRAND BOURG"] <- "GRAND BOURG"
		cant.map["LE TAMPON 1"] <- "TAMPON 1"
		cant.map["LE TAMPON 2"] <- "TAMPON 2"
		cant.map["LES ABYMES 1"] <- "ABYMES 1"
		cant.map["LES ABYMES 2"] <- "ABYMES 2"
		cant.map["LES ABYMES 3"] <- "ABYMES 3"
		cant.map["CARCASONNE 2 SUD"] <- "CARCASSONNE 2 SUD"
		cant.map["SALIGNAC EYVIGNES"] <- "SALIGNAC EYVIGUES"
		cant.map["MAMOUDZOU 1ER CANTON"] <- "MAMOUDZOU 1"
		cant.map["MAMOUDZOU 2E CANTON"] <- "MAMOUDZOU 2"
		cant.map["MAMOUDZOU 3E CANTON"] <- "MAMOUDZOU 3"
		cant.map["MARSEILLE LA BLANCARDE"] <- "LA BLANCARDE"
		cant.map["MARSEILLE LES TROIS LUCS"] <- "LES TROIS LUCS"
		cant.map["MARSEILLE SAINTE MARGUERITE"] <- "SAINTE MARGUERITE"
		cant.map["MARSEILLE NOTRE DAME LIMITE"] <- "NOTRE DAME LIMITE"
		cant.map["MARSEILLE VAUBAN"] <- "VAUBAN"
		for(i in 1:length(cant.map))
			data[which(data[,COL_ATT_CANT_NOM]==names(cant.map)[i]),COL_ATT_CANT_NOM] <- cant.map[i]
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
	corr.rows <- c()
	if(COL_ATT_COM_CODE %in% colnames(data))
	{	tlog(0,"Normalizing municipality ids")
		tmp <- data[,COL_ATT_COM_CODE]
		# we simply keep the first three characters of the id
		data[,COL_ATT_COM_CODE] <- substr(x=data[,COL_ATT_COM_CODE], start=1, stop=3)
		idx <- which(data[,COL_ATT_COM_CODE]!=tmp)
		data[idx,COL_ATT_CORREC_INFO] <- TRUE 
		corr.rows <- union(corr.rows,idx)
		tlog(2,"Adjusted ",length(idx)," municipality ids")
	}
	# replace the NC political nuance by proper NAs
	if(COL_ATT_ELU_NUANCE %in% colnames(data))
	{	tlog(0,"Normalizing political nuance labels")
		idx <- which(data[,COL_ATT_ELU_NUANCE]=="NC")
		if(length(idx)>0)
		{	data[idx,COL_ATT_ELU_NUANCE] <- NA
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Fixed ",length(idx)," political nuances")
	}
	# EPCI-specific cleaning
	if(COL_ATT_EPCI_NOM %in% colnames(data))
	{	# clean missing department codes
		tlog(0,"Cleaning missing department codes")
		idx <- which(data[,COL_ATT_DPT_CODE]=="0" | data[,COL_ATT_DPT_CODE]=="00")
		if(length(idx)>0)
		{	data[idx,COL_ATT_DPT_CODE] <- NA
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Cleaned ",length(idx)," rows")
		
		# clean missing municipality codes
		tlog(0,"Cleaning missing municipality codes")
		idx <- which(data[,COL_ATT_COM_CODE]=="0" | data[,COL_ATT_COM_CODE]=="00")
		if(length(idx)>0)
		{	data[idx,COL_ATT_COM_CODE] <- NA
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Cleaned ",length(idx)," rows")
		
		# complete missing municipality names (when code is available)
		idx <- which(is.na(data[,COL_ATT_COM_NOM]) & !is.na(data[,COL_ATT_COM_CODE]))
		if(length(idx)>0)
		{	idx2 <- idx[is.na(data[idx,COL_ATT_DPT_CODE])]
			if(length(idx2)>0)
				data[idx2,COL_ATT_DPT_CODE] <- data[idx2,COL_ATT_EPCI_DPT]
			locs <- cbind(apply(cbind(data[,COL_ATT_DPT_CODE],data[,COL_ATT_COM_CODE]),1,function(r) paste(r,collapse="_")),data[,COL_ATT_COM_NOM])
			idx2 <- match(locs[idx,1],locs[-idx,1])
			idx3 <- which(!is.na(idx2))
			idx1 <- idx[idx3]
			idx2 <- idx2[idx3]
			if(length(idx2)>0)
			{	data[idx1,COL_ATT_COM_NOM] <- locs[-idx,2][idx2]
				data[idx1,COL_ATT_CORREC_INFO] <- TRUE 
				corr.rows <- union(corr.rows,idx1)
				idx <- idx1
			}
		}		
		tlog(2,"Fixed ",length(idx)," rows")
	}
	tlog(0,"CHECKPOINT 3: Fixed a total of ",length(corr.rows)," rows (",(100*length(corr.rows)/nrow(data)),"%) for various (non-date-related) issues")
	
	# remove rows without mandate dates and without function dates
	tlog(0,"Removing rows with no mandate and no function date")
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	idx <- which(is.na(data[,COL_ATT_MDT_DBT]) & is.na(data[,COL_ATT_MDT_FIN]) 
						& is.na(data[,COL_ATT_FCT_DBT]) & is.na(data[,COL_ATT_FCT_FIN]))
		tlog(2,"CHECKPOINT 4: Removed ",length(idx)," incomplete rows (",(100*length(idx)/nrow(data)),"%)")
		if(length(idx)>0)
			data <- data[-idx, ]
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	}
	
	# use mandate start date when function start date is missing
	corr.rows <- c()
	if(COL_ATT_FCT_NOM %in% colnames(data))
	{	tlog(0,"Completing missing function start dates using mandate start dates")
		idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) & is.na(data[,COL_ATT_FCT_DBT]))
		if(length(idx)>0)
		{	data[idx,COL_ATT_FCT_DBT] <- data[idx,COL_ATT_MDT_DBT]
			data[idx,COL_ATT_CORREC_DATE] <- TRUE
			corr.rows <- union(corr.rows, idx)
		}
		tlog(2,"Fixed ",length(idx)," rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	}
	# use mandate end date when function end date is missing 
	if(COL_ATT_FCT_NOM %in% colnames(data))
	{	tlog(0,"Completing missing function end dates using mandate end dates")
		idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) & is.na(data[,COL_ATT_FCT_FIN]) & !is.na(data[,COL_ATT_MDT_FIN]))
		if(length(idx)>0)
		{	data[idx,COL_ATT_FCT_FIN] <- data[idx,COL_ATT_MDT_FIN]
			data[idx,COL_ATT_CORREC_DATE] <- TRUE 
			corr.rows <- union(corr.rows, idx)
		}			
		tlog(2,"Fixed ",length(idx)," rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	}
	tlog(0,"CHECKPOINT 5: Fixed a total of ",length(corr.rows)," rows (",(100*length(corr.rows)/nrow(data)),"%) for start/end function date using mandate dates")
	
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
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	
	
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
		
		# convert to handle multiple codes/names
		dpt.table.copy <- dpt.table[-(1:nrow(dpt.table)),]
		for(r in 1:nrow(dpt.table))
		{	# retrieve info
			id <- dpt.table[r,COL_ATT_DPT_ID]
			codes <- strsplit(dpt.table[r,COL_ATT_DPT_CODE], ",", fixed=TRUE)[[1]]
			code <- codes[length(codes)]
			names <- strsplit(dpt.table[r,COL_ATT_DPT_NOM], ",", fixed=TRUE)[[1]]
			# add to table
			for(name in names)
			{	df <- data.frame(id,code,name, stringsAsFactors=FALSE)
				dpt.table.copy <- rbind(dpt.table.copy,df)
			}
		}
		colnames(dpt.table.copy) <- colnames(dpt.table)
		
		# get the appropriate unique ids
		dpt.idx <- match(data[,COL_ATT_DPT_NOM], dpt.table.copy[,COL_ATT_DPT_NOM])
		
		# insert in the table
		dpt.ids <- data.frame(dpt.table.copy[dpt.idx, COL_ATT_DPT_ID], stringsAsFactors=FALSE)
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
	tlog(0,"Merging compatible rows for compulsory columns \"",paste(comp.cols, collapse="\",\""),"\"")
	rm.col <- which(colnames(data) %in% comp.cols)
	date.cols <- which(colnames(data) %in% c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN))
	
	# identify redundant rows
	concat <- apply(data[,comp.cols], 1, function(row) paste(row, collapse=":"))
	tt <- table(concat)
	codes <- names(tt)[which(tt>1)]
	tlog(2,"Looking for possibly redundant rows: found ",length(codes))
	
	# identify compatible rows against redundant ones
	tlog(2,"Looking for compatible rows among them")
	tmp <- lapply(1:length(codes), function(i)
	{	code <- codes[i]
		tlog(4,"Processing code ",code," (",i,"/",length(codes),")")
		res <- c()
		
		rs <- which(concat==code)
		while(length(rs)>=2)
		{	r1 <- rs[1]
			rs <- rs[-1]
			r2 <- 1
			while(r2<=length(rs))
			{	if(all(is.na(data[r1,-rm.col]) | is.na(data[rs[r2],-rm.col]) | data[r1,-rm.col]==data[rs[r2],-rm.col]))
				{	tlog(6, "Found a match:")
					tlog(6, format.row(data[r1,]))
					tlog(6, format.row(data[rs[r2],]))
					
					idx <- which(is.na(data[r1,]))
					if(length(idx)>0)
					{	data[r1,idx] <<- data[rs[r2],idx]
						tlog(6, "After: ",format.row(data[r1,]))
						if(length(intersect(idx,date.cols))>0)
							data[r1,COL_ATT_CORREC_DATE] <- TRUE 
						if(length(setdiff(idx,date.cols))>0)
							data[r1,COL_ATT_CORREC_INFO] <- TRUE 
					}
					res <- c(res, rs[r2])
					rs <- rs[-r2]
				}
				else
					r2 <- r2 + 1
			}
		}
		return(res)
	})
	
	# actually remove the rows marked for deletion
	nbr.before <- nrow(data)
	removed.nbr <- 0
	if(length(tmp)>0)
	{	idx <- unlist(tmp)
		if(length(idx)>0)
		{	removed.nbr <- length(idx)
			data <- data[-idx,]
		}
	}
	tlog(2,"CHECKPOINT 6: Done merging compatible rows, removed ",removed.nbr," rows (",(100*removed.nbr/nbr.before),"%)")
	
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
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
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	
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
	
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	# process each row
		for(r in 1:nrow(data))
		{	str <- paste0("Considering (",r,"/",nrow(data),") ", format.row.dates(data[r,]))
			str2 <- format.row(data[r,])
			
			changed.start <- FALSE
			changed.end <- FALSE
			
			# problem with the start date
			if(!is.na(data[r,COL_ATT_FCT_DBT]) && !is.na(data[r,COL_ATT_MDT_DBT]) 
					&& data[r,COL_ATT_FCT_DBT]<data[r,COL_ATT_MDT_DBT])
			{	data[r,COL_ATT_FCT_DBT] <- data[r,COL_ATT_MDT_DBT]
				changed.start <- TRUE
			}
			# problem with the end date
			if(!is.na(data[r,COL_ATT_FCT_FIN]) && !is.na(data[r,COL_ATT_MDT_FIN]) 
					&& data[r,COL_ATT_FCT_FIN]>data[r,COL_ATT_MDT_FIN])
			{	data[r,COL_ATT_FCT_FIN] <- data[r,COL_ATT_MDT_FIN]
				changed.end <- TRUE
			}
			
#			# sometimes, the function start date is posterior to the mandate end date
#			if(!is.na(data[r,COL_ATT_FCT_DBT]) && !is.na(data[r,COL_ATT_MDT_FIN])
#							&& data[r,COL_ATT_FCT_DBT]>data[r,COL_ATT_MDT_FIN])
#			{	data[r,COL_ATT_FCT_DBT] <- data[r,COL_ATT_MDT_FIN]
#				changed.start <- TRUE
#			}
			
			# log changes
			if(changed.start || changed.end)
			{	#print(data[r,])
				tlog(2,str)
				tlog(4,"Before: ",str2)
				if(changed.start)
					tlog(6,"Modifying function start")
				if(changed.end)
					tlog(6,"Modifying function end")
				data[r,COL_ATT_CORREC_DATE] <- TRUE
				tlog(4,"After: ",format.row(data[r,]))
				nbr.corr <- nbr.corr + 1
			}
		}
	}
	
	tlog(2, "CHECKPOINT 7: Total number of adjusted function dates: ",nbr.corr, " (",(100*nbr.corr/nrow(data)),"%)")
	tlog(2, "Number of rows remaining: ",nrow(data))
	return(data)
}




#############################################################################################
# Loads election-related data and returns the corresponding tables as a list.
#
# data: main table.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
#
# return: a list containing at least election.table, and possibly (in case of series) series.table
#		  and series.list.
#############################################################################################
load.election.data <- function(data, election.file, series.file)
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
		colClasses=col.classes		# column data types
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
		if(COL_ATT_CANT_CODE %in% colnames(data))								# CD table
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
			colClasses=col.classes		# column data types
		)
		series.table[series.table[,COL_VERIF_SERIE]=="",COL_VERIF_SERIE] <- NA
		
		# for debug
		# idx <- match(cant.table[,COL_ATT_CANT_NOM],series.table[,COL_ATT_CANT_NOM])
		# cant.table[which(is.na(idx)),]
	
		# set up result
		res <- list(election.table=election.table, series.table=series.table, series.list=series.list)
	}
	
	# if no series
	else
		res <- list(election.table=election.table)
	
	return(res)
}




#############################################################################################
# Adjusts the start/end of mandates/functions so that they match election dates whenever possible.
# If the start/end date of a mandate is approximately equal to the closest election date, it is
# set to this date. If it contains function dates, these are set to the same date.
#
# data: original table.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
# tolerance: maximal difference between 2 dates for them to be considered the same.
#
# return: same table, with mandate/function dates rounded.
#############################################################################################
round.mdtfct.dates <- function(data, election.file, series.file, tolerance)
{	tlog(0,"Rounding start/end dates when approximately equal to election dates")
	series.present <- hasArg(series.file)
	col.mdt <- c(COL_ATT_MDT_DBT, COL_ATT_MDT_FIN)
	col.fct <- c(COL_ATT_FCT_DBT, COL_ATT_FCT_FIN)
	has.fct <- COL_ATT_FCT_DBT %in% colnames(data)
	
	# load election-related data
	tmp <- load.election.data(data, election.file, series.file)
	election.table <- tmp$election.table
	if(series.present)
	{	series.table <- tmp$series.table
		series.list <- tmp$series.list
	}
	
	# compare mandate and election dates
	tlog(2,"Check mandate dates against election dates")
	nbr.corrected <- 0
	for(r in 1:nrow(data))
	{	tlog(4,"Processing row ",r,"/",nrow(data),": ",format.row.dates(data[r,]))
		
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
		
		# process the smallest election difference for each mandate and election date
		date.diffs <- t(sapply(1:nrow(election.dates), function(e)
		{	sapply(1:length(col.mdt), function(i)
			{	diffs <- c(data[r, col.mdt[i]]-election.dates[e, COL_VERIF_DATE_TOUR1],
					data[r, col.mdt[i]]-election.dates[e, COL_VERIF_DATE_TOUR2])
				diffs <- diffs[!is.na(diffs)]
				if(length(diffs)>0)
					diffs[which.min(abs(diffs))]
				else
					NA
			})
		}))
		date.diffs[,2] <- date.diffs[,2] + 1	# we want to end one day before the election
		
		# get the closest date for each election
		idx <- apply(date.diffs, 2, function(dates)
		{	res <- which.min(abs(dates))
			if(length(res)==0)
				res <- NA
			return(res)
		})
		
		# possibly update mandate/function dates
		#bef.str <- format.row.dates(data[r,])
		bef.str <- format.row(data[r,])
		mdt.changed <- c(FALSE,FALSE)
		fct.changed <- c(FALSE,FALSE)
		date.diff <- c(NA,NA)
		for(i in 1:2)
		{	if(!is.na(idx[i]))
			{	date.diff[i] <- date.diffs[idx[i],i]
				if(abs(date.diff[i])>0 && abs(date.diff[i])<tolerance)
				{	if(has.fct && !is.na(data[r,col.fct[i]]) && !is.na(data[r,col.mdt[i]]) && data[r,col.fct[i]]==data[r,col.mdt[i]])
					{	data[r,col.fct[i]] <- data[r,col.fct[i]] - date.diff[i]
						fct.changed[i] <- TRUE
					}
					data[r,col.mdt[i]] <- data[r,col.mdt[i]] - date.diff[i]
					data[r,COL_ATT_CORREC_DATE] <- TRUE
					mdt.changed[i] <- TRUE
				}
			}
		}
		
		# adjustments, probably for micro-mandates anyways
		if(!is.na(data[r,COL_ATT_MDT_FIN]) && data[r,COL_ATT_MDT_FIN]<data[r,COL_ATT_MDT_DBT])
			data[r,COL_ATT_MDT_FIN] <- data[r,COL_ATT_MDT_DBT]
		if(has.fct && !is.na(data[r,COL_ATT_FCT_DBT]) && !is.na(data[r,COL_ATT_FCT_FIN]) && data[r,COL_ATT_FCT_FIN]<data[r,COL_ATT_FCT_DBT])
			data[r,COL_ATT_FCT_FIN] <- data[r,COL_ATT_FCT_DBT]

		# possibly update mandate and/or function motives
		motive.changed <- FALSE
		if(mdt.changed[2] || (!is.na(date.diff[2]) && abs(date.diff[2])==0))	# if the mandate end was aligned, or already aligned
		{	# missing mandate end motive
			if(is.na(data[r,COL_ATT_MDT_MOTIF]))
			{	data[r,COL_ATT_MDT_MOTIF] <- "FM"	# regular motive for mandate end
				data[r,COL_ATT_CORREC_INFO] <- TRUE
				motive.changed <- TRUE
			}
			# end fonction aligned, and missing mandate end motive
			if(has.fct && !is.na(data[r,COL_ATT_FCT_FIN]) && data[r,COL_ATT_FCT_FIN]==data[r,COL_ATT_MDT_FIN] && is.na(data[r,COL_ATT_FCT_MOTIF]))
			{	data[r,COL_ATT_FCT_MOTIF] <- "FM"	# regular motive for function end
				data[r,COL_ATT_CORREC_INFO] <- TRUE
				motive.changed <- TRUE
			}
		}
		
		# log changes
		if(any(c(mdt.changed,fct.changed,motive.changed)))
		{	tlog(6, "Before: ", bef.str)
			nbr.corrected <- nbr.corrected + 1
			# log changes
			#tlog(6, "After: ", format.row.dates(data[r,]))
			tlog(6, "After: ", format.row(data[r,]))
			#readline()
		}
		
		# debug
		#if(motive.changed)
		#	readline()		
	}
	tlog(2,"CHECKPOINT 8: Rounded ",nbr.corrected," rows with election-related issues (",(100*nbr.corrected/nrow(data)),"%)")
	
	return(data)
}




#############################################################################################
# Merges the mandates that strictly overlap and have compatible data. Compatible data means 
# that besides the personal info and mandate dates, for the rest of the columns, one value must 
# be NA or both values must be exactly identical.
#
# data: original table.
# type: type of the considered mandate (CD, CM, etc.).
#
# return: same table, with merged overlapping mandates.
#############################################################################################
merge.overlapping.mandates <- function(data, type)
{	tlog(0,"Merging overlapping rows of the same person (provided their functions are compatible)")
	has.fct <- COL_ATT_FCT_DBT %in% colnames(data)
	
	# set the attributes used to test for compatibility
	atts <- setdiff(colnames(data), c(COL_ATT_ELU_ID, 
					COL_ATT_MDT_DBT, COL_ATT_MDT_FIN, 
					COL_ATT_FCT_DBT, COL_ATT_FCT_FIN))
	
	# set the attribute used to represent the function
	fct.att <- NA
	if(COL_ATT_FCT_NOM %in% colnames(data))
		fct.att <- COL_ATT_FCT_NOM
	else if(COL_ATT_FCT_CODE %in% colnames(data))
		fct.att <- COL_ATT_FCT_CODE
	
	# list of circonscription codes
	if(type=="CD")
		circo.codes <- data[,COL_ATT_CANT_ID]
	else if(type=="CM" || type=="M")
		circo.codes <- apply(data[,c(COL_ATT_DPT_CODE,COL_ATT_COM_CODE)], 1, function(r) paste(r,collapse="_"))
	else if(type=="CR")
		circo.codes <- data[,COL_ATT_REG_CODE]
	else if(type=="D")
		circo.codes <- apply(data[,c(COL_ATT_DPT_CODE,COL_ATT_CIRC_CODE)], 1, function(r) paste(r,collapse="_"))
	else if(type=="DE")
		circo.codes <- data[,COL_ATT_CIRCE_NOM]
	else if(type=="EPCI")
		circo.codes <- data[,COL_ATT_EPCI_SIREN]
	else if(type=="S")
		circo.codes <- data[,COL_ATT_DPT_CODE]
	
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
				
				for(k in (j+1):length(idx))
				{	tlog(6,"Comparing to row ",k,"/",length(idx))
					
					if((is.na(circo.codes[idx[i]]) || is.na(circo.codes[idx[j]])				# at least one NA circonscription, 
						|| circo.codes[idx[i]]==circo.codes[idx[j]])							# or same circonscription
							&& date.intersect(start1=data[idx[j],COL_ATT_MDT_DBT], 				# mandates must overlap
									end1=data[idx[j],COL_ATT_MDT_FIN],
									start2=data[idx[k],COL_ATT_MDT_DBT], 
									end2=data[idx[k],COL_ATT_MDT_FIN])
							&& (is.na(fct.att) || 												# either no function specified at all in the table
								is.na(data[idx[j],fct.att]) || is.na(data[idx[k],fct.att]) 		# or NA function in at least one of the rows
									|| is.na(data[idx[j],COL_ATT_FCT_DBT])						# or no function date in at least one of the rows 
									|| is.na(data[idx[k],COL_ATT_FCT_DBT])
									|| (data[idx[j],fct.att]==data[idx[k],fct.att] && 			# or the same function in both row, in which case
										date.intersect(start1=data[idx[j],COL_ATT_FCT_DBT],		# the function dates must overlap too
												end1=data[idx[j],COL_ATT_FCT_FIN],
												start2=data[idx[k],COL_ATT_FCT_DBT], 
												end2=data[idx[k],COL_ATT_FCT_FIN])))
					)
					{	# log detected overlap
						tlog(10, format.row(data[idx[j],]))
						tlog(10, format.row(data[idx[k],]))
						tlog(8, "Overlap detected between")
						tlog(10, format.row.dates(data[idx[j],]),if(has.fct) paste0(" (",data[idx[j],fct.att],")") else "")
						tlog(10, format.row.dates(data[idx[k],]),if(has.fct) paste0(" (",data[idx[k],fct.att],")") else "")
						
						# update mandate start date
						if(is.na(data[idx[j],COL_ATT_MDT_DBT]) || is.na(data[idx[k],COL_ATT_MDT_DBT]))
							stop("ERROR: empty mandate start date")
						else 
							data[idx[j],COL_ATT_MDT_DBT] <- min(data[idx[j],COL_ATT_MDT_DBT], data[idx[k],COL_ATT_MDT_DBT])
						# update mandate end date
						if(is.na(data[idx[j],COL_ATT_MDT_FIN]) || is.na(data[idx[k],COL_ATT_MDT_FIN]))
							data[idx[j],COL_ATT_MDT_FIN] <- NA
						else
							data[idx[j],COL_ATT_MDT_FIN] <- max(data[idx[j],COL_ATT_MDT_FIN], data[idx[k],COL_ATT_MDT_FIN])
						
						# possibly update function dates
						if(!is.na(fct.att))
						{	if(is.na(data[idx[j],COL_ATT_FCT_DBT]))			# the end date should also be NA (first row)
								data[idx[j],c(COL_ATT_FCT_DBT,COL_ATT_FCT_FIN)] <- data[idx[k],c(COL_ATT_FCT_DBT,COL_ATT_FCT_FIN)]
							else if(!is.na(data[idx[k],COL_ATT_FCT_DBT]))	# both start dates are not NA (second row)
							{	data[idx[j],COL_ATT_FCT_DBT] <- min(data[idx[j],COL_ATT_FCT_DBT], data[idx[k],COL_ATT_FCT_DBT])
								if(!is.na(data[idx[j],COL_ATT_FCT_FIN]) && !is.na(data[idx[k],COL_ATT_FCT_FIN]))
									data[idx[j],COL_ATT_FCT_FIN] <- max(data[idx[j],COL_ATT_FCT_FIN], data[idx[k],COL_ATT_FCT_FIN])
								else
									data[idx[j],COL_ATT_FCT_FIN] <- NA
							}
						}
						
						# update the rest of the columns
						for(c in 1:length(atts))
						{	if(is.na(data[idx[j],atts[c]]))
								data[idx[j],atts[c]] <- data[idx[k],atts[c]]
						}
						
						# log merged row
						tlog(8, "After merge:")
						tlog(10, format.row(data[idx[j],]))
						#tlog(10, format.row.dates(data[idx[j],])," (",data[idx[j],fct.att],")")
						#readline() #stop()
						
						# update counters
						nbr.corr <- nbr.corr + 1
						idx.rmv <- c(idx.rmv, idx[k])
						data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
					}
				}
			}
		}
	}
	tlog(2, "CHECKPOINT 10: Total number of rows deleted after merging: ",nbr.corr, " (",100*nbr.corr/nrow(data),"%)")
	
	if(length(idx.rmv)>0)
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
	nbr.before <- nrow(data)
	col.mdt <- c(COL_ATT_MDT_DBT, COL_ATT_MDT_FIN)
	col.fct <- c(COL_ATT_FCT_DBT, COL_ATT_FCT_FIN)
	has.fct <- COL_ATT_FCT_DBT %in% colnames(data)
	new.data <- data[-(1:nrow(data)),]
	
	# load election-related data
	tmp <- load.election.data(data, election.file, series.file)
	election.table <- tmp$election.table
	if(series.present)
	{	series.table <- tmp$series.table
		series.list <- tmp$series.list
	}
	
	# compare mandate and election dates
	tlog(2,"Check mandate dates against election dates")
	nbr.splits <- 0
	for(r in 1:nrow(data))
	{	# specific case of Senators representing people leaving abroad: don't split mandates
		if(!(series.present 
				&& COL_ATT_DPT_CODE %in% colnames(series.table) 
				&& data[r,COL_ATT_DPT_NOM]=="FRANCAIS DE L ETRANGER"))
		{	split.flag <- TRUE
			
			while(split.flag)
			{	tlog(4,"Processing row ",r,"/",nrow(data),": ",format.row.dates(data[r,]))
				tlog(6, format.row(data[r,]))
				split.flag <- FALSE
				
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
				
				# look for mandates containing election dates
				tests <- (data[r,COL_ATT_MDT_DBT]<election.dates[,COL_VERIF_DATE_TOUR1] 
							& (is.na(data[r,COL_ATT_MDT_FIN]) 
								| data[r,COL_ATT_MDT_FIN]>=election.dates[,COL_VERIF_DATE_TOUR2]))
				res <- any(tests)
				
				# possibly split the row
				if(res)
				{	idx.tests <- which(tests)
					if(length(idx.tests)>1)
						#stop("ERROR: several elections match")
						idx.tests <- idx.tests[1]
#					else
					{	# log event
						tlog(6,"Splitting overlap detected for election ",format(election.dates[idx.tests,1]),"--",format(election.dates[idx.tests,2]))
						tlog(8,"Before: ",format.row.dates(data[r,])," vs. ", format(election.dates[idx.tests,1]), "--", format(election.dates[idx.tests,2]))
						data[r,COL_ATT_CORREC_DATE] <- TRUE
						#readline() #stop()
						
						# copy row
						new.row <- data[r,]
				
						# update mandate start date in existing row and end date in new row
						data[r,COL_ATT_MDT_DBT] <- election.dates[idx.tests,COL_VERIF_DATE_TOUR2]
						new.row[1,COL_ATT_MDT_FIN] <- election.dates[idx.tests,COL_VERIF_DATE_TOUR2] - 1
						new.row[1,COL_ATT_MDT_MOTIF] <- "FM"	# regular motive for mandate end
						
						# possibly update similarly function dates
						if(has.fct && !is.na(data[r,COL_ATT_FCT_DBT]))
						{	# case where the function overlaps two consecutive mandates
							if(data[r,COL_ATT_FCT_DBT]<election.dates[idx.tests,COL_VERIF_DATE_TOUR1] 
								&& (is.na(data[r,COL_ATT_FCT_FIN]) 
									|| data[r,COL_ATT_FCT_FIN]>=election.dates[idx.tests,COL_VERIF_DATE_TOUR2]))
							{	data[r,COL_ATT_FCT_DBT] <- election.dates[idx.tests,COL_VERIF_DATE_TOUR2]
								new.row[1,COL_ATT_FCT_FIN] <- election.dates[idx.tests,COL_VERIF_DATE_TOUR2] - 1
								new.row[1,COL_ATT_FCT_MOTIF] <- "FM"	# regular motive for function end
							}
							
							# case where the function starts after the 1st mandate
							else if(data[r,COL_ATT_FCT_DBT]>=election.dates[idx.tests,COL_VERIF_DATE_TOUR2])
							{	new.row[1,COL_ATT_FCT_DBT] <- NA
								new.row[1,COL_ATT_FCT_FIN] <- NA
								if(COL_ATT_FCT_CODE %in% colnames(data))
									new.row[1,COL_ATT_FCT_CODE] <- NA
								if(COL_ATT_FCT_NOM %in% colnames(data))
									new.row[1,COL_ATT_FCT_NOM] <- NA
							}
							# case where the function ends before the 2nd mandate
							else
							{	data[r,COL_ATT_FCT_DBT] <- NA
								data[r,COL_ATT_FCT_FIN] <- NA
								if(COL_ATT_FCT_CODE %in% colnames(data))
									data[r,COL_ATT_FCT_CODE] <- NA
								if(COL_ATT_FCT_NOM %in% colnames(data))
									data[r,COL_ATT_FCT_NOM] <- NA
							}
						}
						
						# log modification
						tlog(8,"After 1: ",format.row.dates(new.row[1,]))
						tlog(8,"After 2: ",format.row.dates(data[r,]))
#						readline() #stop()
					
						# add new row to new data frame
						new.data <- rbind(new.data, new.row)
						
						nbr.splits <- nbr.splits + 1
						split.flag <- TRUE
					}
				}
			}
		}
	}
	tlog(2,"CHECKPOINT 11: Added ",nbr.splits," rows (",(100*nbr.splits/nbr.before),"%) to the table by splitting periods spanning several actual mandates")
	
	data <- rbind(data, new.data)
	tlog(2, "Table now containing ",nrow(data)," rows")
	return(data)
}

	
	
	
#############################################################################################
# Deletes the rows corresponding to micro-mandates, i.e. mandates of only a few days, which
# correspond to errors (or sometimes missing mandates impossible to recover through alternative
# means).
#
# data: original table.
# tolerance: minimal length of a mandate (expressed in days).
#
# return: same table, without the micro-mandates.
#############################################################################################
remove.micro.mandates <- function(data, tolerance)
{	tlog(0,"Removing micro-mandates, for a duration <",tolerance," days")
	nbr.removed <- 0
	nbr.before <- nrow(data)
	
	# compute duration in number of days
	idx <- which(!is.na(data[,COL_ATT_MDT_DBT]) & !is.na(data[,COL_ATT_MDT_FIN]))
	tlog(2,"Found ",length(idx)," rows with both start and end mandate dates")
	if(length(idx)>0)
	{	# compute mandate duration
		durations <- as.integer(data[idx,COL_ATT_MDT_FIN] - data[idx,COL_ATT_MDT_DBT])
		
		# compare to limit
		if(!is.na(tolerance))
			idx <- idx[durations<=tolerance]
		tlog(2,"Found ",length(idx)," mandate(s) which are too short (<=",tolerance," days)")
		
		if(length(idx)>0)
		{	# look for exceptions
			exception.idx <- which(data[idx,COL_ATT_ELU_ID_RNE]=="663"
							& data[idx,COL_ATT_MDT_DBT]==as.Date("2017/9/25") 
							& data[idx,COL_ATT_MDT_FIN]==as.Date("2017/9/30"))
			tlog(4,"Including ",length(exception.idx)," manually marked exceptions")
			
			# log the list of micro-mandates
			tmp <- sapply(1:length(idx), function(i)
			{	tlog(4, "Row ", idx[i], "(",i,"/",length(idx),"): ",format.row.dates(data[idx[i],]),
					if(i %in% exception.idx) " (Exception)" else "")
				tlog(6, format.row(data[idx[i],]))
			})
			
			# get the ids associated to a single micro-mandate
			nms1 <- names(which(table(data[,COL_ATT_ELU_ID])==1))
			nms2 <- unique(data[idx,COL_ATT_ELU_ID])
			nms <- intersect(nms1,nms2)
			tlog(2,"Among them, ",length(nms)," correspond to persons with only this very mandate")
			
			# log the mandates associated to these ids
			if(length(nms)>0)
			{	tmp <- sapply(1:length(nms), function(i)
				{	j <- which(data[,COL_ATT_ELU_ID]==nms[i])
					tlog(4, "Row ", j, "(",i,"/",length(idx),"): ", format.row(data[j,]))
				})
			}
			
			# remove exceptions
			if(length(exception.idx)>0)
				idx <- idx[-exception.idx]
			
			# remove micro-mandates
			if(length(idx)>0)
				data <- data[-idx,]
			nbr.removed <- length(idx)
		}
	}
	
	tlog(2,"CHECKPOINT 9: Removed a total of ",nbr.removed," rows (",(100*nbr.removed/nbr.before),"%) corresponding to micro-mandates")
	tlog(2,"Number of rows remaining: ",nrow(data))
	return(data)
}




#############################################################################################
# For a given position, detects overlapping mandates and solves the problem by shortening the
# older one.
#
# data: original table.
# type: type of mandate (CD, CM, etc.).
# tolerance: maximal overlap (if too long, the overlap is ignored, not solved here).
#
# return: same table, without the (minor) overlaps.
#############################################################################################
shorten.overlapping.mandates <- function(data, type, tolerance=1)
{	tlog(0,"Solving mandate overlaps for unique positions")
	has.fct <- COL_ATT_FCT_DBT %in% colnames(data)
	count <- 0
	
	# some mandate types don't have unique positions
	if((type %in% c("CD","CM","D","M")))
	{	# date columns
		col.start <- COL_ATT_MDT_DBT
		col.end <- COL_ATT_MDT_FIN
		
		# identify all unique position
		tlog(2,"Identifying all unique positions")
		if(type=="CD")
		{	data.pos <- data[,COL_ATT_CANT_ID]
			unique.pos <- sort(unique(data.pos[data[,COL_ATT_CANT_NOM]!="CANTON FICTIF"]))
		}
		else if(type=="CM")
		{	col.start <- COL_ATT_FCT_DBT
			col.end <- COL_ATT_FCT_FIN
			dpts <- data[,COL_ATT_DPT_CODE]
			coms <- data[,COL_ATT_COM_CODE]
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- apply(cbind(dpts,coms,functs),1,function(r) paste(r,collapse="_"))
			idx <- which(is.na(functs))
			unique.pos <- sort(unique(data.pos[-idx]))
		}
		else if(type=="D")
		{	dpts <- data[,COL_ATT_DPT_CODE]
			circos <- data[,COL_ATT_CIRC_CODE]
			data.pos <- apply(cbind(dpts,circos),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos))
		}
		else if(type=="M")
		{	col.start <- COL_ATT_FCT_DBT
			col.end <- COL_ATT_FCT_FIN
			dpts <- data[,COL_ATT_DPT_CODE]
			coms <- data[,COL_ATT_COM_CODE]
			data.pos <- apply(cbind(dpts,coms),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos))
		}
		tlog(4,"Found ",length(unique.pos)," of them")
	
		# process each unique position
		tlog(2,"Processing each unique position")
		for(p in 1:length(unique.pos))
		{	tlog(4,"Processing position ",unique.pos[p], "(",p,"/",length(unique.pos),")")
			
			# get the corresponding rows
			idx <- which(data.pos==unique.pos[p])
			tlog(4,"Found ",length(idx)," rows")
			
			if(length(idx)>1)
			{	# check if their dates overlap
				ccount <- 0
				
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared mandate
					start1 <- data[idx[i],col.start]
					end1 <- data[idx[i],col.end]
					sex1 <- data[idx[i],COL_ATT_ELU_SEXE]
					tlog(6,"Considering row ",i,"/",length(idx),": ",format(start1),"--",format(end1)," (",sex1,")")
					tlog(6, format.row(data[idx[i],]))
					
					# check if there is at least a start date  (for functions, not mandates which must have one)
					if(is.na(start1))
						tlog(8, "Missing start date: can't do anything")
					
					# otherwise, we have a first start date
					else
					{	for(j in (i+1):length(idx))
						{	# get the dates of the second compared mandate
							start2 <- data[idx[j],col.start]
							end2 <- data[idx[j],col.end]
							sex2 <- data[idx[j],COL_ATT_ELU_SEXE]
							tlog(8,"Comparing to row ",j,"/",length(idx),": ",format(start2),"--",format(end2)," (",sex2,")")
							tlog(8, format.row(data[idx[j],]))
							
							# check if there is at least a start date  (for functions, not mandates which must have one)
							if(is.na(start2))
								tlog(8, "Missing start date: can't do anything")
							
							# otherwise, we have a second start date
							else
							{	# check if the periods intersect
								if(date.intersect(start1, end1, start2, end2)
									&& (type=="CM" || type=="D" || type=="M"
											# for CD: unique positions before 2015, but mixed M/F pairs after 2015
											|| (type=="CD" && (get.year(start1)<2015 || get.year(start2)<2015 || sex1==sex2))))
								{	# check if open end date
									if(is.na(end1) && is.na(end2))
										tlog(10,"Overlap with unspecified end dates: cannot solve this issue")
									else
									{	# check if overlap duration small enough
										ovlp.duration <- min(end1-start2, end2-start1, na.rm=TRUE) + 1
										if(ovlp.duration>tolerance)
										{	tlog(10,"Major overlap (above the specified limit of ",tolerance," days)")
											if(start1==start2)
											{	tlog(12,"Start dates match, adjusting the start date associated to the latest end date")
												if(is.na(end1) || !is.na(end2) && end1>end2)
												{	start1 <- end2 + 1
													data[idx[i],col.start] <- start1
													if(col.start==COL_ATT_MDT_DBT && has.fct)
														data[idx[i],COL_ATT_FCT_DBT] <- max(start1,data[idx[i],COL_ATT_FCT_DBT])
													data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the first mandate: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[i],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else if(is.na(end2) || !is.na(end1) && end2>end1)
												{	start2 <- end1 + 1
													data[idx[j],col.start] <- start2
													if(col.start==COL_ATT_MDT_DBT && has.fct)
														data[idx[j],COL_ATT_FCT_DBT] <- max(start2,data[idx[j],COL_ATT_FCT_DBT])
													data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the second mandate: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[j],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else
												{	tlog(14,"Same start and end dates, is that even possible?")
													tlog(14, format.row(data[idx[i],]))
													tlog(14, format.row(data[idx[j],]))
													#stop("Same start and end dates, is that even possible?")
													#readline()						
												}
											}
											else if(is.na(end1) && is.na(end2) || (!is.na(end1) && !is.na(end2) && end1==end2))
											{	tlog(12,"End dates match, adjusting the end date associated to the earliest start date")
												if(start1>start2)
												{	end2 <- start1 - 1
													data[idx[j],col.end] <- end2
													if(col.end==COL_ATT_MDT_FIN && has.fct)
														data[idx[j],COL_ATT_FCT_FIN] <- min(end2,data[idx[j],COL_ATT_FCT_FIN])
													data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the second mandate: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[j],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else if(start2>start1)
												{	end1 <- start2 - 1
													data[idx[i],col.end] <- end1
													if(col.end==COL_ATT_MDT_FIN && has.fct)
														data[idx[i],COL_ATT_FCT_FIN] <- min(end1,data[idx[i],COL_ATT_FCT_FIN])
													data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the first mandate: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[i],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else
												{	tlog(14,"This should not be possible")
													tlog(14, format.row(data[idx[i],]))
													tlog(14, format.row(data[idx[j],]))
													#stop("This should not be possible")
													#readline()
												}
											}
											else
												tlog(12,"End and start dates do not match, cannot solve the issue")
										}
										else
										{	tlog(10,"Minor overlap, correcting the end date of the older mandate")
											# adjust the end of the older mandate to avoid overlap
											if(!is.na(end1) && (is.na(end2) || end1<end2))
											{	end1 <- end1 - ovlp.duration
												data[idx[i],col.end] <- end1
												if(col.end==COL_ATT_MDT_FIN && has.fct)
													data[idx[i],COL_ATT_FCT_FIN] <- min(end1,data[idx[i],COL_ATT_FCT_FIN])
												data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
												tlog(12,"After correction of the first mandate: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
												tlog(12, format.row(data[idx[i],]))
											}
											else
											{	end2 <- end2 - ovlp.duration
												data[idx[j],col.end] <- end2
												if(col.end==COL_ATT_MDT_FIN && has.fct)
													data[idx[j],COL_ATT_FCT_FIN] <- min(end2,data[idx[j],COL_ATT_FCT_FIN])
												data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
												tlog(12,"After correction of the second mandate: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
												tlog(12, format.row(data[idx[j],]))
											}
											# count the problematic cases
											count <- count + 1
											ccount <- ccount + 1
										}
									}
								}
							}
						}
					}
				}
				tlog(6,"Corrected ",ccount," overlaps for this specific position")
			}
		}
		tlog(4,"Processing over")
		
		tlog(2,"CHECKPOINT 12: Shortened a total of ",count," mandates due to self-overlap, for the whole table (",(100*count/nrow(data)),"%)")
		tlog(2, "Number of rows remaining: ",nrow(data))
	}
	
	return(data)
}




#############################################################################################
# Removes the mandate and function motives associated to missing en dates.
#
# data: table to process.
#
# returns: same table, but after the correction.
#############################################################################################
delete.superfluous.motives <- function(data)
{	tlog(0,"Removing end motives associated to no end date")
	
	# correct mandate motives
	idx <- which(is.na(data[,COL_ATT_MDT_FIN]) & !is.na(data[,COL_ATT_MDT_MOTIF]))
	tlog(2,"Found ",length(idx)," superfluous mandate end motives")
	if(length(idx)>0)
		data[idx,COL_ATT_MDT_MOTIF] <- NA
	
	# possibly correct function motives
	if(COL_ATT_FCT_MOTIF %in% colnames(data))
	{	idx.fct <- which(is.na(data[,COL_ATT_FCT_FIN]) & !is.na(data[,COL_ATT_FCT_MOTIF]))
		tlog(2,"Found ",length(idx.fct)," superfluous function end motives")
		if(length(idx.fct)>0)
			data[idx.fct,COL_ATT_FCT_MOTIF] <- NA
		idx <- sort(union(idx, idx.fct))
	}
	
	# log changes
	tlog(2,"List of corrected rows: ")
	for(i in idx)
		tlog(4, format.row(data[i,]))
	
	tlog(2,"CHECKPOINT 14: emptied a total of ",length(idx)," superfluous motives, for the whole table (",(100*length(idx)/nrow(data)),"%)")
	tlog(2, "Number of rows remaining: ",nrow(data))
	#readline()
	
	return(data)
}




#############################################################################################
# Performs various corrections on the dates defining mandates and functions: 
# 1) Adjusts function dates so that they are contained inside the corresponding mandate period.
# 2) Rounds mandate and function dates to match election dates, when approximately equal.
# 3) Removes mandates deemed too short.
# 4) Merges rows corresponding to overlapping compatible mandates.
# 5) Splits rows spanning election dates.
# 6) Shorten overlapping rows (for a given position).
# 7) Removes short mandates (again).
# 8) Fix motives (for the end of mandates/functions).
# 
# data: the data table.
# election.file: name of the file containing the election dates.
# series.file: name of the file containing the series (optional, depends on the type of mandate).
# type: type of mandate (CD, CM, etc.).
#
# returns: same table, but with corrected dates.
#############################################################################################
fix.mdtfct.dates <- function(data, election.file, series.file, type)
{	# adjust function dates so that they are contained inside the corresponding mandate period
	data <- adjust.function.dates(data)
	
	# round mandate and function dates to match election dates, when approximately equal
	if(hasArg(election.file))
		data <- round.mdtfct.dates(data, election.file, series.file, tolerance=7)
	
	# removes micro-mandates
	data <- remove.micro.mandates(data, tolerance=7)
	
	# merge rows corresponding to overlapping and compatible mandates
	data <- merge.overlapping.mandates(data, type)
	
	# splits rows containing election dates (other than as a start date)
	if(hasArg(election.file))
		data <- split.long.mandates(data, election.file, series.file)
	
	# solve mandate intersections (same position)
	data <- shorten.overlapping.mandates(data, type, tolerance=8)
	
	# remove micro-mandates again (in case split created any)
	data <- remove.micro.mandates(data, tolerance=7)
	
	# delete superfluous motives
	data <- delete.superfluous.motives(data)
	
#	stop()
	return(data)
}
