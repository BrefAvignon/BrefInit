#############################################################################################
# Functions used to load each table.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Loads the specified tables, binds them is there are several ones, convert certain values
# to appropriate types (dates, integers), and returns the resulting data frame.
#
# filenames: list of files to read.
# col.map: how to convert column names.
# correc.file: file containing the corrections.
# equiv.ids.file: file containing the map of equivalent ids.
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the files.
#############################################################################################
load.data <- function(filenames, col.map, correc.file, equiv.ids.file, correct.data)
{	plan(multiprocess, workers=CORE.NBR/2)
	
	# load the corrections
	tlog(0,"Loading correction file \"",correc.file,"\"")
	correc.table <- read.table(
			file=correc.file,			# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE					# don't convert strings to factors
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	
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
	
	# load the data table(s)
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
#				fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		tlog(2,"Read ",nrow(temp)," lines and ",ncol(temp)," columns")
		
		# add to the main table
		tlog(0,"Adding to main table")
		if(all(is.null(data)))
			data <- temp
		else
			data <- rbind(data,temp)
		tlog(2,"Now ",nrow(data)," lines and ",ncol(data)," columns in main table")
	}
	tlog(2,"Columns: ",paste(colnames(data),collapse=","))

	# normalize data table column names
	norm.names <- col.map[colnames(data)]
	if(any(is.na(norm.names)) || length(norm.names)!=ncol(data))
		stop("Problem with the number of columns (or their names) when loading the table, after normalization")
	else
		colnames(data) <- norm.names
	
	# setting appropriate encoding of string columns,
	# replace "" by NAs, and normalize proper nouns
	tlog(0,"Cleaning/encoding/normalizing strings")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		
		# the column is an actual string
		if(col.type %in% c("cat","nom"))
		{	tlog(2,"Processing column \"",col.name,"\"")
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
	
	if(correct.data)
	{	# load table of equivalent ids
		tlog(0,"Loading the table of equivalent ids (",equiv.ids.file,")")
		equiv.table <- read.table(
				file=equiv.ids.file,		# name of the equivalence file
				header=TRUE, 				# look for a header
				sep="\t", 					# character used to separate columns 
				check.names=FALSE, 			# don't change the column names from the file
				comment.char="", 			# ignore possible comments in the content
				row.names=NULL, 			# don't look for row names in the file
				quote="", 					# don't expect double quotes "..." around text fields
				as.is=TRUE,					# don't convert strings to factors
				colClasses="character"		# all column originally read as characters
#				fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		
		# fix dupplicate ids
		if(nrow(equiv.table)>0)
		{	# convert to map
			tlog(0,"Convert table of equivalent ids to map")
			unique.ids <- unique(data[,COL_ATT_ELU_ID])
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
				data[,COL_ATT_ELU_ID] <- future_sapply(data[,COL_ATT_ELU_ID], function(id)
						{	new.id <- conv.map[id]
							if(is.na(new.id))
								return(id)
							else
								return(new.id)
						})
			}
		}
		
		# apply ad hoc corrections
		if(nrow(correc.table)>0)	
		{	tlog(0,"Applying ad hoc corrections")
			
			# apply each correction one after the other
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
					idx <- which(data[,COL_ATT_ELU_ID]==correc.table[r,COL_CORREC_ID]
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
							{	tlog(4,"Correcting entry: ",paste(correc.table[r,],collapse=";"))
								data[row,correc.table[r,COL_CORREC_ATTR]] <- correc.table[r,COL_CORREC_VALAPR]
							}
						}
					}
					else
					{	if(!is.na(row) && row!=idx)
						{	tlog(4,"The specified row (",row,") does not correspond to the one matching the criteria (",idx,")")
							stop(paste0("The specified row (",row,") does not correspond to the one matching the criteria (",idx,")"))
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
		
		# possibly normalize municipality ids
		if(COL_ATT_COM_CODE %in% colnames(data))
		{	# we simply keep the first three characters of the id
			data[,COL_ATT_COM_CODE] <- substr(x=data[,COL_ATT_COM_CODE], start=1, stop=3)
		}
		
		# replace the NC political nuance by proper NAs
		if(COL_ATT_ELU_NUANCE %in% colnames(data))
		{	idx <- which(data[,COL_ATT_ELU_NUANCE]=="NC")
			data[idx,COL_ATT_ELU_NUANCE] <- NA
		}
	}
	
	# convert date and numeric columns
	tlog(0,"Converting date and numeric columns")
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
#		}
		
		# the other columns stay strings
		else
			tlog(2,"Col. \"",col.name,"\": simple string, no conversion")
	}
		
	# normalize columns order
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when loading the table, after reordering")
	else
		data <- data[,norm.cols]
	
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
# comp.cols: columns whose values must be identical.
# out.folder: where to write the procuded file (if NA, no file is produced).
#
# returns: the table with the merged similar rows.
#############################################################################################
merge.similar.rows <- function(data, comp.cols, out.folder=NA)
{	tlog(0,"Merging compatible rows for compulsory columns \"",paste(comp.cols,collapse="\",\""),"\"")
	plan(multiprocess, workers=CORE.NBR/2)
	rm.col <- which(colnames(data) %in% comp.cols)
	
	# identify redundant rows
	concat <- apply(data[,comp.cols], 1, function(row) paste(row, collapse=":"))
	tt <- table(concat)
	codes <- names(tt)[which(tt>1)]
	tlog(2,"Looking for redundant rows: found ",length(codes))
	
	# identify compatible rows amongst redundant ones
	tlog(2,"Looking for compatible rows among them")
	mats <- future_lapply(codes, function(code)
			{	res <- matrix(nrow=0,ncol=ncol(data))
				rs <- which(concat==code)
				for(r1 in 1:(length(rs)-1))
				{	row1 <- data[r1, -rm.col]
					for(r2 in (r1+1):length(rs))
					{	row2 <- data[r2, -rm.col]
						if(all(is.na(row1) | is.na(row2) | row1==row2))
							res <- rbind(res, data[r1,], data[r2,], rep(NA,ncol(data)))
					}
				}
				if(nrow(res)>0)
					res <- rbind(res, rep(NA,ncol(data)))
				return(res)
			})
	
	# merge the list of tables and record them
	tlog(2,"Merging the ",length(mats)," tables")
	tab <- data[-(1:nrow(data)),]
	for(mat in mats)
		tab <- rbind(tab,mat)
	if(is.na(out.folder) && nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"compatible_rows.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, col.names=TRUE)
	}
	
	tlog(2,"Done merging compatible rows")
	return(data)
}




#############################################################################################
# Inserts a new column in the table, that contains the unique id associated to each canton.
# Otherwise, their code is not unique, there is no match between codes and names.
#
# data: the data table.
# file.canton.ids: file containing the unique ids.
#
# returns: the table with the extra column.
#############################################################################################
insert.unique.canton.id <- function(data, file.canton.ids)
{	# load table of canton ids
	tlog(0,"Loading the table of canton ids (",file.canton.ids,")")
	equiv.table <- read.table(
			file=FILE_CANTON_IDS,		# name of the id file
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
	
	# insert in the table, at the correct place
	col <- which(colnames(data)==COL_ATT_CANT_CODE)
	data <- cbind(data[,1:(col-1)], equiv.table[idx,COL_ATT_CANT_ID], data[,col:ncol(data)])
	colnames(data)[col] <- COL_ATT_CANT_ID
	
	return(data)
}
#############################################################################################
# Loads the table for departmental counsilors (first extraction).
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd.data <- function(correct.data)
{	# names of the columns
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
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
	col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
	col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
	col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
	col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
	col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
	col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
	
	# load the data
	data <- load.data(filenames=FILES_TAB_CD, col.map=col.map, correc.file=FILE_CORREC_CD, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# perform additional corrections
	if(correct.data)
	{	# add a unique id for cantons
		data <- insert.unique.canton.id(data=data, file.canton.ids=FILE_CANTON_IDS)
		
		# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_CD)
	}
	
	return(data)
}
#############################################################################################
# Loads the table for departmental counsilors (second extraction).
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd2.data <- function(correct.data)
{	# names of the columns
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
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
	col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
	col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
	col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
	col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
	col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
	
	# load the data
	data <- load.data(filenames=FILES_TAB_CD2, col.map=col.map, correc.file=FILE_CORREC_CD2, equiv.ids.file=FILE_EQUIV_IDS2, correct.data) 
	
	# perform additional corrections
	if(correct.data)
	{	# add a unique id for cantons
		data <- insert.unique.canton.id(data=data, file.canton.ids=FILE_CANTON_IDS)
		
		# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_CD2)
	}
	
	return(data)
}




#############################################################################################
# Loads the tables for municipal counsilors (first extraction).
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["Code du département (Maire)"] <- COL_ATT_DPT_CODE
	col.map["Libellé de département (Maires)"] <- COL_ATT_DPT_NOM
	col.map["Code Insee de la commune"] <- COL_ATT_COM_CODE
	col.map["Libellé de la commune"] <- COL_ATT_COM_NOM
	col.map["Population de la commune"] <- COL_ATT_COM_POP
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Code sexe"] <- COL_ATT_ELU_SEXE
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_CM, col.map=col.map, correc.file=FILE_CORREC_CM, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# convert population numbers to actual integers
	tlog(0,"Converting population to integer values")
	cn <- COL_ATT_COM_POP
	vals <- data[,cn]
#	vals <- suppressWarnings(as.integer(vals))
	vals <- as.integer(vals)
	data <- data[, names(data)!=cn]
	data <- cbind(data,vals)
	names(data)[ncol(data)] <- cn
	
	# add mandate name
	vals <- rep("CONSEILLER MUNICIPAL",nrow(data))
	data <- cbind(data, vals)
	colnames(data)[ncol(data)] <- COL_ATT_MDT_NOM
	
	# normalize columns order
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when reordering table CM columns")
	else
		data <- data[,norm.cols]
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_CM)
	}
	
	return(data)
}




#############################################################################################
# Loads the tables for municipal counsilors (second extraction).
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm2.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	data <- load.data(filenames=FILES_TAB_CM2, col.map=col.map, correc.file=FILE_CORREC_CM2, equiv.ids.file=FILE_EQUIV_IDS2, correct.data)
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_CM2)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for regional counsilors (first extraction).
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["Code région"] <- COL_ATT_REG_CODE
	col.map["Libellé de la région"] <- COL_ATT_REG_NOM
	col.map["Code du département"] <- COL_ATT_DPT_CODE
	col.map["Libellé de département"] <- COL_ATT_DPT_NOM
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Code sexe"] <- COL_ATT_ELU_SEXE
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_CR, col.map=col.map, correc.file=FILE_CORREC_CR, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_CR)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for regional counsilors (first extraction).
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr2.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["Région"] <- COL_ATT_REG_NOM
	col.map["Circonscription électorale"] <- COL_ATT_DPT_CODE
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	data <- load.data(filenames=FILES_TAB_CR2, col.map=col.map, correc.file=FILE_CORREC_CR2, equiv.ids.file=FILE_EQUIV_IDS2, correct.data)
	
	# split region name column
	reg.code <- sapply(data[,COL_ATT_REG_NOM], function(x) substr(x,1,2))
	reg.name <- sapply(data[,COL_ATT_REG_NOM], function(x) substr(x,3,nchar(x)))
	data[,COL_ATT_REG_NOM] <- reg.name
	data[,COL_ATT_REG_CODE] <- reg.code
	
	# normalize columns order
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when reordering table CR2 columns")
	else
		data <- data[,norm.cols]
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_CR2)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for members of the parliament.
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.d.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["Code du département"] <- COL_ATT_DPT_CODE
	col.map["Libellé du département"] <- COL_ATT_DPT_NOM
	col.map["Code de la cir.législative"] <- COL_ATT_CIRC_CODE
	col.map["Libellé de la cir.législative"] <- COL_ATT_CIRC_NOM
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_D, col.map=col.map, correc.file=FILE_CORREC_D, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_D)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for European counsilors.
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.de.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["CodeCirER"] <- COL_ATT_CIRCE_CODE
	col.map["LibelléCirER"] <- COL_ATT_CIRCE_NOM
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
	col.map["Code sexe"] <- COL_ATT_ELU_SEXE
	col.map["Code profession"] <- COL_ATT_PRO_CODE
	col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
	col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
	col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
	col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
	col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
	col.map["Nuance politique (Rep. P.E.)"] <- COL_ATT_ELU_NUANCE
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_DE, col.map=col.map, correc.file=FILE_CORREC_DE, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_DE)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for EPCI counsilors.
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.epci.data <- function(correct.data)
{	# names of the columns
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
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_EPCI, col.map=col.map, correc.file=FILE_CORREC_EPCI, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# add mandate name
	vals <- rep("CONSEILLER EPCI",nrow(data))
	data <- cbind(data, vals)
	colnames(data)[ncol(data)] <- COL_ATT_MDT_NOM
	
	# normalize columns order
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when reordering table EPCI columns")
	else
		data <- data[,norm.cols]
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_EPCI)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for mayors.
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.m.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["Code du département (Maire)"] <- COL_ATT_DPT_CODE
	col.map["Libellé de département (Maires)"] <- COL_ATT_DPT_NOM
	col.map["Code Insee de la commune"] <- COL_ATT_COM_CODE
	col.map["Libellé de la commune"] <- COL_ATT_COM_NOM
	col.map["Population de la commune"] <- COL_ATT_COM_POP
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_M, col.map=col.map, correc.file=FILE_CORREC_M, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# convert population numbers to actual integers
	tlog(0,"Converting population to integer values")
	cn <- COL_ATT_COM_POP
	vals <- data[,cn]
	vals <- gsub(" ", "",  vals)	# \\s matches all whitespaces
	vals <- gsub(",00", "",  vals)
	vals <- suppressWarnings(as.integer(vals))
	data <- data[, names(data)!=cn]
	data <- cbind(data,vals)
	names(data)[ncol(data)] <- cn
	
	# normalize columns order
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when reordering table M columns")
	else
		data <- data[,norm.cols]
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_M)
	}
	
	return(data)
}




#############################################################################################
# Loads the table for senators.
#
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.s.data <- function(correct.data)
{	# names of the columns
	col.map <- c()
	col.map["Code du département"] <- COL_ATT_DPT_CODE
	col.map["Libellé du département"] <- COL_ATT_DPT_NOM
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Date de naissance"] <- COL_ATT_ELU_DDN
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
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID
	
	# load the data
	data <- load.data(filenames=FILES_TAB_S, col.map=col.map, correc.file=FILE_CORREC_S, equiv.ids.file=FILE_EQUIV_IDS, correct.data)
	
	# perform additional corrections
	if(correct.data)
	{	# merge similar rows
		comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
		data <- merge.similar.rows(data=data, comp.cols=comp.cols, out.folder=FOLDER_OUT_S)
	}
	
	return(data)
}
