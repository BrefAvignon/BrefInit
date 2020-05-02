#############################################################################################
# Functions used to correct the RNE data.
# 
# 03/2020 Vincent Labatut
#
# source("src/load/apply_corrections.R")
#############################################################################################




#############################################################################################
# Loads the raw RNE data as string, normalizes and performs basic cleaning, then returns a proper
# data frame. The column names are also normalized.
#
# filenames: list of files to read.
# col.map: how to convert column names.
# correct.data: whether or not to apply the correction on the data read.
#
# returns: data frame containing only (clean) strings.
#############################################################################################
retrieve.normalize.data <- function(filenames, col.map, correct.data)
{	corr.idx <- c()
	
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
	bef.nrows <- nrow(data)
	
	# normalize data table column names
	if(hasArg(col.map))
	{	norm.names <- col.map[colnames(data)]
		if(any(is.na(norm.names)) || length(norm.names)!=ncol(data))
			stop("Problem with the number of columns (or their names) when loading the table, after normalization")
		else
			colnames(data) <- norm.names
	}
	
	# EPCI-specific cleaning
	if(correct.data && COL_ATT_EPCI_NOM %in% colnames(data))
	{	# clean CC names (must be done before normalization)
		idx <- which(grepl("archiv",data[,COL_ATT_EPCI_NOM],fixed=TRUE))
		data[,COL_ATT_EPCI_NOM] <- gsub(pattern=" (archivé)",replacement="",x=data[,COL_ATT_EPCI_NOM],fixed=TRUE)
		data[,COL_ATT_EPCI_NOM] <- gsub(pattern=" - archivé",replacement="",x=data[,COL_ATT_EPCI_NOM],fixed=TRUE)
		# log
		tlog(0,"EPCI-specific cleaning: ",length(idx)," names")
		corr.idx <- union(corr.idx, idx)
	}
	# municipality-specific cleaning
	if(correct.data && COL_ATT_COM_NOM %in% colnames(data))
	{	# clean municipality names (must be done before normalization)
		idx <- which(grepl("archiv",data[,COL_ATT_COM_NOM],fixed=TRUE))
		data[,COL_ATT_COM_NOM] <- gsub(pattern=" (archivée)",replacement="",x=data[,COL_ATT_COM_NOM],fixed=TRUE)
		# log
		tlog(0,"CM- and M-specific cleaning: ",length(idx)," names")
		corr.idx <- union(corr.idx, idx)
	}
	
	# setting appropriate encoding of string columns, replace "" by NAs, and normalize proper nouns
	tlog.start.loop(0,ncol(data),"Cleaning/encoding/normalizing strings")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		tlog.loop(2,c,"Processing column \"",col.name,"\" (",c,"/",ncol(data),")")
		
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
	tlog.end.loop(0,"Done with the cleaning/encoding/normalizing of strings")
	
	# add columns to store correction flags
	correc.date <- rep(FALSE, nrow(data))
	correc.info <- rep(FALSE, nrow(data))
	correc.info[corr.idx] <- TRUE
	data <- cbind(data, correc.date, correc.info)
	colnames(data)[(ncol(data)-1):ncol(data)] <- c(COL_ATT_CORREC_DATE, COL_ATT_CORREC_INFO) 
	
	tlog(2,"CHECKPOINT 0: performed ",length(corr.idx)," corrections")
	tlog(4,"Now ",nrow(data)," rows and ",ncol(data)," columns in main table")
	update.stat.table(s.nbr=0, s.name="Load data", del.nbr=0, mod.nbr=length(corr.idx), add.nbr=0, size=bef.nrows)
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
		tlog(0,"Converting table of equivalent ids to map")
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
			update.stat.table(s.nbr=1, s.name="Fix IDs", del.nbr=0, mod.nbr=length(corrected.ids), add.nbr=0, size=nrow(data))
		}
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	}
	
	# debug
#	tlog(0,"Comparing personal info of equivalent ids")
#	for(i in 1:length(conv.map))
#	{	id1 <- names(conv.map)[i]
#		id2 <- conv.map[i]
#		r1 <- which(data[,COL_ATT_ELU_ID_RNE]==id1)[1]
#		r2 <- which(data[,COL_ATT_ELU_ID_RNE]==id2)[1]
#		
#		lastname1 <- data[r1,COL_ATT_ELU_NOM]
#		lastname2 <- data[r2,COL_ATT_ELU_NOM]
#		firstname1 <- data[r1,COL_ATT_ELU_PRENOM]
#		firstname2 <- data[r2,COL_ATT_ELU_PRENOM]
#		birthdate1 <- data[r1,COL_ATT_ELU_NAIS_DATE]
#		birthdate2 <- data[r2,COL_ATT_ELU_NAIS_DATE]
#		
#		if(lastname1!=lastname2 || firstname1!=firstname2 || birthdate1!=birthdate2)
#		{	tlog(2, "Ids ",names(conv.map)[i]," and ",conv.map[i],": ")
#			if(lastname1!=lastname2)
#				tlog(4,"Different lastnames: ",lastname1," vs. ",lastname2)
#			if(firstname1!=firstname2)
#				tlog(4,"Different firstnames: ",firstname1," vs. ",firstname2)
#			if(birthdate1!=birthdate2)
#				tlog(4,"Different birthdates: ",format(birthdate1)," vs. ",format(birthdate2))
#		}
#	}
	
	# debug: merge two tables of equivalent ids
#	head(main.tab)
#	head(sec.tab)
#	colnames(sec.tab) <- colnames(main.tab)
#	# check names
#	names1 <- data0[match(sec.tab[,1],data0[,COL_ATT_ELU_ID_RNE]),COL_ATT_ELU_NOM]
#	names2 <- data0[match(sec.tab[,2],data0[,COL_ATT_ELU_ID_RNE]),COL_ATT_ELU_NOM]
#	print(cbind(names1,names2))
#	# check ids common to both tables
#	map <- match(sec.tab[,1],main.tab[,1])
#	com.ids <- which(!is.na(map))
#	div.ids <- com.ids[sec.tab[com.ids,2]!=main.tab[map[com.ids],2]]
#	print(cbind(sec.tab[div.ids,],main.tab[map[div.ids],2]))
#	# compare with verified homonyms
#	map2 <- match(sec.tab[,1], homon.table[,1])
#	com.ids1 <- which(!is.na(map2))
#	print(length(com.ids1))
#	# add ids present only in secundary table
#	diff.ids <- which(is.na(map))
#	main.tab <- rbind(main.tab, sec.tab[diff.ids,])
#	main.tab <- main.tab[order(as.integer(main.tab[,1])),]
#	# record file
#	write.table(x=main.tab, file=file.path(FOLDER_LOG,"new_equiv_map.txt"), quote=F, sep="\t", row.names=F, col.names=F)
	
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
{	nbr.rows <- nrow(data)
	# load the correction table
	correc.table <- load.correction.table(col.map, correc.file)
	#print(colnames(data))	
	
	# apply ad hoc corrections
	corrected.rows <- c()
	if(nrow(correc.table)>0)	
	{	# apply each correction one after the other
		tlog.start.loop(0,nrow(correc.table),"Applying ad hoc corrections")
		idx.rm <- c()
		for(r in 1:nrow(correc.table))
		{	tlog.loop(2,r,"Correction ",r,"/",nrow(correc.table))
			correc.attr <- correc.table[r,COL_CORREC_ATTR]
			
			# check that the column exists
			if(!(correc.attr %in% colnames(data)))
			{	if(correc.attr %in% c(COL_ATT_ELU_ID_EURO))
				{	data <- cbind(data, rep(NA,nrow(data)))
					colnames(data)[ncol(data)] <- COL_ATT_ELU_ID_EURO
				}
				else
					stop("Could not find column ",correc.attr)
			}
			
			# identify the corresponding correction flag
			if(correc.attr %in% c(COL_ATT_MDT_DBT, COL_ATT_MDT_FIN, COL_ATT_FCT_DBT, COL_ATT_FCT_FIN))
				correc.col <- COL_ATT_CORREC_DATE
			else
				correc.col <- COL_ATT_CORREC_INFO
			
			# get the targed row
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
							& (data[,COL_ATT_ELU_PRENOM]==correc.table[r,COL_CORREC_PRENOM]
								| is.na(data[,COL_ATT_ELU_PRENOM]) & is.na(correc.table[r,COL_CORREC_PRENOM]))
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
						else
						{	tlog(4,"The specified row (",row,") does not correspond to any of the matching rows (",paste(idx,collapse=","),")")
							stop("The specified row (",row,") does not correspond to any of the matching rows (",paste(idx,collapse=","),")")
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
		tlog.end.loop(2,"Corrected ",length(corrected.rows)," rows in total (",(100*length(corrected.rows)/nrow(data)),"%)")
		
		# remove the marked rows
		if(length(idx.rm)>0)
		{	tlog(2,"Removing ",length(idx.rm)," rows from the table (",(100*length(idx.rm)/nrow(data)),"%)")
			data <- data[-idx.rm,]
		}
		
		total <- length(corrected.rows)+length(idx.rm)
		tlog(2,"CHECKPOINT 2: corrected/removed ",total," rows (",(100*total/nbr.rows),"%)")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
		update.stat.table(s.nbr=2, s.name="Apply ad hoc corrections", del.nbr=length(idx.rm), mod.nbr=length(corrected.rows), add.nbr=0, size=nbr.rows)
	}
	
	# debug
#	idx <- which(is.na(data[,COL_ATT_ELU_NAIS_DATE]))
#	print(data[idx,])
	
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
# type: type of mandate (CD, CM, etc.).
#
# returns: data frame after the corrections.
#############################################################################################
apply.systematic.corrections <- function(data, type)
{	corr.rows <- c()
	# normalise usage names
	tlog(0,"Normalizing usage names")
	old.names <- data[,COL_ATT_ELU_NOM]
	idx <- which(grepl(pattern="(.+)( EP | EPOUSE )(.+)", x=data[,COL_ATT_ELU_NOM]))
	if(length(idx)>0)
	{	tlog(0,"Found ",length(idx)," spouse names")
		# update names explicitly containing "spouse" 
		usage.names <- gsub(x=data[idx,COL_ATT_ELU_NOM], pattern="(.+)( EP | EPOUSE )(.+)",replacement="\\1")
		birth.names <- gsub(x=data[idx,COL_ATT_ELU_NOM], pattern="(.+)( EP | EPOUSE )(.+)",replacement="\\3")
		new.names <- gsub(x=data[idx,COL_ATT_ELU_NOM], pattern="(.+)( EP | EPOUSE )(.+)",replacement="\\3 \\1")
		#head(cbind(old.names[idx], birth.names, new.names))
		data[idx,COL_ATT_ELU_NOM] <- new.names
		tmp <- matrix(ncol=2,nrow=0)
		# possibly update other names
		tlog.start.loop(2,length(idx),"Processing each name")
		for(r in 1:length(idx))
		{	tlog.loop(4,r,"Processing name ",data[idx[r],COL_ATT_ELU_NOM]," (",r,"/",length(idx),")")
			idx2 <- which(data[,COL_ATT_ELU_PRENOM]==data[idx[r],COL_ATT_ELU_PRENOM]				# same first name
						& data[,COL_ATT_ELU_NAIS_DATE]==data[idx[r],COL_ATT_ELU_NAIS_DATE]			# same birthdate
						& data[,COL_ATT_ELU_NOM]!=data[idx[r],COL_ATT_ELU_NOM]						# different last names
						& (grepl(pattern=birth.names[r], x=data[,COL_ATT_ELU_NOM], fixed=TRUE)		# but includes birth or usage name
							| grepl(pattern=usage.names[r], x=data[,COL_ATT_ELU_NOM], fixed=TRUE)))
			if(length(idx2)>0)
			{	data[idx2,COL_ATT_ELU_NOM] <- new.names[r]
				tlog(6, "Updating other rows based on the first below")
				tlog(8, format.row(data[idx[r],]))
				for(r2 in idx2)
					tlog(6, format.row(data[r2,]))
				ids <- union(data[idx[r],COL_ATT_ELU_ID_RNE], data[idx2,COL_ATT_ELU_ID_RNE])
				if(length(ids)>1)
				{	tlog(6, "WARNING: consider merging RNE ids ",paste(ids,collapse=","))
					ids <- sort(as.integer(ids))
					row <- c(ids[1],paste(ids[-1],collapse=","))
					tmp <- rbind(tmp, row)
				}
			}
		}
		tlog.end.loop(2,"Loop over")
		tmp <- tmp[order(tmp[,1]),]
		print(tmp)		# debug
		# log result
		idx <- which(old.names!=data[,COL_ATT_ELU_NOM])
		data[idx,COL_ATT_CORREC_INFO] <- TRUE 
		corr.rows <- union(corr.rows,idx)
		tlog(2,"Normalized ",length(idx)," last names")
	}
	# possibly normalize municipality ids
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
		# replace the NC political nuance by proper NAs
		idx <- which(data[,COL_ATT_ELU_NUANCE]=="NC")
		if(length(idx)>0)
		{	data[idx,COL_ATT_ELU_NUANCE] <- NA
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		# replace the duplicate labels
		dupl.nuances <- c("RDG"="PRG", "M-NC"="MAJ")
		for(i in 1:length(dupl.nuances))
		{	idx <- which(data[,COL_ATT_ELU_NUANCE]==names(dupl.nuances)[i])
			if(length(idx)>0)
			{	data[idx,COL_ATT_ELU_NUANCE] <- dupl.nuances[i]
				data[idx,COL_ATT_CORREC_INFO] <- TRUE 
				corr.rows <- union(corr.rows,idx)
			}
		}
		tlog(2,"Fixed ",length(idx)," political nuances")
	}
	# EPCI-specific cleaning
	if(type=="EPCI")
	{	# clean missing department codes
		tlog(0,"Cleaning missing EPCI department codes")
		idx <- which(data[,COL_ATT_DPT_CODE]=="0" | data[,COL_ATT_DPT_CODE]=="00")
		if(length(idx)>0)
		{	data[idx,COL_ATT_DPT_CODE] <- NA
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Cleaned ",length(idx)," rows")
		
		# clean missing municipality codes
		tlog(0,"Cleaning missing EPCI municipality codes")
		idx <- which(data[,COL_ATT_COM_CODE]=="0" | data[,COL_ATT_COM_CODE]=="00")
		if(length(idx)>0)
		{	data[idx,COL_ATT_COM_CODE] <- NA
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Cleaned ",length(idx)," rows")
		
		# complete missing municipality names (when municipality and department codes areavailable)
		tlog(0,"Completing missing EPCI municipality names")
		idx <- which(is.na(data[,COL_ATT_COM_NOM]) & !is.na(data[,COL_ATT_COM_CODE]) & !is.na(data[,COL_ATT_DPT_CODE]))
		if(length(idx)>0)
		{	# list full codes and associated names
			locs <- cbind(apply(cbind(data[,COL_ATT_DPT_CODE],data[,COL_ATT_COM_CODE]),1,function(r) paste(r,collapse="_")),data[,COL_ATT_COM_NOM])
			# match missing codes to this list
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
		tlog(2,"Completed municipality names in ",length(idx)," rows")
		
		# fix missing info using CM whenever possible
		tlog(0,"Completing missing EPCI data using CM")
		idx <- which(is.na(data[,COL_ATT_DPT_CODE]) | is.na(data[,COL_ATT_COM_CODE]) | is.na(data[,COL_ATT_COM_NOM]))
		if(length(idx)>0)
		{	idx0 <- c()
			cm.data <- load.cm.data(correct.data=TRUE, complete.data=TRUE)
			# for each problematic row in EPCI
			for(j in 1:length(idx))
			{	r <- idx[j]
				if(j %% 1000 == 0)
					tlog(2,"Computing row ",j,"/",length(idx))
				
				# get rows with matching ids in CM
				idx2 <- which(cm.data[,COL_ATT_ELU_ID_RNE]==data[r,COL_ATT_ELU_ID_RNE])
				if(length(idx2)==0)
				{	# should not happen at all
					#tlog(2,"PROBLEM: did not find EPCI person ",data[r,COL_ATT_ELU_ID_RNE]," in CM (",j,"/",length(idx),")")
					#candidates <- which(cm.data[,COL_ATT_ELU_NOM]==data[r,COL_ATT_ELU_NOM] & cm.data[,COL_ATT_ELU_PRENOM]==data[r,COL_ATT_ELU_PRENOM] & cm.data[,COL_ATT_ELU_NAIS_DATE]==data[r,COL_ATT_ELU_NAIS_DATE])
					#if(length(candidates)==0)
					#	candidates <- which(cm.data[,COL_ATT_ELU_NOM]==data[r,COL_ATT_ELU_NOM] & cm.data[,COL_ATT_ELU_PRENOM]==data[r,COL_ATT_ELU_PRENOM])
					#cand.idx <- cm.data[candidates,COL_ATT_ELU_ID_RNE]
					#tlog(2,data[r,COL_ATT_ELU_ID_RNE]," : ",paste(cand.idx,colapse=","))
				}
				# look for a matching mandate amongst them
				else
				{	match.idx <- NA
					i <- 1
					while(is.na(match.idx) && i<=length(idx2))
					{	r2 <- idx2[i]
						if(date.intersect(start1=as.Date(data[r,COL_ATT_MDT_DBT], "%d/%m/%Y"), 
								end1=as.Date(data[r,COL_ATT_MDT_FIN], "%d/%m/%Y"), 
								start2=cm.data[r2,COL_ATT_MDT_DBT], 
								end2=cm.data[r2,COL_ATT_MDT_FIN]))
							match.idx <- r2
						i <- i + 1
					}
					# if found one, make the correction
					if(is.na(match.idx))
					{	match.idx <- idx2[1]
						#stop("PROBLEM: did not find EPCI mandate in CM (",j,"/",length(idx),")")
					}
					for(col in c(COL_ATT_DPT_CODE, COL_ATT_COM_CODE, COL_ATT_COM_NOM))
					{	if(is.na(data[r,col]))
							data[r,col] <- cm.data[match.idx,col]
					}
					# update list of corrected rows
					idx0 <- c(idx0, r)
				}
			}
			idx <- idx0
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Completed ",length(idx)," missing EPCI municipality-related values")
		
		# fix typo in function name
		tlog(0,"Correcting EPCI function names")
		idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) & data[,COL_ATT_FCT_NOM]=="PRESIDENT")
		if(length(idx)>0)
		{	data[idx,COL_ATT_FCT_NOM] <- "PRESIDENT D EPCI"
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			corr.rows <- union(corr.rows,idx)
		}
		tlog(2,"Fixed ",length(idx)," rows (function)")
	}
	# M-specific cleaning
	if(type=="M")
	{	tlog(0,"Align dates that are systematically wrong (by a few days) for certain departments and elections in M") # NOTE this is very ad hoc
		#
		idx <- which(data[,COL_ATT_DPT_NOM]=="CREUSE" & data[,COL_ATT_MDT_DBT]==as.Date("3/3/2001"))
		if(length(idx)>0)
		{	data[idx,COL_ATT_MDT_DBT] <- "11/3/2001"
			data[idx,COL_ATT_CORREC_DATE] <- TRUE 
			tlog(2,"Fixed ",length(idx)," rows for CREUSE 03/03/2001--")
			corr.rows <- union(corr.rows,idx)
		}
	}
	# D-specific cleaning
	if(type=="D")
	{	tlog(0,"Fix a typo in D, for the President function")
		#
		idx <- which(!is.na(data[,COL_ATT_FCT_NOM]) & data[,COL_ATT_FCT_NOM]=="PRESIDENT DE ASSEMBLEE NATIONALE")
		if(length(idx)>0)
		{	data[idx,COL_ATT_FCT_NOM] <- "PRESIDENT DE L ASSEMBLEE NATIONALE"
			data[idx,COL_ATT_CORREC_INFO] <- TRUE 
			tlog(2,"Fixed ",length(idx)," rows (function)")
			corr.rows <- union(corr.rows,idx)
		}
	}
	tlog(0,"CHECKPOINT 3: Fixed a total of ",length(corr.rows)," rows (",(100*length(corr.rows)/nrow(data)),"%) for various (non-date-related) issues")
	update.stat.table(s.nbr=3, s.name="Apply systematic non-date corrections", del.nbr=0, mod.nbr=length(corr.rows), add.nbr=0, size=nrow(data))
	
	# remove rows without mandate dates and without function dates
	tlog(0,"Removing rows with no mandate and no function date")
	bef.nrow <- nrow(data)
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	idx <- which(is.na(data[,COL_ATT_MDT_DBT]) & is.na(data[,COL_ATT_MDT_FIN]) 
						& is.na(data[,COL_ATT_FCT_DBT]) & is.na(data[,COL_ATT_FCT_FIN]))
		if(length(idx)>0)
			data <- data[-idx, ]
		tlog(0,"Removed ",length(idx)," incomplete rows")
		tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	}
	tlog(0,"CHECKPOINT 4: Removed ",length(idx)," incomplete rows (",(100*length(idx)/nrow(data)),"%)")
	update.stat.table(s.nbr=4, s.name="Remove rows without mandate date", del.nbr=length(idx), mod.nbr=0, add.nbr=0, size=bef.nrow)
	
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
	update.stat.table(s.nbr=5, s.name="Complete missing function dates", del.nbr=0, mod.nbr=length(corr.rows), add.nbr=0, size=nrow(data))
	
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
{	tlog.start.loop(0,ncol(data),"Converting date and numeric columns")
	for(c in 1:ncol(data))
	{	col.name <- colnames(data)[c]
		col.type <- COL_TYPES[col.name]
		
		# dealing with dates
		if(col.type=="dat")
		{	tlog.loop(2,c,"Column \"",col.name,"\": converting to date")
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
#		{	tlog.loop(2,c,"Column \"",col$name,"\": converting to numbers")
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
			tlog.loop(2,c,"Column \"",col.name,"\": simple string, no conversion")
	}
	tlog.end.loop(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	
	
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
	tlog.start.loop(2,length(codes),"Looking for compatible rows among them")
	tmp <- lapply(1:length(codes), function(i)
	{	code <- codes[i]
		tlog.loop(4,i,"Processing code ",code," (",i,"/",length(codes),")")
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
	tlog.end.loop(2,"Loop over")

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
	update.stat.table(s.nbr=6, s.name="Merge compatible rows", del.nbr=removed.nbr, mod.nbr=0, add.nbr=0, size=nbr.before)
	
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
		tlog.start.loop(2,nrow(data),"Processing each row")
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
				tlog.loop(4,r,str)
				tlog(6,"Before: ",str2)
				if(changed.start)
					tlog(8,"Modifying function start")
				if(changed.end)
					tlog(8,"Modifying function end")
				data[r,COL_ATT_CORREC_DATE] <- TRUE
				tlog(6,"After : ",format.row(data[r,]))
				nbr.corr <- nbr.corr + 1
			}
		}
		tlog.end.loop(2,"Loop over")
	}
	
	tlog(2, "CHECKPOINT 7: Total number of adjusted function dates: ",nbr.corr, " (",(100*nbr.corr/nrow(data)),"%)")
	update.stat.table(s.nbr=7, s.name="Adjust function dates", del.nbr=0, mod.nbr=nbr.corr, add.nbr=0, size=nrow(data))
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
# Ugly fix used to get the election series of senators that represent people leaving abroad.
# This senatorial circonscription is not treated like the others, and is not associated to
# a single series. But all of them. So we distinguish people instead.
#
# name: last name of the concerned senator.
# 
# returns: the election series of this senator.
#############################################################################################
get.adhoc.senator.series.fix <- function(name)
{	if(name=="ANGO ELA PAVLOPULOS") series <- "b"
	else if(name=="ARMENGAUD") series <- "c1"
	else if(name=="BANSARD") series <- "b"
	else if(name=="BARRAS") series <- "c1"
	else if(name=="BAYLE") series <- "b"
	else if(name=="BETHOUART") series <- "a"
	else if(name=="BIARNES") series <- "a"
	else if(name=="BRISEPIERRE") series <- "a"
	else if(name=="CADIC") series <- "a"
	else if(name=="CANTEGRIT") series <- "b"
	else if(name=="CARRIER") series <- "b"
	else if(name=="CERISIER BEN GUIGA") series <- "b"
	else if(name=="COINTAT") series <- "c2"
	else if(name=="CONWAY MOURET") series <- "c1"
	else if(name=="CROZE") series <- "a"
	else if(name=="D ORNANO") series <- "c1"
	else if(name=="DE CUTTOLI") series <- "b"
	else if(name=="DE VILLEPIN") series <- "c1"
	else if(name=="DEL PICCHIA") series <- "a"
	else if(name=="DEROMEDI") series <- "a"
	else if(name=="DURAND CHASTEL") series <- "c1"
	else if(name=="DUVERNOIS") series <- "b"
	else if(name=="FERRAND") series <- "a"
	else if(name=="FRASSA") series <- "a"
	else if(name=="GARRIAUD MAYLAM") series <- "c1"
	else if(name=="GASPARD") series <- "a"
	else if(name=="GROS") series <- "b"
	else if(name=="GUERRY") series <- "b"
	else if(name=="HABERT") series <- "a"
	else if(name=="KAMMERMANN") series <- "c1"
	else if(name=="LE GLEUT") series <- "b"
	else if(name=="LECONTE") series <- "b"
	else if(name=="LEPAGE") series <- "a"
	else if(name=="LONGCHAMBON") series <- "a"
	else if(name=="MAMAN") series <- "b"
	else if(name=="MOTAIS DE NARBONNE") series <- "c1"
	else if(name=="PENNE") series <- "c1"
	else if(name=="REGNARD") series <- "b"
	else if(name=="RENAUD GARABEDIAN") series <- "b"
	else if(name=="ROSSELLI") series <- "c1"
	else if(name=="ROUX") series <- "b"
	else if(name=="SAUVAGEOT") series <- "b"
	else if(name=="WIRTH") series <- "c1"
	else if(name=="YUNG") series <- "c2"
	
	else stop("Could not find Senator ",name)
	
	return(series)
}




#############################################################################################
# Retrieves the election dates associated to the specified series, or all of them if there
# is no specified series.
#
# data: the full data table.
# r: number of the considered row in the data table.
# election.data: previously loaded election-related data.
# series.file: just there to indicate whether we should look for a series.
#
# returns: table containing the appropriate election dates (possibly all of them).
#############################################################################################
retrieve.series.election.dates <- function(data, r, election.data, series.file)
{	# set up variables
	election.table <- election.data$election.table
	election.dates <- election.table
	
	# get the dates
	if(hasArg(series.file))
	{	series.table <- election.data$series.table
		series.list <- election.data$series.list
		
		# CD table
		if(COL_ATT_CANT_CODE %in% colnames(series.table))
		{	idx <- which(series.table[,COL_ATT_DPT_CODE]==data[r,COL_ATT_DPT_CODE]
							& series.table[,COL_ATT_CANT_NOM]==data[r,COL_ATT_CANT_NOM])
			series <- series.table[idx,COL_VERIF_SERIE]
		}
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
	
	return(election.dates)
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
	col.mdt <- c(COL_ATT_MDT_DBT, COL_ATT_MDT_FIN)
	col.fct <- c(COL_ATT_FCT_DBT, COL_ATT_FCT_FIN)
	has.fct <- COL_ATT_FCT_DBT %in% colnames(data)
	
	# load election-related data
	election.data <- load.election.data(data, election.file, series.file)
	
	# compare mandate and election dates
	tlog.start.loop(2,nrow(data),"Check mandate dates against election dates")
	nbr.corrected <- 0
	for(r in 1:nrow(data))
	{	tlog.loop(4,r,"Processing row ",r,"/",nrow(data),": ",format.row.dates(data[r,]))
		
		# get the appropriate election dates
		election.dates <- retrieve.series.election.dates(data, r, election.data, series.file)
		
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
				{	if(has.fct && !is.na(data[r,col.fct[i]]) && !is.na(data[r,col.mdt[i]]) 
							&& ((i==1 && data[r,col.fct[i]]<=(data[r,col.mdt[i]]-date.diff[i]))
								|| (i==2 && data[r,col.fct[i]]>=(data[r,col.mdt[i]]-date.diff[i]))))
					{	data[r,col.fct[i]] <- data[r,col.mdt[i]] - date.diff[i]
						fct.changed[i] <- TRUE
					}
					data[r,col.mdt[i]] <- data[r,col.mdt[i]] - date.diff[i]
					data[r,COL_ATT_CORREC_DATE] <- TRUE
					mdt.changed[i] <- TRUE
				}
			}
		}
		
		# post-adjustments, probably for micro-mandates anyways
		if(!is.na(data[r,COL_ATT_MDT_FIN]) && data[r,COL_ATT_MDT_FIN]<data[r,COL_ATT_MDT_DBT])
			data[r,COL_ATT_MDT_FIN] <- data[r,COL_ATT_MDT_DBT]
#		if(has.fct && !is.na(data[r,COL_ATT_FCT_DBT]) && data[r,COL_ATT_FCT_DBT]<data[r,COL_ATT_MDT_DBT])
#			data[r,COL_ATT_FCT_DBT] <- data[r,COL_ATT_MDT_DBT]
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
			#tlog(6, "After : ", format.row.dates(data[r,]))
			tlog(6, "After : ", format.row(data[r,]))
			#readline()
		}
		
		# debug
		#if(motive.changed)
		#	readline()		
	}
	tlog.end.loop(2,"CHECKPOINT 8: Rounded ",nbr.corrected," rows with election-related issues (",(100*nbr.corrected/nrow(data)),"%)")
	update.stat.table(s.nbr=8, s.name="Round rows using election dates", del.nbr=0, mod.nbr=nbr.corrected, add.nbr=0, size=nrow(data))
	
	return(data)
}




#############################################################################################
# Merges the mandates that overlap (or are consecutive) and have compatible data. Compatible data 
# means that besides the personal info and mandate dates (a few other exceptions such as correction
# flags), for the rest of the columns, one value must be NA or both values must be exactly identical.
#
# data: original table.
# type: type of the considered mandate (CD, CM, etc.).
# log: whether or not to display log messages.
#
# return: same table, with merged overlapping mandates.
#############################################################################################
merge.overlapping.mandates <- function(data, type, log=TRUE)
{	tlog(0,"Merging overlapping rows of the same person (provided their functions are compatible)")
	has.fct <- COL_ATT_FCT_DBT %in% colnames(data)
	
	# set the attributes used to test for compatibility (note the setdiff!)
	atts <- setdiff(colnames(data), c(COL_ATT_ELU_ID, 		# person ID
				COL_ATT_MDT_DBT, COL_ATT_MDT_FIN, 			# mandate dates
				COL_ATT_FCT_DBT, COL_ATT_FCT_FIN,			# function dates
				COL_ATT_CORREC_INFO, COL_ATT_CORREC_DATE,	# correction flags
				COL_ATT_SOURCES))							# sources
	
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
		circo.codes <- sapply(1:nrow(data), function(r) paste(data[r,COL_ATT_DPT_CODE],data[r,COL_ATT_COM_CODE],collapse="_"))
	else if(type=="CR")
		circo.codes <- data[,COL_ATT_REG_CODE]
	else if(type=="D")
		circo.codes <- sapply(1:nrow(data), function(r) paste(data[r,COL_ATT_DPT_CODE],data[r,COL_ATT_CIRC_CODE],collapse="_"))
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
	tlog.start.loop(2,length(unique.ids),"Processing each unique id")
	for(i in 1:length(unique.ids))
	{	idx <- which(data[,COL_ATT_ELU_ID]==unique.ids[i])
		changed <- TRUE
		
		while(changed)
		{	tlog.loop(4,i,"(Re)processing id ",unique.ids[i]," (",i,"/",length(unique.ids),"): found ",length(idx)," rows")
			changed <- FALSE
			
			# nothing to do
			if(length(idx)<2)
			{	if(log) tlog(6,"Unique row, nothing to compare to")
			}
			
			# comparing the to the subsequent rows
			else
			{	for(j in 1:(length(idx)-1))
				{	if(log) tlog(6,"Processing row ",j,"/",length(idx))
					
					# check if row not already removed
					if(idx[j] %in% idx.rmv)
					{	if(log) tlog(8,"Row already removed, nothing to compare to")
					}
					# row not removed, we go on
					else 
					{	for(k in (j+1):length(idx))
						{	if(log) tlog(8,"Comparing to row ",k,"/",length(idx))
							# check if row not already removed
							if(idx[k] %in% idx.rmv)
							{	if(log) tlog(10,"Row already removed, nothing to compare to")
							}
							# row not removed, we go on
							else 
							{	if((is.na(circo.codes[idx[j]]) || is.na(circo.codes[idx[k]])				# at least one NA circonscription, 
										|| circo.codes[idx[j]]==circo.codes[idx[k]])						# or same circonscription
									&& (date.intersect(start1=data[idx[j],COL_ATT_MDT_DBT], 				# mandate dates must overlap
											end1=data[idx[j],COL_ATT_MDT_FIN],
											start2=data[idx[k],COL_ATT_MDT_DBT], 
											end2=data[idx[k],COL_ATT_MDT_FIN])
										|| (!is.na(data[idx[j],COL_ATT_MDT_DBT])							# or mandates must be consecutive
											&& !is.na(data[idx[k],COL_ATT_MDT_FIN])
											&& data[idx[j],COL_ATT_MDT_DBT]==(data[idx[k],COL_ATT_MDT_FIN]+1))
										|| (!is.na(data[idx[k],COL_ATT_MDT_DBT])							# (another way of being consecutive)
											&& !is.na(data[idx[j],COL_ATT_MDT_FIN])
											&& data[idx[k],COL_ATT_MDT_DBT]==(data[idx[j],COL_ATT_MDT_FIN]+1)))
									&& (is.na(fct.att) || 													# no function specified at all in the table
										is.na(data[idx[j],fct.att]) || is.na(data[idx[k],fct.att]) 			# or no function name in at least one of the rows
											|| is.na(data[idx[j],COL_ATT_FCT_DBT])							# or no function date in at least one of the rows 
											|| is.na(data[idx[k],COL_ATT_FCT_DBT])
											|| (data[idx[j],fct.att]==data[idx[k],fct.att] && 				# or the same function name in both rows, 
													(date.intersect(start1=data[idx[j],COL_ATT_FCT_DBT],	# in which case the function dates must overlap too
														end1=data[idx[j],COL_ATT_FCT_FIN],
														start2=data[idx[k],COL_ATT_FCT_DBT], 
														end2=data[idx[k],COL_ATT_FCT_FIN])
													|| (!is.na(data[idx[j],COL_ATT_FCT_DBT])				# or functions must be consecutive
														&& !is.na(data[idx[k],COL_ATT_FCT_FIN])
														&& data[idx[j],COL_ATT_FCT_DBT]==(data[idx[k],COL_ATT_FCT_FIN]+1))
													|| (!is.na(data[idx[k],COL_ATT_FCT_DBT])				# (another way of being consecutive)
														&& !is.na(data[idx[j],COL_ATT_FCT_FIN])
														&& data[idx[k],COL_ATT_FCT_DBT]==(data[idx[j],COL_ATT_FCT_FIN]+1)))
												)
										)
								)
								{	# log detected overlap
									if(log) tlog(12, format.row(data[idx[j],]))
									if(log) tlog(12, format.row(data[idx[k],]))
									if(log) tlog(10, "Overlap detected between")
									if(log) tlog(12, format.row.dates(data[idx[j],]),if(has.fct) paste0(" (",data[idx[j],fct.att],")") else "")
									if(log) tlog(12, format.row.dates(data[idx[k],]),if(has.fct) paste0(" (",data[idx[k],fct.att],")") else "")
									
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
									
									# update the sources
									src1 <- strsplit(data[idx[j],COL_ATT_SOURCES],",")[[1]]
									src2 <- strsplit(data[idx[k],COL_ATT_SOURCES],",")[[1]]
									src <- paste(union(src1,src2),collapse=",")
									data[idx[j],COL_ATT_SOURCES] <- src
									
									# update the correction flags
									data[idx[j],COL_ATT_CORREC_INFO] <- data[idx[j],COL_ATT_CORREC_INFO] || data[idx[k],COL_ATT_CORREC_INFO]
									data[idx[j],COL_ATT_CORREC_DATE] <- data[idx[j],COL_ATT_CORREC_DATE] || data[idx[k],COL_ATT_CORREC_DATE]
									
									# update the rest of the columns
									for(c in 1:length(atts))
									{	if(is.na(data[idx[j],atts[c]]))
											data[idx[j],atts[c]] <- data[idx[k],atts[c]]
										# if both are not NA, priority to RNE data
										else if(!is.na(data[idx[k],atts[c]]))
										{	if(!("RNE" %in% src1))
												data[idx[j],atts[c]] <- data[idx[k],atts[c]]
										}
									}
									
									# log merged row
									if(log) tlog(10, "After merge:")
									if(log) tlog(12, format.row(data[idx[j],]))
									#tlog(12, format.row.dates(data[idx[j],])," (",data[idx[j],fct.att],")")
									#readline() #stop()
									
									# update counters
									nbr.corr <- nbr.corr + 1
									idx.rmv <- c(idx.rmv, idx[k])
									data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
									changed <- TRUE
								}
							}
						}
					}
				}
			}
		}
	}
	if(log) 
	{	tlog.end.loop(2, "CHECKPOINT 10: Total number of rows deleted after merging: ",nbr.corr, " (",100*nbr.corr/nrow(data),"%)")
		update.stat.table(s.nbr=10, s.name="Merge overlapping mandates", del.nbr=nbr.corr, mod.nbr=0, add.nbr=0, size=nrow(data))
	}
	else
		tlog.end.loop(2, "Total number of rows deleted after merging: ",nbr.corr, " (",100*nbr.corr/nrow(data),"%)")
	
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
	election.data <- load.election.data(data, election.file, series.file)
	
	# compare mandate and election dates
	tlog.start.loop(2,nrow(data),"Check mandate dates against election dates")
	nbr.splits <- 0
	for(r in 1:nrow(data))
	{	split.flag <- TRUE
		
		while(split.flag)
		{	tlog.loop(4,r,"Processing row ",r,"/",nrow(data),": ",format.row.dates(data[r,]))
			tlog(6, format.row(data[r,]))
			split.flag <- FALSE
			
			# get the appropriate election dates
			election.dates <- retrieve.series.election.dates(data, r, election.data, series.file)
			
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
#				else
				{	# log event
					tlog(6,"Splitting overlap detected for election ",format(election.dates[idx.tests,1]),"--",format(election.dates[idx.tests,2]))
					tlog(8,"Before : ",format.row.dates(data[r,])," vs. ", format(election.dates[idx.tests,1]), "--", format(election.dates[idx.tests,2]))
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
						if(data[r,COL_ATT_FCT_DBT]<election.dates[idx.tests,COL_VERIF_DATE_TOUR2] 
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
#					readline() #stop()
				
					# add new row to new data frame
					new.data <- rbind(new.data, new.row)
					
					nbr.splits <- nbr.splits + 1
					split.flag <- TRUE
				}
			}
		}
	}
	tlog.end.loop(2,"CHECKPOINT 11: Added ",nbr.splits," rows (",(100*nbr.splits/nbr.before),"%) to the table by splitting periods spanning several actual mandates")
	update.stat.table(s.nbr=11, s.name="Split long mandates", del.nbr=0, mod.nbr=0, add.nbr=nbr.splits, size=nbr.before)
	
	data <- rbind(data, new.data)
	tlog(2, "Table now containing ",nrow(data)," rows")
	return(data)
}

	
	
	
#############################################################################################
# Deletes the rows corresponding to micro-mandates and micro-functions, i.e. mandates or functions 
# of only a few days, which correspond to errors (or sometimes missing mandates impossible to 
# recover through alternative means).
#
# data: original table.
# tolerance: minimal length of a mandate/function (expressed in days).
#
# return: same table, without the micro-mandates and micro-functions.
#############################################################################################
remove.micro.mdtfcts <- function(data, tolerance)
{	tlog(0,"Removing micro-mandates and micro-functions, for a duration <",tolerance," days")
	
	# compute duration in number of days
	mdt.removed <- 0
	mdt.before <- nrow(data)
	idx <- which(!is.na(data[,COL_ATT_MDT_DBT]) & !is.na(data[,COL_ATT_MDT_FIN]))
	tlog(2,"Found ",length(idx)," rows with both start and end mandate dates")
	if(length(idx)>0)
	{	# compute mandate duration
		durations <- as.integer(data[idx,COL_ATT_MDT_FIN] - data[idx,COL_ATT_MDT_DBT])
		
		# compare to limit
		if(!is.na(tolerance))
			idx <- idx[durations<=tolerance]
		tlog(4,"Found ",length(idx)," mandate(s) which are too short (<=",tolerance," days)")
		
		if(length(idx)>0)
		{	# look for exceptions
			exception.idx <- which(data[idx,COL_ATT_ELU_ID_RNE]=="663"
							& data[idx,COL_ATT_MDT_DBT]==as.Date("2017/9/25") 
							& data[idx,COL_ATT_MDT_FIN]==as.Date("2017/9/30"))
			tlog(6,"Including ",length(exception.idx)," manually marked exceptions")
			
			# log the list of micro-mandates
			tlog.start.loop(4,length(idx),"List of concerned rows:")
			tmp <- sapply(1:length(idx), function(i)
			{	tlog.loop(6,i,"Row ", idx[i], "(",i,"/",length(idx),"): ",format.row.dates(data[idx[i],]),
					if(i %in% exception.idx) " (Exception)" else "")
				tlog(8, format.row(data[idx[i],]))
			})
			tlog.end.loop(4,"Loop over")
	
			# get the ids associated to a single micro-mandate
			nms1 <- names(which(table(data[,COL_ATT_ELU_ID])==1))
			nms2 <- unique(data[idx,COL_ATT_ELU_ID])
			nms <- intersect(nms1,nms2)
			tlog(4,"Among them, ",length(nms)," correspond to persons with only this very mandate")
			
			# log the mandates associated to these ids
			if(length(nms)>0)
			{	tmp <- sapply(1:length(nms), function(i)
				{	j <- which(data[,COL_ATT_ELU_ID]==nms[i])
					tlog(6, "Row ", j, "(",i,"/",length(idx),"): ", format.row(data[j,]))
				})
			}
			
			# remove exceptions
			if(length(exception.idx)>0)
				idx <- idx[-exception.idx]
			
			# remove micro-mandates
			if(length(idx)>0)
				data <- data[-idx,]
			mdt.removed <- length(idx)
		}
	}
	
	# possibly do the same for functions
	fct.removed <- 0
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	# compute duration in number of days
		fct.before <- nrow(data)
		idx <- which(!is.na(data[,COL_ATT_FCT_DBT]) & !is.na(data[,COL_ATT_FCT_FIN]))
		tlog(2,"Found ",length(idx)," rows with both start and end function dates")
		if(length(idx)>0)
		{	# compute mandate duration
			durations <- as.integer(data[idx,COL_ATT_FCT_FIN] - data[idx,COL_ATT_FCT_DBT])
			
			# compare to limit
			if(!is.na(tolerance))
				idx <- idx[durations<=tolerance]
			tlog(4,"Found ",length(idx)," function(s) which are too short (<=",tolerance," days)")
			
			if(length(idx)>0)
			{	# look for exceptions
				exception.idx <- c()
				tlog(6,"Including ",length(exception.idx)," manually marked exceptions")
				
				# log the list of micro-mandates
				tlog.start.loop(4,length(idx),"List of concerned rows:")
				tmp <- sapply(1:length(idx), function(i)
				{	tlog.loop(6,i,"Row ", idx[i], "(",i,"/",length(idx),"): ",format.row.dates(data[idx[i],]),
							if(i %in% exception.idx) " (Exception)" else "")
					tlog(8, format.row(data[idx[i],]))
				})
				tlog.end.loop(4,"Loop over")
		
				# get the ids associated to a single micro-function
				nms1 <- names(which(table(data[,COL_ATT_ELU_ID])==1))
				nms2 <- unique(data[idx,COL_ATT_ELU_ID])
				nms <- intersect(nms1,nms2)
				tlog(4,"Among them, ",length(nms)," correspond to persons with only this very mandate")
				
				# log the mandates associated to these ids
				if(length(nms)>0)
				{	tmp <- sapply(1:length(nms), function(i)
					{	j <- which(data[,COL_ATT_ELU_ID]==nms[i])
						tlog(6, "Row ", j, "(",i,"/",length(idx),"): ", format.row(data[j,]))
					})
				}
				
				# remove exceptions
				if(length(exception.idx)>0)
					idx <- idx[-exception.idx]
				
				# remove micro-functions
				if(length(idx)>0)
				{	data[idx,COL_ATT_FCT_CODE] <- NA
					data[idx,COL_ATT_FCT_DBT] <- NA
					data[idx,COL_ATT_FCT_FIN] <- NA
					data[idx,COL_ATT_FCT_MOTIF] <- NA
					data[idx,COL_ATT_FCT_NOM] <- NA
					data[idx,COL_ATT_CORREC_DATE] <- TRUE
					data[idx,COL_ATT_CORREC_INFO] <- TRUE
				}
				fct.removed <- length(idx)
			}
		}
	}
	
	tot.removed <- mdt.removed + fct.removed
	tlog(2,"CHECKPOINT 9: Removed a total of ",tot.removed," rows (",(100*tot.removed/mdt.before),"%) corresponding to micro-mandates and/or functions")
	tlog(4,"Removed ",mdt.removed," rows (",(100*mdt.removed/mdt.before),"%) corresponding to micro-mandates")
	if(COL_ATT_FCT_DBT %in% colnames(data))
		tlog(4,"Removed ",fct.removed," rows (",(100*fct.removed/fct.before),"%) corresponding to micro-functions")
	update.stat.table(s.nbr=9, s.name="Remove micro mandates/functions", del.nbr=mdt.removed+fct.removed, mod.nbr=0, add.nbr=0, size=mdt.before)
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
	count <- 0
	
	# some mandate types don't have unique positions
	if((type %in% c("CD","D")))
	{	# identify all unique position
		tlog(2,"Identifying all unique positions")
		if(type=="CD")
		{	data.pos <- data[,COL_ATT_CANT_ID]
			unique.pos <- sort(unique(data.pos[data[,COL_ATT_CANT_NOM]!="CANTON FICTIF"]))
		}
		else if(type=="D")
		{	dpts <- data[,COL_ATT_DPT_CODE]
			circos <- data[,COL_ATT_CIRC_CODE]
			data.pos <- apply(cbind(dpts,circos),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos))
		}
		tlog(4,"Found ",length(unique.pos)," of them")
		
		# process each unique position
		tlog.start.loop(2,length(unique.pos),"Processing each unique mandate position")
		for(p in 1:length(unique.pos))
		{	tlog.loop(4,p,"Processing mandate position ",unique.pos[p], "(",p,"/",length(unique.pos),")")
			
			# get the corresponding rows
			idx <- which(data.pos==unique.pos[p])
			tlog(4,"Found ",length(idx)," rows")
			
			if(length(idx)>1)
			{	# check if their dates overlap
				ccount <- 0
				
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared mandate
					start1 <- data[idx[i],COL_ATT_MDT_DBT]
					end1 <- data[idx[i],COL_ATT_MDT_FIN]
					sex1 <- data[idx[i],COL_ATT_ELU_SEXE]
					tlog(6,"Considering row ",i,"/",length(idx),": ",format(start1),"--",format(end1)," (",sex1,")")
					tlog(6, format.row(data[idx[i],]))
					
					# we suppose there is a start date for all mandates (at this stage of the process)
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared mandate
						start2 <- data[idx[j],COL_ATT_MDT_DBT]
						end2 <- data[idx[j],COL_ATT_MDT_FIN]
						sex2 <- data[idx[j],COL_ATT_ELU_SEXE]
						tlog(8,"Comparing to row ",j,"/",length(idx),": ",format(start2),"--",format(end2)," (",sex2,")")
						tlog(8, format.row(data[idx[j],]))
						
						# check if the periods intersect
						if(date.intersect(start1, end1, start2, end2)
							&& (type=="D" 
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
											data[idx[i],COL_ATT_MDT_DBT] <- start1
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
											data[idx[j],COL_ATT_MDT_DBT] <- start2
											data[idx[j],COL_ATT_FCT_DBT] <- max(start2,data[idx[j],COL_ATT_FCT_DBT])
											data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
											tlog(14,"After correction of the second mandate: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
											tlog(14, format.row(data[idx[j],]))
											# count the problematic cases
											count <- count + 1
											ccount <- ccount + 1
										}
										else
										{	tlog(14,"Same start and end dates: probably an error, handle manually")
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
											data[idx[j],COL_ATT_MDT_FIN] <- end2
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
											data[idx[i],COL_ATT_MDT_FIN] <- end1
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
										data[idx[i],COL_ATT_MDT_FIN] <- end1
										data[idx[i],COL_ATT_FCT_FIN] <- min(end1,data[idx[i],COL_ATT_FCT_FIN])
										data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
										tlog(12,"After correction of the first mandate: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
										tlog(12, format.row(data[idx[i],]))
									}
									else
									{	end2 <- end2 - ovlp.duration
										data[idx[j],COL_ATT_MDT_FIN] <- end2
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
				tlog(6,"Corrected ",ccount," overlaps for this specific position")
			}
		}
		tlog.end.loop(4,"Processing over")
		
		tlog(2,"CHECKPOINT 12: Shortened a total of ",count," mandates due to self-overlap, for the whole table (",(100*count/nrow(data)),"%)")
		tlog(2,"Number of rows remaining: ",nrow(data))
		update.stat.table(s.nbr=12, s.name="Shorten overlapping mandates", del.nbr=0, mod.nbr=count, add.nbr=0, size=nrow(data))
	}
	
	return(data)
}




#############################################################################################
# For a given position, detects overlapping functions and solves the problem by shortening 
# the older one.
#
# data: original table.
# type: type of mandate (CD, CM, etc.).
# tolerance: maximal overlap (if too long, the overlap is ignored, not solved here).
#
# return: same table, without the (minor) overlaps.
#############################################################################################
shorten.overlapping.functions <- function(data, type, tolerance=1)
{	tlog(0,"Solving function overlaps for unique positions")
	count <- 0
	
	# skip if no function in the table
	if(COL_ATT_FCT_DBT %in% colnames(data))
	{	# identify all unique position
		tlog(2,"Identifying all unique positions")
		if(type=="CD")
		{	ign.functs <- c(
				"VICE PRESIDENT DU CONSEIL DEPARTEMENTAL",			# several of them, but not distinguished
				"VICE PRESIDENT DELEGUE DU CONSEIL DEPARTEMENTAL",	# several of them, but not distinguished
				"QUESTEUR",											# several of them, but not distinguished
				"PRESIDENT DE GROUPE",								# several of them, but not distinguished
				"PRESIDENT DE COMMISSION",							# several of them, but not distinguished
				"AUTRE MEMBRE COMMISSION PERMANENTE",				# several of them, but not distinguished
				"AUTRE MEMBRE"										# many
			)
			dpts <- data[,COL_ATT_DPT_CODE]
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- apply(cbind(dpts,functs),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos[!is.na(functs)]))
		}
		else if(type=="CM")
		{	ign.functs <- c("MAIRE DELEGUE")
			dpts <- data[,COL_ATT_DPT_CODE]
			coms <- data[,COL_ATT_COM_CODE]
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- apply(cbind(dpts,coms,functs),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos[!is.na(functs)]))
		}
		else if(type=="CR")
		{	ign.functs <- c(
				"PRESIDENT DE COMMISSION",
				"AUTRE MEMBRE COMMISSION PERMANENTE"
			)
			regs <- data[,COL_ATT_REG_CODE]
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- apply(cbind(regs,functs),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos[!is.na(functs)]))
		}
		else if(type=="D")
		{	ign.functs <- c(
				#"MEMBRE",										# many of them (actually should be removed at AN integration)
				"QUESTEUR DE L ASSEMBLEE NATIONALE",			# several but not distinguished
				"SECRETAIRE DE L ASSEMBLEE NATIONALE",			# several but not distinguished
				"SECRETAIRE D AGE DE L ASSEMBLEE NATIONALE",	# several but not distinguished
				"VICE PRESIDENT DE L ASSEMBLEE NATIONALE"		# several but not distinguished
			)
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- functs
			unique.pos <- sort(unique(data.pos[!is.na(functs)]))
		}
		else if(type=="EPCI")
		{	ign.functs <- c("VICE PRESIDENT D EPCI")
			siren <- data[,COL_ATT_EPCI_SIREN]
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- apply(cbind(siren,functs),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos[!is.na(functs)]))
			
		}
		else if(type=="M")
		{	dpts <- data[,COL_ATT_DPT_CODE]
			coms <- data[,COL_ATT_COM_CODE]
			data.pos <- apply(cbind(dpts,coms),1,function(r) paste(r,collapse="_"))
			unique.pos <- sort(unique(data.pos))
		}
		else if(type=="S")
		{	ign.functs <- c(
				"QUESTEUR DU SENAT",		# several but not distinguished
				"SECRETAIRE DU SENAT",		# several but not distinguished
				"VICE PRESIDENT DU SENAT"	# several but not distinguished
			)
			functs <- data[,COL_ATT_FCT_NOM]
			data.pos <- functs
			unique.pos <- sort(unique(data.pos[!is.na(functs)]))
		}
		tlog(4,"Found ",length(unique.pos)," of them")
		
		# process each unique position
		tlog.start.loop(2,length(unique.pos),"Processing each unique position")
		for(p in 1:length(unique.pos))
		{	tlog.loop(4,p,"Processing position ",unique.pos[p], "(",p,"/",length(unique.pos),")")
			
			# get the corresponding rows
			idx <- which(data.pos==unique.pos[p])
			tlog(4,"Found ",length(idx)," rows")
			
			if(length(idx)>1)
			{	# check if their dates overlap
				ccount <- 0
				
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared function
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					tlog(6,"Considering row ",i,"/",length(idx),": ",format(start1),"--",format(end1))
					tlog(6, format.row(data[idx[i],]))
					
					# check if there is at least a start date
					if(is.na(start1))
						tlog(8, "Missing start date: can't do anything")
					
					# otherwise, we have a first start date
					else
					{	for(j in (i+1):length(idx))
						{	# get the dates of the second compared function
							start2 <- data[idx[j],COL_ATT_FCT_DBT]
							end2 <- data[idx[j],COL_ATT_FCT_FIN]
							tlog(8,"Comparing to row ",j,"/",length(idx),": ",format(start2),"--",format(end2))
							tlog(8, format.row(data[idx[j],]))
							
							# check if there is at least a start date
							if(is.na(start2))
								tlog(8, "Missing start date: can't do anything")
							
							# otherwise, we have a second start date
							else
							{	# check if the periods intersect
								if(date.intersect(start1, end1, start2, end2))
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
													data[idx[i],COL_ATT_FCT_DBT] <- start1
													data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the first function: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[i],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else if(is.na(end2) || !is.na(end1) && end2>end1)
												{	start2 <- end1 + 1
													data[idx[j],COL_ATT_FCT_DBT] <- start2
													data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the second function: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[j],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else
												{	tlog(14,"Same start and end dates: probably an error, handle manually")
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
													data[idx[j],COL_ATT_FCT_FIN] <- end2
													data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the second function: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
													tlog(14, format.row(data[idx[j],]))
													# count the problematic cases
													count <- count + 1
													ccount <- ccount + 1
												}
												else if(start2>start1)
												{	end1 <- start2 - 1
													data[idx[i],COL_ATT_FCT_FIN] <- end1
													data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
													tlog(14,"After correction of the first function: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
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
										{	tlog(10,"Minor overlap, correcting the end date of the earlier function")
											# adjust the end of the older function to avoid overlap
											if(!is.na(end1) && (is.na(end2) || end1<end2))
											{	end1 <- end1 - ovlp.duration
												data[idx[i],COL_ATT_FCT_FIN] <- end1
												data[idx[i],COL_ATT_CORREC_DATE] <- TRUE
												tlog(12,"After correction of the first function: ",format(start1),"--",format(end1)," (overlap: ",ovlp.duration," days)")
												tlog(12, format.row(data[idx[i],]))
											}
											else
											{	end2 <- end2 - ovlp.duration
												data[idx[j],COL_ATT_FCT_FIN] <- end2
												data[idx[j],COL_ATT_CORREC_DATE] <- TRUE
												tlog(12,"After correction of the second function: ",format(start2),"--",format(end2)," (overlap: ",ovlp.duration," days)")
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
				tlog(6,"Corrected ",ccount," function overlaps for this specific position")
			}
		}
		tlog.end.loop(4,"Processing over")
		
		tlog(2,"CHECKPOINT 13: Shortened a total of ",count," functions due to overlap, for the whole table (",(100*count/nrow(data)),"%)")
		tlog(2,"Number of rows remaining: ",nrow(data))
		update.stat.table(s.nbr=13, s.name="Shorten overlapping functions", del.nbr=0, mod.nbr=count, add.nbr=0, size=nrow(data))
	}
	
	return(data)
}




#############################################################################################
# Removes the mandate and function motives associated to missing en dates, adds FM when the
# motive is missing and the end date is aligned with election dates.
#
# data: table to process.
#
# returns: same table, but after the correction.
#############################################################################################
adjust.end.motives <- function(data, election.file, series.file)
{	tlog(0,"Removing end motives associated to no end date")
	treated.rows <- c()
	nbr.removed <- 0
	nbr.added <- 0
	
	##############################
	
	# correct superfluous mandate motives
	idx <- which(is.na(data[,COL_ATT_MDT_FIN]) & !is.na(data[,COL_ATT_MDT_MOTIF]))
	tlog(2,"Found ",length(idx)," superfluous mandate end motives")
	if(length(idx)>0)
	{	data[idx,COL_ATT_MDT_MOTIF] <- NA
		nbr.removed <- nbr.removed + length(idx)
		treated.rows <- union(treated.rows, idx)
	}
	
	# possibly correct function motives
	if(COL_ATT_FCT_MOTIF %in% colnames(data))
	{	# correct superfluous function motives
		idx <- which(is.na(data[,COL_ATT_FCT_FIN]) & !is.na(data[,COL_ATT_FCT_MOTIF]))
		tlog(2,"Found ",length(idx)," superfluous function end motives")
		if(length(idx)>0)
		{	data[idx,COL_ATT_FCT_MOTIF] <- NA
			nbr.removed <- nbr.removed + length(idx)
			treated.rows <- union(treated.rows, idx)
		}
	}
	
	##############################

	# load election-related data
	election.data <- load.election.data(data, election.file, series.file)
	
	# correct missing mandate motives
	idx <- which(!is.na(data[,COL_ATT_MDT_FIN]) & is.na(data[,COL_ATT_MDT_MOTIF]))
	tlog.start.loop(2,length(idx),"Found ",length(idx)," rows with mandate end date but no motive, trying to complete them")
	nbr.added <- 0
	if(length(idx)>0)
	{	for(i in 1:length(idx))
		{	r <- idx[i]
			tlog.loop(4,i,"Processing case ",i,"/",length(idx)," (row ",r,")")
			tlog(6,format.row(data[r,]))
			
			# get the appropriate election dates
			election.dates <- retrieve.series.election.dates(data, r, election.data, series.file)
			election.dates <- c(election.dates[,1], election.dates[,2])
			election.dates <- election.dates[!is.na(election.dates)]
			
			# check if the end date matches the day before an election date
			matches <- which(election.dates-1==data[r,COL_ATT_MDT_FIN])
			
			# update the end motive
			if(length(matches)>0)
			{	tlog(6,"Mandate end date matches election ",format(election.dates[matches]))
				data[r,COL_ATT_MDT_MOTIF] <- "FM"
				data[r,COL_ATT_CORREC_INFO] <- TRUE
				nbr.added <- nbr.added + 1
				treated.rows <- union(treated.rows, r)
			}
			else
				tlog(6,"No election match")
		}
		tlog.end.loop(2,"Loop over")
	}
	
	# correct missing function motives
	if(COL_ATT_FCT_MOTIF %in% colnames(data))
	{	idx <- which(!is.na(data[,COL_ATT_FCT_FIN]) & is.na(data[,COL_ATT_FCT_MOTIF]))
		tlog.start.loop(2,length(idx),"Found ",length(idx)," rows with function end date but no motive, trying to complete them")
		nbr.added <- 0
		if(length(idx)>0)
		{	for(i in 1:length(idx))
			{	r <- idx[i]
				tlog.loop(4,i,"Processing case ",i,"/",length(idx)," (row ",r,")")
				tlog(6,format.row(data[r,]))
				
				# get the appropriate election dates
				election.dates <- retrieve.series.election.dates(data, r, election.data, series.file)
				election.dates <- c(election.dates[,1], election.dates[,2])
				election.dates <- election.dates[!is.na(election.dates)]
				
				# check if the end date matches the day before an election date
				matches <- which(election.dates-1==data[r,COL_ATT_FCT_FIN])
				
				# update the end motive
				if(length(matches)>0)
				{	tlog(6,"Function end date matches election ",format(election.dates[matches]))
					data[r,COL_ATT_FCT_MOTIF] <- "FM"
					data[r,COL_ATT_CORREC_INFO] <- TRUE
					nbr.added <- nbr.added + 1
					treated.rows <- union(treated.rows, r)
				}
				else
					tlog(6,"No election match")
			}
			tlog.end.loop(2,"Loop over")
		}
	}
	
	##############################
	
	# log changes
	if(length(treated.rows)>0)
	{	tlog(2,"List of corrected rows: ")
		for(i in treated.rows)
			tlog(4, format.row(data[i,]))
	}
	
	tlog(2,"CHECKPOINT 15: adjusted a total of ",length(treated.rows)," rows for the whole table (",(100*length(treated.rows)/nrow(data)),"%)")
	tlog(4,"Motives deleted: ",nbr.removed," rows (",(100*nbr.removed/nrow(data)),"%)")
	tlog(4,"Motives added: ",nbr.added," rows (",(100*nbr.added/nrow(data)),"%)")
	tlog(2, "Number of rows remaining: ",nrow(data))
	update.stat.table(s.nbr=15, s.name="Adjust end motives", del.nbr=0, mod.nbr=length(treated.rows), add.nbr=0, size=nrow(data))
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
#data8 <- data	
	
	# round mandate and function dates to match election dates, when approximately equal
	if(hasArg(election.file))
		data <- round.mdtfct.dates(data, election.file, series.file, tolerance=7)
#data <- round.mdtfct.dates(data, election.file, tolerance=7)	#debug: data9
#data9 <- data
	
	# removes micro-mandates
	data <- remove.micro.mdtfcts(data, tolerance=7)
#data10 <- data
	
	# merge rows corresponding to overlapping and compatible mandates
	data <- merge.overlapping.mandates(data, type, log=TRUE)
#data11 <- data
	
	# splits rows containing election dates (other than as a start date)
	if(hasArg(election.file))
		data <- split.long.mandates(data, election.file, series.file)
#data <- split.long.mandates(data, election.file)
#data12 <- data
	
	# solve mandate and function intersections (same position)
	data <- shorten.overlapping.mandates(data, type, tolerance=8)
#data13 <- data
	data <- shorten.overlapping.functions(data, type, tolerance=8)
#data14 <- data
	
	# remove micro-mandates again (in case split created any)
	data <- remove.micro.mdtfcts(data, tolerance=7)
#data15 <- data
	
	# adjust end of mandate or function motives
	data <- adjust.end.motives(data, election.file, series.file)
#data16 <- data
	
	#stop()
	return(data)
}
