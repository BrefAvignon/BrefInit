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
#
# returns: data frame made of the cleaned data contained in the files.
#############################################################################################
load.data <- function(filenames, col.map, correc.file)
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
			as.is=TRUE					# don't convert strings to factors
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	# possibly normalize column names
	if(nrow(correc.table)>0)
	{	for(r in 1:nrow(correc.table))
		{	att.name <- correc.table[r,COL_CORREC_ATTR]
			norm.name <- col.map[att.name]
			if(!is.na(norm.name))
				correc.table[r,COL_CORREC_ATTR] <- norm.name
		}
	}
	
	# load all the tables
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

	# normalize column names
	norm.names <- col.map[colnames(data)]
	if(any(is.na(norm.names)) || length(norm.names)!=ncol(data))
		stop("Problem with the number of columns (or their names) when loading the table, after normalization")
	else
		colnames(data) <- norm.names
	
	# convert date and numeric columns
	tlog(0,"Converting date and numeric columns")
	for(c in 1:ncol(data))
	{	col.name <- norm.names[c]
		col.type <- COL_TYPES[col.name]
		# dealing with dates
		if(col.type=="dat")
		{	tlog(2,"Col. \"",col.name,"\": converting to date")
			
			# possibly apply corrections
			if(nrow(correc.table)>0)	
			{	# keep only the relevant corrections
				cor.tab <- correc.table[correc.table[,COL_CORREC_ATTR]==col.name,]
				# process each remaining correction
				if(nrow(cor.tab)>0)
				{	for(r in 1:nrow(cor.tab))
					{	# identify the targeted row in the data table
						idx <- which(data[,COL_ATT_ELU_ID]==cor.tab[r,COL_CORREC_ID]
							& data[,COL_ATT_ELU_NOM]==cor.tab[r,COL_CORREC_NOM]
							& data[,COL_ATT_ELU_PRENOM]==cor.tab[r,COL_CORREC_PRENOM]
							& data[,cor.tab[r,COL_CORREC_ATTR]]==cor.tab[r,COL_CORREC_VALAVT]
						)
						# there should be exactly one
						if(length(idx)<1)
						{	tlog(4,"Could not find a correction: ",paste(cor.tab[r,],collapse=";"))
							stop(paste0("Could not find a correction: ",paste(cor.tab[r,],collapse=";")))
						}
						else if(length(idx)>1)
						{	tlog(4,"A correction matches several cases: ",paste(cor.tab[r,],collapse=";"))
							stop(paste0("A correction matches several cases: ",paste(cor.tab[r,],collapse=";")))
						}
						else
						{	tlog(4,"Correcting entry: ",paste(cor.tab[r,],collapse=";"))
#							vals[which(vals==as.Date("29/1/0201",format="%d/%m/%Y"))] <- as.Date("20/10/2016",format="%d/%m/%Y")	# manual correction for COCHONNEAU Virginie 1/5/1982 (CM), completely arbitrary
							data[idx,cor.tab[r,COL_CORREC_ATTR]] <- cor.tab[r,COL_CORREC_VALAPR]
						}
					}
				}
			}
			
			vals <- as.Date(data[,col.name], "%d/%m/%Y")
			
			#format(x, format="%Y/%m/%d")
			data <- data[, names(data)!=col.name]
			data <- cbind(data,vals)
			names(data)[ncol(data)] <- col.name
		}
		
		# dealing with numbers
# actually, this is done later if needed 
# (as there's only one such column which requires more specific processing)
#		else if(col$tp=="num")
#		{	tlog(2,"Col. \"",col$name,"\": converting to number")
#			vals <- suppressWarnings(as.numeric(data[,col$name]))
#			data <- data[, names(data)!=col$name]
#			data <- cbind(data,vals)
#			names(data)[ncol(data)] <- col$name
#		}
		
		# the other columns stay strings
		else
			tlog(2,"Col. \"",col.name,"\": simple string, no conversion")
	}
	
	# setting appropriate encoding of string columns
	# and replace "" by NAs
	tlog(0,"Cleaning/encoding strings")
	for(c in 1:ncol(data))
	{	if(is.character(data[,c]))
		{	tlog(2,"Processing column \"",colnames(data)[c],"\"")
			# convert encoding
##			data[,c] <- iconv(x=data[,c], from="Latin1", to="UTF8")
#			data[,c] <- iconv(x=data[,c], to="UTF8")
			# trim leading/ending whitespace
			data[,c] <- trimws(data[,c])
			# replace empty cells by explicit NAs
			data[which(data[,c]==""),c] <- NA
			
			# possibly apply corrections
			if(nrow(correc.table)>0)	
			{	# keep only the relevant corrections
				cor.tab <- correc.table[correc.table[,COL_CORREC_ATTR]==colnames(data)[c],]
				# process each remaining correction
				if(nrow(cor.tab)>0)
				{	for(r in 1:nrow(cor.tab))
					{	# general correction
						if(all(is.na(cor.tab[r,c(COL_CORREC_ID,COL_CORREC_NOM,COL_CORREC_PRENOM)])))
						{	# identify the targeted rows in the data table
							idx <- which(data[,cor.tab[r,COL_CORREC_ATTR]]==cor.tab[r,COL_CORREC_VALAVT])
							# there should be at least one
							if(length(idx)<1)
							{	tlog(4,"Could not find a correction: ",paste(cor.tab[r,],collapse=";"))
								stop(paste0("Could not find a correction: ",paste(cor.tab[r,],collapse=";")))
							}
							else 
							{	tlog(4,"Replacing ",cor.tab[r,COL_CORREC_VALAVT]," by ",cor.tab[r,COL_CORREC_VALAPR])
								data[idx,cor.tab[r,COL_CORREC_ATTR]] <- cor.tab[r,COL_CORREC_VALAPR]
							}
						}
						# correction of a specific row
						else
						{	# identify the targeted row in the data table
							idx <- which(data[,COL_ATT_ELU_ID]==cor.tab[r,COL_CORREC_ID]
										& data[,COL_ATT_ELU_NOM]==cor.tab[r,COL_CORREC_NOM]
										& data[,COL_ATT_ELU_PRENOM]==cor.tab[r,COL_CORREC_PRENOM]
										& data[,cor.tab[r,COL_CORREC_ATTR]]==cor.tab[r,COL_CORREC_VALAVT]
							)
							# there should be exactly one
							if(length(idx)<1)
							{	tlog(4,"Could not find a correction: ",paste(cor.tab[r,],collapse=";"))
								stop(paste0("Could not find a correction: ",paste(cor.tab[r,],collapse=";")))
							}
							else if(length(idx)>1)
							{	tlog(4,"A correction matches several cases: ",paste(cor.tab[r,],collapse=";"))
								stop(paste0("A correction matches several cases: ",paste(cor.tab[r,],collapse=";")))
							}
							else
							{	tlog(4,"Correcting entry: ",paste(cor.tab[r,],collapse=";"))
#								vals[which(vals==as.Date("29/1/0201",format="%d/%m/%Y"))] <- as.Date("20/10/2016",format="%d/%m/%Y")	# manual correction for COCHONNEAU Virginie 1/5/1982 (CM), completely arbitrary
								data[idx,cor.tab[r,COL_CORREC_ATTR]] <- cor.tab[r,COL_CORREC_VALAPR]
							}
						}
					}
				}
			}
		}
		else
			tlog(2,"Col. \"",colnames(data)[c],"\": not a string")
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
# Loads the table for departmental counsilors (first extraction).
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd.data <- function()
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
	data <- load.data(filenames=FILES_TAB_CD, col.map=col.map, correc.file=FILE_CORREC_CD)
	
	return(data)
}
#############################################################################################
# Loads the table for departmental counsilors (second extraction).
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd2.data <- function()
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
	data <- load.data(filenames=FILES_TAB_CD2, col.map=col.map, correc.file=FILE_CORREC_CD2) 
	
	return(data)
}




#############################################################################################
# Loads the tables for municipal counsilors (first extraction).
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm.data <- function()
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
	data <- load.data(filenames=FILES_TAB_CM, col.map=col.map, correc.file=FILE_CORREC_CM)
	
	# convert population numbers to actual integers
	tlog(0,"Converting population to integer values")
	cn <- COL_ATT_COM_POP
	vals <- data[,cn]
#	vals <- suppressWarnings(as.integer(vals))
	vals <- as.integer(vals)
	data <- data[, names(data)!=cn]
	data <- cbind(data,vals)
	names(data)[ncol(data)] <- cn
	
	# normalize columns order
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(data))
	if(length(norm.cols)!=ncol(data))
		stop("Problem with the number of columns when reordering table CM columns")
	else
		data <- data[,norm.cols]
	
	return(data)
}




#############################################################################################
# Loads the tables for municipal counsilors (second extraction).
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm2.data <- function()
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
	data <- load.data(filenames=FILES_TAB_CM2, col.map=col.map, correc.file=FILE_CORREC_CM2)
	
	return(data)
}




#############################################################################################
# Loads the table for regional counsilors (first extraction).
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr.data <- function()
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
	data <- load.data(filenames=FILES_TAB_CR, col.map=col.map, correc.file=FILE_CORREC_CR)
	
	return(data)
}




#############################################################################################
# Loads the table for regional counsilors (first extraction).
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr2.data <- function()
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
	data <- load.data(filenames=FILES_TAB_CR2, col.map=col.map, correc.file=FILE_CORREC_CR2)
	
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
	
	return(data)
}




#############################################################################################
# Loads the table for members of the parliament.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.d.data <- function()
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
	data <- load.data(filenames=FILES_TAB_D, col.map=col.map, correc.file=FILE_CORREC_D)
	
	return(data)
}




#############################################################################################
# Loads the table for European counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.de.data <- function()
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
	data <- load.data(filenames=FILES_TAB_DE, col.map=col.map, correc.file=FILE_CORREC_DE)
	
	return(data)
}




#############################################################################################
# Loads the table for EPCI counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.epci.data <- function()
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
	data <- load.data(filenames=FILES_TAB_EPCI, col.map=col.map, correc.file=FILE_CORREC_EPCI)
	
	return(data)
}




#############################################################################################
# Loads the table for mayors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.m.data <- function()
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
	data <- load.data(filenames=FILES_TAB_M, col.map=col.map, correc.file=FILE_CORREC_M)
	
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
	
	return(data)
}




#############################################################################################
# Loads the table for senators.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.s.data <- function()
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
	data <- load.data(filenames=FILES_TAB_S, col.map=col.map, correc.file=FILE_CORREC_S)
	
	return(data)
}
