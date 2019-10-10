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
# cols: description of the columns of the loaded tables.
# correc.file: file containing the corrections.
#
# returns: data frame made of the cleaned data contained in the files.
#############################################################################################
load.data <- function(filenames, cols, correc.file)
{	# load the corrections
	fn <- file.path(FOLDER_CORRECS, correc.file)
	tlog(0,"Loading correction file \"",fn,"\"")
	correc.table <- read.table(
			file=fn, 					# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE,					# don't convert strings to factors
			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	
	# load all the tables
	data <- NULL
	for(filename in filenames)
	{	fn <- file.path(FOLDER_TABLES, filename)
		tlog(0,"Loading table file \"",fn,"\"")
		# read the partial table
		temp <- read.table(
				file=fn, 					# name of the data file
				header=TRUE, 				# look for a header
				sep="\t", 					# character used to separate columns 
				check.names=FALSE, 			# don't change the column names from the file
				comment.char="", 			# ignore possible comments in the content
				row.names=NULL, 			# don't look for row names in the file
				quote="", 					# don't expect double quotes "..." around text fields
				skip=1,						# ignore the first line of the file ("Titre du rapport")
				as.is=TRUE,					# don't convert strings to factors
				fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
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
	
	# convert date columns to R dates
	tlog(0,"Converting date columns to actual R dates")
	for(col in cols)
	{	if(col$tp=="dat")
		{	tlog(2,"Col. \"",col$name,"\": CONVERTING")
			
			# possibly apply corrections
			if(nrow(correc.table)>0)	
			{	# keep only the relevant corrections
				cor.tab <- correc.table[correc.table[,COL_CORREC_ATTR]==col$name,]
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
			
			vals <- as.Date(data[,col$name], "%d/%m/%Y")
			
			#format(x, format="%Y/%m/%d")
			data <- data[, names(data)!=col$name]
			data <- cbind(data,vals)
			names(data)[ncol(data)] <- col$name
		}
		else
			tlog(2,"Col. \"",col$name,"\": not a date")
	}
	
	# setting appropriate encoding of string columns
	# and replace "" by NAs
	tlog(0,"Converting string encoding")
	for(c in 1:ncol(data))
	{	if(is.character(data[,c]))
		{	tlog(2,"Col. \"",colnames(data)[c],"\": CONVERTING")
#			data[,c] <- iconv(x=data[,c], from="Latin1", to="UTF8")
			data[,c] <- iconv(x=data[,c], to="UTF8")
			data[which(data[,c]==""),c] <- NA
		}
		else
			tlog(2,"Col. \"",colnames(data)[c],"\": not a string")
	}
	
	return(data)
}




#############################################################################################
# Loads the tables for departmental counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cd.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_DPT_CODE, basename="dpt_code", tp="cat"),
		list(name=COL_ATT_DPT_NOM, basename="dpt_nom", tp="nom"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_CANT_CODE, basename="canton_code", tp="cat"),
		list(name=COL_ATT_CANT_NOM, basename="canton_nom", tp="nom"),
		list(name=COL_ATT_ELU_NUANCE_CD, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_MDT_NOM, basename="mandat_nom", tp="cat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames=FILES_TAB_CD, cols=cols, corre.file=FILE_CORREC_CD)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for municipal counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_DPT_CODE_M, basename="dpt_code", tp="cat"),
		list(name=COL_ATT_DPT_NOM_M, basename="dpt_nom", tp="nom"),
		list(name=COL_ATT_COM_CODE, basename="ville_code", tp="cat"),
		list(name=COL_ATT_COM_NOM, basename="ville_nom", tp="nom"),
		list(name=COL_ATT_COM_POP, basename="ville_population", tp="num"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_CM, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_CM, cols, FILE_CORREC_CM)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for regional counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_REG_CODE, basename="region_code", tp="cat"),
		list(name=COL_ATT_REG_NOM, basename="region_nom", tp="nom"),
		list(name=COL_ATT_DPT_CODE, basename="dpt_code", tp="cat"),
		list(name=COL_ATT_DPT_NOM_CR, basename="dpt_nom", tp="nom"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_NOM, basename="mandat_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_CR, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_CR, cols, FILE_CORREC_CR)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for members of the parliament.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.d.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_DPT_CODE, basename="dpt_code", tp="cat"),
		list(name=COL_ATT_DPT_NOM, basename="dpt_nom", tp="nom"),
		list(name=COL_ATT_CIRC_CODE, basename="circo_code", tp="cat"),
		list(name=COL_ATT_CIRC_NOM, basename="circo_nom", tp="nom"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_NOM, basename="mandat_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_D, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_D, cols, FILE_CORREC_D)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for European counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.de.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_CIRCE_CODE, basename="circo_code", tp="cat"),
		list(name=COL_ATT_CIRCE_NOM, basename="circo_nom", tp="nom"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_NOM, basename="mandat_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_DE, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_DE, cols, FILE_CORREC_DE)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for EPCI counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.epci.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_DPT_CODE_EPCI, basename="dpt_code_epci", tp="cat"),
		list(name=COL_ATT_DPT_CODE_COM, basename="dpt_code_com", tp="cat"),
		list(name=COL_ATT_COM_CODE_EPCI, basename="ville_code", tp="cat"),
		list(name=COL_ATT_COM_NOM_EPCI, basename="ville_nom", tp="nom"),
		list(name=COL_ATT_EPCI_SIREN, basename="epci_siren", tp="cat"),
		list(name=COL_ATT_EPCI_NOM, basename="epci_nom", tp="nom"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_CR, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_EPCI, cols, FILE_CORREC_EPCI)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for mayors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.m.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_DPT_CODE_M, basename="dpt_code", tp="cat"),
		list(name=COL_ATT_DPT_NOM_M, basename="dpt_nom", tp="nom"),
		list(name=COL_ATT_COM_CODE, basename="ville_code", tp="cat"),
		list(name=COL_ATT_COM_NOM, basename="ville_nom", tp="nom"),
		list(name=COL_ATT_COM_POP, basename="ville_population", tp="num"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_NOM, basename="mandat_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_CM, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_M, cols, FILE_CORREC_M)
	
	# convert population numbers to actual integers
	cn <- COL_ATT_COM_POP
	vals <- data[,cn]
	vals <- gsub("\\s", "",  vals)	# \\s matches all whitespaces
	vals <- gsub(",00", "",  vals)
	vals <- suppressWarnings(as.integer(vals))
	data <- data[, names(data)!=cn]
	data <- cbind(data,vals)
	names(data)[ncol(data)] <- cn
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for senators.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.s.data <- function()
{	# names of the columns
	cols <- list(
		list(name=COL_ATT_DPT_CODE, basename="dpt_code", tp="cat"),
		list(name=COL_ATT_DPT_NOM, basename="dpt_nom", tp="nom"),
		list(name=COL_ATT_ELU_NOM, basename="patronyme", tp="nom"),
		list(name=COL_ATT_ELU_PRENOM, basename="prenom", tp="nom"),
		list(name=COL_ATT_ELU_DDN, basename="naissance_date", tp="dat"),
		list(name=COL_ATT_ELU_SEXE, basename="sexe", tp="cat"),
		list(name=COL_ATT_PRO_CODE, basename="profession_code", tp="cat"),
		list(name=COL_ATT_PRO_NOM, basename="profession_nom", tp="cat"),
		list(name=COL_ATT_MDT_NOM, basename="mandat_nom", tp="cat"),
		list(name=COL_ATT_MDT_DBT, basename="mandat_debut", tp="dat"),
		list(name=COL_ATT_MDT_FIN, basename="mandat_fin", tp="dat"),
		list(name=COL_ATT_MDT_MOTIF, basename="mandat_motif", tp="cat"),
		list(name=COL_ATT_FCT_NOM, basename="fonction_nom", tp="cat"),
		list(name=COL_ATT_FCT_DBT, basename="fonction_debut", tp="dat"),
		list(name=COL_ATT_FCT_FIN, basename="fonction_fin", tp="dat"),
		list(name=COL_ATT_FCT_MOTIF, basename="fonction_motif", tp="cat"),
		list(name=COL_ATT_ELU_NUANCE_S, basename="nuance_pol", tp="cat"),
		list(name=COL_ATT_ELU_ID, basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(FILES_TAB_S, cols, FILE_CORREC_S)
	
	res <- list(data=data,cols=cols)
	return(res)
}
