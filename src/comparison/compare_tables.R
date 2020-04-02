#############################################################################################
# Compares two text files containing two tables, row by row.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Extract the year from the string representation of a date. This Function us used to compare 
# the years of dates represented as strings.
# 
# str: vector of dates represented as strings.
#
# returns: the substring corresponding to the year.
#############################################################################################
extract.year <- function(str) 
{	if(grepl("/", str[1], fixed=TRUE))
		substr(str, nchar(str)-3, nchar(str))
	else
		sapply(strsplit(str, "-"), function(vect) vect[1])
}




#############################################################################################
# Extract the month from the string representation of a date. This Function us used to compare 
# the months of dates represented as strings.
# 
# str: vector of dates represented as strings.
#
# returns: the substring corresponding to the month.
#############################################################################################
extract.month <- function(str) 
{	if(grepl("/", str[1], fixed=TRUE))
		sapply(strsplit(str, "/"), function(vect) vect[2])
	else
		sapply(strsplit(str, "-"), function(vect) vect[2])
}




#############################################################################################
# Extract the day from the string representation of a date. This Function us used to compare 
# the days of dates represented as strings.
# 
# str: vector of dates represented as as strings.
#
# returns: the substring corresponding to the day.
#############################################################################################
extract.day <- function(str) 
{	if(grepl("/", str[1], fixed=TRUE))
		sapply(strsplit(str, "/"), function(vect) vect[1])
	else
		sapply(strsplit(str, "-"), function(vect) vect[3])
}




#############################################################################################
# Looks for a row of the first table in the second table, performing comparison based on certain
# columns only. 
# 
# tab1: first table.
# i: row number in the first table.
# tab2: second table.
# map: row numbers in the second table, which are already matched to a row in the first table.
#	   These cannot be matched to another row of the first table.
#
# returns: the number of the similar row in the second table, or NULL if none was found.
#############################################################################################
lookup.row <- function(tab1, i, tab2, map)
{	if(COL_ATT_ELU_ID %in% colnames(tab1) && COL_ATT_ELU_ID %in% colnames(tab2))
	{	id.col.name <- COL_ATT_ELU_ID
	}else
		id.col.name <- COL_ATT_ELU_ID_RNE
	idx <- which((tab2[,id.col.name]==tab1[i,id.col.name] 											# same person id
						| tab2[,COL_ATT_ELU_PRENOM]==tab1[i,COL_ATT_ELU_PRENOM] 					# or same name
						& tab2[,COL_ATT_ELU_NOM]==tab1[i,COL_ATT_ELU_NOM])							# and first name
					& extract.year(tab2[,COL_ATT_MDT_DBT])==extract.year(tab1[i,COL_ATT_MDT_DBT]))	# same mandate start year
	if(hasArg(map))
		idx <- setdiff(idx,map)
	
	# if several matches, consider the start month
	if(length(idx)>1)
	{	idx2 <- which(extract.month(tab2[idx,COL_ATT_MDT_DBT])==extract.month(tab1[i,COL_ATT_MDT_DBT]))	# having a look at the month
		idx <- idx[idx2]
		
		# if several matches, consider the start day
		if(length(idx)>1)
		{	idx2 <- which(extract.day(tab2[idx,COL_ATT_MDT_DBT])==extract.day(tab1[i,COL_ATT_MDT_DBT]))	# having a look at the day
			idx <- idx[idx2]
			
			# if several matches, consider the end date
			if(length(idx)>1)
			{	# if the end date is NA
				if(is.na(tab1[i,COL_ATT_MDT_FIN]))
				{	idx2 <- which(is.na(tab2[idx,COL_ATT_MDT_FIN]))
					idx <- idx[idx2]
				}
				
				# otherwise, consider the end year
				else
				{	idx2 <- which(extract.year(tab2[idx,COL_ATT_MDT_FIN])==extract.year(tab1[i,COL_ATT_MDT_FIN]))	# having a look at the year
					idx <- idx[idx2]
					
					# if several matches, consider the end month
					if(length(idx)>1)
					{	idx2 <- which(extract.month(tab2[idx,COL_ATT_MDT_FIN])==extract.month(tab1[i,COL_ATT_MDT_FIN]))	# having a look at the month
						idx <- idx[idx2]
						
						# if several matches, consider the end day
						if(length(idx)>1)
						{	idx2 <- which(extract.day(tab2[idx,COL_ATT_MDT_FIN])==extract.day(tab1[i,COL_ATT_MDT_FIN]))	# having a look at the day
							idx <- idx[idx2]
						}
					}
				}
				
				# if several matches, be more strict on the id
				if(length(idx)>1)
				{	idx2 <- which(tab2[idx,id.col.name]==tab1[i,id.col.name])
					idx <- idx[idx2]
					
					# if several matches, check the motivation to end the mandate
					if(length(idx)>1)
					{	idx2 <- which(tab2[idx,COL_ATT_MDT_MOTIF]==tab1[i,COL_ATT_MDT_MOTIF])
						idx <- idx[idx2]
						
						# otherwise
						if(length(idx)>1)
						{	tlog(4,"Found several matches: comparison too loose (",paste(idx,collapse=", "),")")
							tlog(6,"tab1[",i,",]: ",paste(tab1[i,],collapse=", "))
							for(idx0 in idx)
								tlog(6,"tab2[",idx0,",]: ",paste(tab2[idx0,],collapse=", "))
							print(idx)
							print(rbind(tab1[i,],tab2[idx,]))
#							stop("Several rows match: the criteria should be revised")
							# nope, at this stage this just means the same row is repeated several times, 
							# so we just take the first one and the others will just be detected as extra rows later
							idx <- idx[1]
						}
					}
				}
			}
		}
	}
	
	return(idx)
}




#############################################################################################
# Force-read the specified files as a string table.
#
# files: vector of files constituting the table. It is assumed they all contain the same columns.
#
# returns: the obtained string table.
#############################################################################################
read.string.table <- function(files, ...)
{	# read each file
	res <- NULL
	for(file in files)
	{	tlog(4,"Reading file ",file)
		temp <- read.table(
				file=file, 					# name of the data file
				header=TRUE, 				# look for a header
				sep="\t", 					# character used to separate columns 
				check.names=FALSE, 			# don't change the column names from the file
				comment.char="", 			# ignore possible comments in the content
				row.names=NULL, 			# don't look for row names in the file
				quote="", 					# don't expect double quotes "..." around text fields
				as.is=TRUE,					# don't convert strings to factors
#				fileEncoding="Latin1",		# original tables seem to be encoded in Latin1 (ANSI)
				colClasses="character",		# forces to consider everything column as strings
				...
		)
		tlog(6,"Read ",nrow(temp)," rows and ",ncol(temp)," columns")
		
		# add to the main table
		tlog(6,"Adding to main table")
		if(all(is.null(res)))
			res <- temp
		else
			res <- rbind(res,temp)
		tlog(6,"Now ",nrow(res)," rows and ",ncol(res)," columns in main table")
	}
	
	# clean each column
	for(c in 1:ncol(res))
	{	# trim leading/trailing whitespaces
		res[,c] <- trimws(res[,c])
		# replace empty strings by NAs
		res[which(res[,c]==""),c] <- NA
	}
	
	# normalize column names
	col.map <- c()
	col.map["Circonscription électorale"] <- COL_ATT_DPT_CODE
	col.map["Code de la cir.législative"] <- COL_ATT_CIRC_CODE
	col.map["Code de la commune"] <- COL_ATT_COM_CODE
	col.map["Code département commune rattachée"] <- COL_ATT_DPT_CODE
	col.map["Code département EPCI"] <- COL_ATT_EPCI_DPT
	col.map["Code du canton"] <- COL_ATT_CANT_CODE
	col.map["Code du département (Maire)"] <- COL_ATT_DPT_CODE
	col.map["Code du département"] <- COL_ATT_DPT_CODE
	col.map["Code Insee de la commune"] <- COL_ATT_COM_CODE
	col.map["Code profession"] <- COL_ATT_PRO_CODE
	col.map["Code région"] <- COL_ATT_REG_CODE
	col.map["Code sexe"] <- COL_ATT_ELU_SEXE
	col.map["CodeCirER"] <- COL_ATT_CIRCE_CODE
	col.map["Date de début de la fonction"] <- COL_ATT_FCT_DBT
	col.map["Date de début du mandat"] <- COL_ATT_MDT_DBT
	col.map["Date de fin de la fonction"] <- COL_ATT_FCT_FIN
	col.map["Date de fin du mandat"] <- COL_ATT_MDT_FIN
	col.map["Date de naissance"] <- COL_ATT_ELU_NAIS_DATE
	col.map["Fonction pour un mandat (Code)"] <- COL_ATT_FCT_CODE
	col.map["Libellé commune rattachée"] <- COL_ATT_COM_NOM
	col.map["Libellé de département (Maires)"] <- COL_ATT_DPT_NOM
	col.map["Libellé de département"] <- COL_ATT_DPT_NOM
	col.map["Libellé de fonction"] <- COL_ATT_FCT_NOM
	col.map["Libellé de la cir.législative"] <- COL_ATT_CIRC_NOM
	col.map["Libellé de la profession"] <- COL_ATT_PRO_NOM
	col.map["Libellé de la région"] <- COL_ATT_REG_NOM
	col.map["Libellé de l'EPCI"] <- COL_ATT_EPCI_NOM
	col.map["Libellé de mandat"] <- COL_ATT_MDT_NOM
	col.map["Libellé du canton"] <- COL_ATT_CANT_NOM
	col.map["Libellé du département"] <- COL_ATT_DPT_NOM
	col.map["LibelléCirER"] <- COL_ATT_CIRCE_NOM
	col.map["Motif de fin de fonction"] <- COL_ATT_FCT_MOTIF
	col.map["Motif de fin de mandat"] <- COL_ATT_MDT_MOTIF
	col.map["N° Identification d'un élu"] <- COL_ATT_ELU_ID_RNE
	col.map["N° SIREN"] <- COL_ATT_EPCI_SIREN
	col.map["Nationalité de l'élu"] <- COL_ATT_ELU_NAT
	col.map["Nom de l'élu"] <- COL_ATT_ELU_NOM
	col.map["Nuance mandat"] <- COL_ATT_ELU_NUANCE
	col.map["Nuance politique (C. Gén.)"] <- COL_ATT_ELU_NUANCE
	col.map["Nuance politique (C. Mun.)"] <- COL_ATT_ELU_NUANCE
	col.map["Nuance politique (Député)"] <- COL_ATT_ELU_NUANCE
	col.map["Nuance politique (Rep. P.E.)"] <- COL_ATT_ELU_NUANCE
	col.map["Nuance politique (Sénateur)"] <- COL_ATT_ELU_NUANCE
	col.map["Population de la commune"] <- COL_ATT_COM_POP
	col.map["Prénom de l'élu"] <- COL_ATT_ELU_PRENOM
	col.map["Région"] <- COL_ATT_REG_NOM
	
	# normalize column names
	for(i in 1:ncol(res))
	{	if(colnames(res)[i] %in% names(col.map))
			colnames(res)[i] <- col.map[colnames(res)[i]]
	}
	
	return(res)
}




#############################################################################################
# Compares two tables using only their raw content, considered as text. All the detected
# differences are recorded in a text file.
#
# files0: vector of files constituting the first table. All columns must be the same.
# files1: vector of files constituting the second table. All columns must be the same.
# out.folder: folder used to record the comparison files.
#############################################################################################
compare.tables <- function(files0, files1, out.folder, ...)
{
	##################
	# read everything as strings to avoid any conversion-related issue
	tlog(2,"Reading table v0")
	t0 <- read.string.table(files0, ...)
	tlog(2,"Reading table v1")
	t1 <- read.string.table(files1, ...)
	
	# only work with columns common to both tables
	cn0 <- colnames(t0)
	cn1 <- colnames(t1)
	ccn <- intersect(cn0,cn1)
	tlog(2,"Number of columns: t0=",length(cn0)," t1=",length(cn1)," common=",length(ccn))
	del.cols <- setdiff(cn0,cn1)
	added.cols <- setdiff(cn1,cn0)
	tab <- NA
	if(length(del.cols)>0)
	{	tlog(4,"Ignored t0 columns: ",paste(del.cols,collapse=", "))
		tab <- cbind(del.cols,rep("Suppression",length(del.cols)))
	}
	if(length(added.cols)>0)
	{	tlog(4,"Ignored t1 columns: ",paste(added.cols,collapse=", "))
		if(all(is.na(tab)))
			tab <- cbind(added.cols,rep("Insertion",length(added.cols)))
		else
			tab <- rbind(tab, cbind(added.cols,rep("Insertion",length(added.cols))))
	}
	
	# record added/removed columns
	if(!all(is.na(tab)))
	{	colnames(tab) <- c("Colonne","Etat")
		table.file <- file.path(out.folder, "_column_differences.txt")
		write.table(x=tab,
			file=table.file,		# name of file containing the new table
#			quote=TRUE,				# put double quotes around strings
			sep="\t",				# use tabulations as separators
			row.names=FALSE,		# no names for rows
			col.names=TRUE			# record table headers
		)
	}
	
	# display row counts
	tlog(2,"Numbers of rows: ",nrow(t0)," vs ",nrow(t1)," (delta: ",(nrow(t1)-nrow(t0)),")")

	##################
	# compare both tables
	t2k <- t1[FALSE,]		# kepts entries
	t2d <- t1[FALSE,]		# deleted entries
	t2a <- t1[FALSE,]		# added entries
	mp0 <- rep(NA,nrow(t0))
	mp1 <- rep(NA,nrow(t1))
	
	# scan table 0
	tlog(2,"Parsing the rows of table 0")
	for(i0 in 1:nrow(t0))
	{	tlog(4,"Processing row ",i0,"/",nrow(t0))
		# cols <- c(COL_ATT_ELU_ID,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)
		# print(rbind(t0[c(5232),cols],t1[c(5244,5245),cols]))
		
		# check whether rows are similar
		idx <- lookup.row(tab1=t0[,ccn],i=i0,tab2=t1[,ccn],map=mp0)
		if(length(idx)==1)
		{	tlog(6,"Found in t1: add to map for latter use")
			mp0[i0] <- idx
		}
		else if(length(idx)>1)
		{	tlog(6,"Found several times in t1 but could not match the month: check manually (",paste(idx,collapse=","),")")
			stop("Several rows match in t1: the criteria should be revised")			
		}
		else
		{	tlog(6,"Not found in t1: deleted row")
			t2d <- rbind(t2d,t0[i0,])
		}
	}
	
	# scan table 1
	tlog(2,"Parsing the rows of table 1")
	for(i1 in 1:nrow(t1))
	{	tlog(4,"Processing row ",i1,"/",nrow(t1))
		
		# check whether rows are similar
		if(i1 %in% mp0)
		{	tlog(6,"Found in t0: kept row")
			t2k <- rbind(t2k,t1[i1,])
			mp1[i1] <- which(mp0==i1)
		}
		else
		{	tlog(6,"Not found in t0: added row")
			t2a <- rbind(t2a,t1[i1,])
		}
	}
	tlog(2,"Tables processed")
	
	# record all resulting tables
	tlog(4,"Similar rows: ",nrow(t2k))
	tab <- cbind(which(!is.na(mp0)),mp0[!is.na(mp0)],t2k)
	colnames(tab)[1:2] <- c("Ligne vx", "Ligne nv")
	table.file <- file.path(out.folder, "_similar_rows.txt")
	write.table(x=tab,
		file=table.file,		# name of file containing the new table
		quote=TRUE,				# put double quotes around strings
		sep="\t",				# use tabulations as separators
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	tlog(4,"Added rows: ",nrow(t2a))
	tab <- cbind(setdiff(1:nrow(t1),mp0[!is.na(mp0)]),t2a)
	colnames(tab)[1] <- c("Ligne nv")
	table.file <- file.path(out.folder, "_added_rows.txt")
	write.table(x=tab,
		file=table.file,		# name of file containing the new table
		quote=TRUE,				# put double quotes around strings
		sep="\t",				# use tabulations as separators
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	tlog(4,"Removed rows: ",nrow(t2d))
	tab <- cbind(which(is.na(mp0)),t2d)
	colnames(tab)[1] <- c("Ligne vx")
	table.file <- file.path(out.folder, "_removed_rows.txt")
	write.table(x=tab,
		file=table.file,		# name of file containing the new table
		quote=TRUE,				# put double quotes around strings
		sep="\t",				# use tabulations as separators
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	
	##################
	# compare each field in the rows common to both tables
	tlog(2,"Comparing values in common rows")
	idx0 <- which(!is.na(mp0))
	idx1 <- mp0[!is.na(mp0)]
	comp <- 1*((is.na(t0[idx0,ccn]) & is.na(t1[idx1,ccn])) | 
				(!is.na(t0[idx0,ccn]) & !is.na(t1[idx1,ccn]) & t0[idx0,ccn]==t1[idx1,ccn]))
	counts <- apply(X=comp, MARGIN=2, FUN=sum)
	mod.nbr <- sum(nrow(comp)-counts)
	tlog(4,"Total number of modified values: ",mod.nbr,"/",(nrow(comp)*ncol(comp))," (",(mod.nbr/nrow(comp)/ncol(comp)),"%)")
	counts <- rbind(counts, nrow(comp)-counts)
	rownames(counts) <- c("Same", "Different")
	# record as a table
	table.file <- file.path(out.folder, "_modified_values.txt")
	write.table(x=counts,
		file=table.file,		# name of file containing the new table
		quote=TRUE,				# put double quotes around strings
		sep="\t",				# use tabulations as separators
		row.names=TRUE,			# record row names
		col.names=TRUE			# record table headers
	)
	
	# record each modified value
	tlog(2,"Processing each column separately")
	for(c in ccn)
	{	tlog(4,"Processing column ",c)
		if(c %in% colnames(comp))
		{	# retrieve data
			idx <- which(comp[,c]==0)
			tlog(6,"Found ",length(idx)," values")
			if(length(idx)>0)
			{	tab <- data.frame(
						"Ligne vx"=idx0[idx],
						"Ligne nv"=idx1[idx],
						"Valeur vx"=t0[idx0[idx],c],
						"Valeur nv"=t1[idx1[idx],c]
					)
				# record in text file
				table.file <- file.path(out.folder, paste0(BASENAMES[c],"_changes.txt"))
				tlog(6,"Recording changes in file ",table.file)
				write.table(x=tab,
					file=table.file,		# name of file containing the new table
					quote=TRUE,				# put double quotes around strings
					sep="\t",				# use tabulations as separators
					row.names=FALSE,		# no names for rows
					col.names=TRUE			# record table headers
				)
			}
		}
	}
}
