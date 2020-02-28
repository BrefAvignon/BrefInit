#############################################################################################
# Functions used to inject data from the Senate database into the RNE.
# 
# 02/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Reads a standard conversion table from a text file.
#
# file: path and name of the file containing the table.
#
# returns: the read conversion table.
#############################################################################################
senate.load.conversion.file <- function(file)
{	tlog(6,"Loading Senate conversion file ",file)
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
# Loads the Senate table containing the description of individuals, and converts/normalizes
# everything that needs to be. This function uses a cache system in order to speed up the process.
#
# cache: whether or not to use the cache system.
# 
# returns: the loaded table.
#############################################################################################
senate.load.general.table <- function(cache)
{	tlog(2,"Loading the Senate general table ")
	
	# possibly use the cached file
	if(cache & file.exists(FILE_SENAT_GENERAL_CACHE))
	{	tlog(4,"Cache enabled and found cached file >> using cache (",FILE_SENAT_GENERAL_CACHE,")")
		result <- read.table(
			file=FILE_SENAT_GENERAL_CACHE,	# name of the data file
			header=TRUE, 					# look for a header
			sep="\t", 						# character used to separate columns 
			check.names=FALSE, 				# don't change the column names from the file
			comment.char="", 				# ignore possible comments in the content
			row.names=NULL, 				# don't look for row names in the file
#			quote="", 						# don't expect double quotes "..." around text fields
			as.is=TRUE,						# don't convert strings to factors
#			colClasses="character"			# all column originally read as characters, then converted later if needed
#			fileEncoding="Latin1"			# original tables seem to be encoded in Latin1 (ANSI)
		)
	}
	
	# no cache or don't use cache
	else
	{	tlog(4,"Cache disabled or no cached file >> processing the file")
		
		# load individual table
		tlog(4,"Read raw file ",FILE_SENAT_GENERAL)
		indiv.table <- read.table(
			file=FILE_SENAT_GENERAL,	# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE,					# don't convert strings to factors
			colClasses="character"		# all column originally read as characters, then converted later if needed
		#	fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		for(c in 1:ncol(indiv.table))
			indiv.table[which(indiv.table[,c]==""),c] <- NA
		
		tlog(4,"Normalizing content")
		# normalize last names
		indiv.last.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_NOM])))
		lastname.conv <- senate.load.conversion.file(FILE_SENAT_CONV_NOMSFAM)
		idx <- match(indiv.last.names, lastname.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.last.names[idx2] <- lastname.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# normalize first names
		indiv.first.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_PRENOM])))
		firstname.conv <- senate.load.conversion.file(FILE_SENAT_CONV_PRENOMS)
		idx <- as.integer(firstname.conv[, COL_CORREC_ROW])
		indiv.first.names[idx] <- firstname.conv[, COL_CORREC_VALAPR]
		
		# init sex
		indiv.sex <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_QUALI])))
		indiv.sex[indiv.sex=="M"] <- "M"
		indiv.sex[indiv.sex=="MME" | indiv.sex=="MLLE"] <- "F"
		
		# init nationality
		indiv.nationality <- rep("FRANCAISE", nrow(indiv.table))
		# NOTE all senators must be French, by law
		
		# convert birth/death dates
		indiv.birth.dates <- as.Date(indiv.table[,COL_SENAT_ELU_DDN], "%d/%m/%Y")
		indiv.death.dates <- as.Date(indiv.table[,COL_SENAT_ELU_DDD], "%d/%m/%Y")
	
		# normalize department names
		indiv.dpt.names <- normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_DPT_NOM]))
		dpts.conv <- senate.load.conversion.file(FILE_SENAT_CONV_DPTS)
		idx <- match(indiv.dpt.names, dpts.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.dpt.names[idx2] <- dpts.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve department codes and ids
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
		indiv.dpt.idx <- match(indiv.dpt.names, dpt.table[,COL_ATT_DPT_NOM])
		indiv.dpt.codes <- sapply(dpt.table[indiv.dpt.idx,COL_ATT_DPT_CODE], function(x) 
				{	str <- strsplit(x, ",", fixed=TRUE)[[1]]
					str[length(str)]
				})
		indiv.dpt.ids <- dpt.table[indiv.dpt.idx,COL_ATT_DPT_ID]
		
		# retrieve political group
		# NOTE: stored in the general table, so associated to each individual by opposition to each *mandate*
		indiv.pol.nuances <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_NUANCE])))
		nuances.conv <- senate.load.conversion.file(FILE_SENAT_CONV_NUANCES)
		idx <- match(indiv.pol.nuances, nuances.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.pol.nuances[idx2] <- nuances.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve occupations names
		indiv.occ.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_PRO_NOM])))
		occup.conv <- senate.load.conversion.file(FILE_SENAT_CONV_PRO)
		idx <- match(indiv.occ.names, occup.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.occ.names[idx2] <- occup.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve occupations codes
		# NOTE we could retrieve codes from the merged main table, but is it really necessary?
		indiv.occ.codes <- rep(NA,nrow(indiv.table))
		
		# retrieve functions
		indiv.fonct.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_FCT_BUR])))
		# NOTE "function" also seems to be available under a dynamic form in the "Groupes" table
#		indiv.fonct.names <- rep(NA,nrow(indiv.table))
		
		##################### cross referencing with RNE to get ids
		# load RNE senate table
		tlog(4,"Loading RNE Senate table")
		data <- load.s.data(correct.data=TRUE, complete.data=FALSE)
		
		# match senate ids with RNE ids
		tlog(4,"Matching Senate and RNE people ids")
		indiv.ids.rne <- rep(NA, nrow(indiv.table))
		indiv.ids.univ <- paste0("SEN_0",indiv.table[,COL_SENAT_ELU_MATRI])
		for(i in 1:nrow(indiv.table))
		{	tlog(6, "Processing senator ",indiv.ids.univ[i]," (",i,"/",nrow(indiv.table),")")
			tmp <- which(data[,COL_ATT_ELU_NOM]==indiv.last.names[i]
							& data[,COL_ATT_ELU_PRENOM]==indiv.first.names[i]
							& data[,COL_ATT_ELU_NAIS_DATE]==indiv.birth.dates[i])
#							& data[,COL_ATT_DPT_NOM]==indiv.dpt.names[i])
			if(length(tmp)==0)
				tlog(6, "No match for senator: ",paste(indiv.table[i,],collapse=","))
			else 
			{	tmp.ids <- sort(unique(data[tmp,COL_ATT_ELU_ID_RNE]))
				if(length(tmp.ids)>1)
				{	tlog(8, "Found several matches (ids ",paste(tmp.ids,collapse=","),") for senator: ",paste(indiv.table[i,],collapse=","))
					# display more details?
					stop("Found several matches")
				}
				else
				{	tlog(8, "Found a single RNE entry (id ",tmp.ids,", name ",
							paste(data[tmp[1],c(COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_NAIS_DATE)],collapse=","),
							") for senator: ",paste(indiv.table[i,],collapse=","))
					indiv.ids.rne[i] <- tmp.ids
					indiv.ids.univ[i] <- data[tmp[1],COL_ATT_ELU_ID]
				}	
			}
		}
		
#		# match RNE ids with senate ids (just for checking)
#		rne.ids <- sort(unique(data[,COL_ATT_ELU_ID]))
#		rne.rows <- match(rne.ids,data[,COL_ATT_ELU_ID])
#		sen.ids <- rep(NA,length(rne.ids))
#		for(i in 1:length(rne.ids))
#		{	tlog(2, "Processing row ",i,"/",length(rne.ids))
#			tmp <- which(indiv.last.names==data[rne.rows[i],COL_ATT_ELU_NOM]
#					& indiv.first.names==data[rne.rows[i],COL_ATT_ELU_PRENOM]
#					& indiv.birth.dates==data[rne.rows[i],COL_ATT_ELU_NAIS_DATE])
##					& indiv.dpt.names==data[rne.rows[i],COL_ATT_DPT_NOM])
#			if(length(tmp)==0)
#				tlog(4, "No match for row: ",paste(data[rne.rows[i],],collapse=","))
#			else 
#			{	tmp.ids <- sort(unique(indiv.table[tmp,COL_SENAT_ELU_MATRI]))
#				if(length(tmp.ids)>1)
#				{	tlog(4, "Found several matches (ids ",paste(tmp.ids,collapse=","),") for row: ",paste(data[rne.rows[i],],collapse=","))
#					# display more details?
#				}
#				else
#				{	tlog(4, "Found a single Senate entry (id ",tmp.ids,", name ",
#							paste(indiv.last.names[tmp[1]],indiv.first.names[tmp[1]],indiv.birth.dates[tmp[1]],sep=","),
#							") for row: ",paste(indiv.table[i,],collapse=","))
#					sen.ids[i] <- tmp.ids
#				}	
#			}
#		}
		
		##################### build clean table
		tlog(4,"Building clean individual table")
		result <- data.frame(
			indiv.dpt.ids,							# department id
			indiv.dpt.codes,						# department code
			indiv.dpt.names,						# department name
			indiv.ids.univ,							# universal id
			indiv.ids.rne,							# RNE id
			indiv.table[,COL_SENAT_ELU_MATRI],		# senate id
			indiv.last.names,						# last name
			indiv.first.names,						# first name
			indiv.birth.dates,						# birth date
			indiv.death.dates,						# death date
			indiv.sex,								# sex
			indiv.nationality,						# country
			indiv.pol.nuances,						# political group
			indiv.occ.codes,						# occupation code
			indiv.occ.names,						# occupation name
			indiv.fonct.names,						# function name
			#
			check.names=FALSE,
			stringsAsFactors=FALSE
		)
		colnames(result) <- c(
			COL_ATT_DPT_ID,
			COL_ATT_DPT_CODE,
			COL_ATT_DPT_NOM,
			COL_ATT_ELU_ID,
			COL_ATT_ELU_ID_RNE,
			COL_ATT_ELU_ID_SENAT,
			COL_ATT_ELU_NOM,
			COL_ATT_ELU_PRENOM,
			COL_ATT_ELU_NAIS_DATE,
			COL_ATT_ELU_DDD,
			COL_ATT_ELU_SEXE,
			COL_ATT_ELU_NAT,
			COL_ATT_ELU_NUANCE,
			COL_ATT_PRO_CODE,
			COL_ATT_PRO_NOM,
			COL_ATT_FCT_NOM
		)
		
		# record the table for later use
		#if(cache)
		{	tlog(4,"Caching the resulting table in file ",FILE_SENAT_GENERAL_CACHE)
			write.table(x=result,
				file=FILE_SENAT_GENERAL_CACHE,	# name of file containing the new table
				quote=TRUE,						# put double quotes around strings
				se="\t",						# use tabulations as separators
				row.names=FALSE,				# no names for rows
				col.names=TRUE					# record table headers
			)
		}
	}
	
	tlog(4,"General file loaded")
	return(result)
}




#############################################################################################
# Loads the Senate table containing the description of mandates.
#
# type: the targeted type of mandate (CD, CM, etc.).
# 
# returns: the loaded table.
#############################################################################################
senate.load.elect.table <- function(type)
{	# get the appropriate senate data file
	sen.data.file <- c()
	sen.data.file["CD"] <- FILE_SENAT_ELEC_CD
	sen.data.file["CM"] <- FILE_SENAT_ELEC_CM
	sen.data.file["CR"] <- FILE_SENAT_ELEC_CR
	sen.data.file["D"] <- FILE_SENAT_ELEC_S
	sen.data.file["DE"] <- FILE_SENAT_ELEC_DE
	sen.data.file["S"] <- FILE_SENAT_ELEC_S
	
	tlog(2,"Loading the Senate madate table: ",sen.data.file)
	
	# load the corresponding senate mandate table(s)
	elect.table <- read.table(
		file=sen.data.file[type],	# name of the data file
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
	
	# set empty cells to NA
	tlog(4,"Cleaning the table")
	for(c in 1:ncol(elect.table))
		elect.table[which(elect.table[,c]==""),c] <- NA
	
	return(elect.table)
}




#############################################################################################
# Convert a data table originating from the Senate DB into one comparable to a RNE table.
#
# general.table: table containing the personal data.
# elect.table: table containing the mandate data.
# type: nature of the table (CD, CM, CR, D, DE, M, S).
# 
# returns: the table resulting from the conversion.
#############################################################################################
senate.convert.mandate.table <- function(general.table, elect.table, type)
{	tlog(2,"Converting the Senate mandate table")
	
	# for mayors, only keep the appropriate mandates
	if(type=="M")
	{	function.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_FCT_NOM])))
		# we only keep the mayors from the municipal counselor table
		elect.table <- elect.table[which(function.names=="MAIRE"),]
	}
	
	# match the general and mandate tables
	idx <- match(elect.table[,COL_SENAT_ELU_MATRI], general.table[,COL_ATT_ELU_ID_SENAT])
	
	# convert mandate dates
	tlog(4,"Converting mandate dates")
	mdt.start.dates <- as.Date(elect.table[,COL_SENAT_MDT_DBT], "%d/%m/%Y")
	mdt.end.dates <- as.Date(elect.table[,COL_SENAT_MDT_FIN], "%d/%m/%Y")
	
	# mandate name
	tlog(4,"Inserting mandate names")
	if(type=="CD")
		mdt.names <- rep("CONSEILLER DEPARTEMENTAL", length(idx))
	else if(type=="CM" || type=="M")
		mdt.names <- rep("CONSEILLER MUNICIPAL", length(idx))
	else if(type=="CR")
		mdt.names <- rep("CONSEILLER REGIONAL", length(idx))
	else if(type=="D")
		mdt.names <- rep("DEPUTE", length(idx))
	else if(type=="DE")
		mdt.names <- rep("REPRESENTANT AU PARLEMENT EUROPEEN", length(idx))
	else if(type=="S")
		mdt.names <- rep("SENATEUR", length(idx))
	
	# possibly get motive of end of mandate
	tlog(4,"Processing end of mandate motive")
	mdt.motives <- rep(NA,length(idx))
	if(COL_SENAT_MDT_MOTIF %in% colnames(elect.table))
		mdt.motives <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_MDT_MOTIF])))
	
	# possibly get and convert function dates
	tlog(4,"Processing function dates")
	fct.start.dates <- rep(NA, length(idx))
	if(COL_SENAT_FCT_DBT %in% colnames(elect.table))
		fct.start.dates <- as.Date(elect.table[,COL_SENAT_FCT_DBT], "%d/%m/%Y")
	fct.end.dates <- rep(NA, length(idx))
	if(COL_SENAT_FCT_FIN %in% colnames(elect.table))
		fct.end.dates <- as.Date(elect.table[,COL_SENAT_FCT_FIN], "%d/%m/%Y")
	
	# possibly get function name
	tlog(4,"Processing function names")
	function.names <- general.table[idx,COL_ATT_FCT_NOM]
	if(COL_SENAT_FCT_NOM %in% colnames(elect.table))
	{	function.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_FCT_NOM])))
		# these are not considered as functions in the RNE
		if(type=="CD")
			function.names[which(function.names=="CONSEILLER DEPARTEMENTAL" | function.names=="CONSEILLER GENERAL")] <- NA
		else if(type=="CM")
			function.names[which(function.names=="CONSEILLER MUNICIPAL")] <- NA
		else if(type=="CR")
			function.names[which(function.names=="CONSEILLER REGIONAL")] <- NA
		else if(type=="D")
			function.names[which(function.names=="DEPUTE")] <- NA
	}
	
	# build senate table
	tlog(4,"Building new Senate mandate table")
	sen.tab <- data.frame(
		general.table[idx,COL_ATT_ELU_ID],			# universal id
		general.table[idx,COL_ATT_ELU_ID_RNE],		# RNE id
		general.table[idx,COL_ATT_ELU_ID_SENAT],	# senate id
		general.table[idx,COL_ATT_ELU_NOM],			# last name
		general.table[idx,COL_ATT_ELU_PRENOM],		# first name
		general.table[idx,COL_ATT_ELU_NAIS_DATE],	# birth date
		general.table[idx,COL_ATT_ELU_DDD],			# death date
		general.table[idx,COL_ATT_ELU_SEXE],		# sex
		general.table[idx,COL_ATT_ELU_NAT],			# country
		general.table[idx,COL_ATT_ELU_NUANCE],		# political group
		general.table[idx,COL_ATT_PRO_CODE],		# occupation code
		general.table[idx,COL_ATT_PRO_NOM],			# occupation name
		mdt.names,									# mandate name
		mdt.start.dates,							# mandate start date
		mdt.end.dates,								# mandate end date
		mdt.motives,								# mandate end motive
		function.names,								# function name
		fct.start.dates,							# function start date
		fct.end.dates,								# function end date
		rep(NA, length(idx)),						# function end motive
		rep("SENAT", length(idx)),					# data source
		#
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
	colnames(sen.tab) <- c(		
		COL_ATT_ELU_ID,
		COL_ATT_ELU_ID_RNE,
		COL_ATT_ELU_ID_SENAT,
		COL_ATT_ELU_NOM,
		COL_ATT_ELU_PRENOM,
		COL_ATT_ELU_NAIS_DATE,
		COL_ATT_ELU_DDD,
		COL_ATT_ELU_SEXE,
		COL_ATT_ELU_NAT,
		COL_ATT_ELU_NUANCE,
		COL_ATT_PRO_CODE,
		COL_ATT_PRO_NOM,
		COL_ATT_MDT_NOM,
		COL_ATT_MDT_DBT,
		COL_ATT_MDT_FIN,
		COL_ATT_MDT_MOTIF,
		COL_ATT_FCT_NOM,
		COL_ATT_FCT_DBT,
		COL_ATT_FCT_FIN,
		COL_ATT_FCT_MOTIF,
		COL_ATT_SOURCES
	)
	
	# possibly add location columns
	place.order <- rep(NA, nrow(sen.tab))
	if(type=="CR")
	{	tlog(4,"Adding region-related columns")
		
		# clean region names
		reg.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_REG_NOM])))
		regs.conv <- senate.load.conversion.file(FILE_SENAT_CONV_REGIONS)
		idx <- match(reg.names, regs.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		reg.names[idx2] <- regs.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve region codes
		reg.table <- read.table(
			file=FILE_CONV_REGIONS,		# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE					# don't convert strings to factors
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		reg.idx <- match(reg.names, reg.table[,COL_ATT_REG_NOM])
		reg.codes <- reg.table[reg.idx, COL_ATT_REG_CODE]
		
		# add to table
		regions <- data.frame(reg.codes, reg.names, stringsAsFactors=FALSE)
		colnames(regions) <- c(COL_ATT_REG_CODE, COL_ATT_REG_NOM)
		sen.tab <- cbind(sen.tab, regions)
		
		# use to order table, later
		place.order <- sen.tab[,COL_ATT_REG_CODE]
	}
	else if(type=="CD")
	{	tlog(4,"Adding department-related columns")
		
		# clean department names
		dpt.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_DPT_NOM2])))
		dpts.conv <- senate.load.conversion.file(FILE_SENAT_CONV_DPTS)
		idx <- match(dpt.names, dpts.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		dpt.names[idx2] <- dpts.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve department codes and ids
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
		dpt.idx <- match(dpt.names, dpt.table[,COL_ATT_DPT_NOM])
		dpt.codes <- sapply(dpt.table[dpt.idx,COL_ATT_DPT_CODE], function(x) 
				{	str <- strsplit(x, ",", fixed=TRUE)[[1]]
					str[length(str)]
				})
		dpt.ids <- dpt.table[dpt.idx, COL_ATT_DPT_ID]
		
		# add departments to table
		departments <- data.frame(dpt.ids, dpt.codes, dpt.names, stringsAsFactors=FALSE)
		colnames(departments) <- c(COL_ATT_DPT_ID, COL_ATT_DPT_CODE, COL_ATT_DPT_NOM) 
		sen.tab <- cbind(sen.tab, departments)
		
		# clean canton names
		cant.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_CANT_NOM])))
		
		# retrieve canton codes and ids
		tlog(4,"Adding canton-related columns")
		cant.table <- read.table(
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
		cant.codes <- rep(NA,length(cant.names))
		cant.ids <- rep(NA,length(cant.names))
		for(i in 1:length(cant.names))
		{	dpt.idx <- which(cant.table[,COL_ATT_DPT_CODE]==dpt.codes[i])
			cant.idx <- dpt.idx[which(cant.table[dpt.idx,COL_ATT_CANT_NOM]==cant.names[i])]
			if(length(cant.idx)!=1)
				stop("Problem")
			cant.codes[i] <- cant.table[i,COL_ATT_CANT_CODE]
			cant.ids[i] <- cant.table[i,COL_ATT_CANT_ID]
		}
		
		# add cantons to table
		cantons <- data.frame(cant.ids, cant.codes, cant.names, stringsAsFactors=FALSE) 
		colnames(cantons) <- c(COL_ATT_CANT_ID, COL_ATT_CANT_CODE, COL_ATT_CANT_NOM)
		sen.tab <- cbind(sen.tab, cantons)
		
		# TODO check whether it is necessery to adjust certain canton names from the CD Senate table
		
		# use to order table, later
		place.order <- paste(COL_ATT_DPT_CODE, sprintf("%02d", as.integer(COL_ATT_CANT_CODE)), sep=":")
	}
	else if(type=="CM" || type=="M")
	{	tlog(4,"Adding municipality-related columns")
		
		# municipalities
		municipality <- data.frame(trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_COM_NOM]))), stringsAsFactors=FALSE)
		sen.tab <- cbind(sen.tab, municipality)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_COM_NOM
		# NOTE we could get the department and other missing columns from the RNE table
		#      but we might also need to check the postgresql version of the senate DB
		# TODO check whether it is necessery to adjust certain municipality names from the CM Senate table
		
		place.order <- sen.tab[,COL_ATT_COM_NOM]
	}
	else if(type=="S")
	{	tlog(4,"Adding department-related columns")
		
		# get the department ids, codes and names
		dpt.ids <- general.table[idx, COL_ATT_DPT_ID]
		dpt.codes <- general.table[idx, COL_ATT_DPT_CODE]
		dpt.names <- general.table[idx, COL_ATT_DPT_NOM]
		
		# add departments to table
		sen.tab <- cbind(sen.tab, dpt.ids, dpt.codes, dpt.names)
		colnames(sen.tab)[(ncol(sen.tab)-2):ncol(sen.tab)] <- c(COL_ATT_DPT_ID, COL_ATT_DPT_CODE, COL_ATT_DPT_NOM)
		
		# use to order table, later
		place.order <- sen.tab[,COL_ATT_DPT_CODE]
	}
	
	# keep only mandates starting before 2001
	if(type!="S")
	{	tlog(4,"We keep only the mandates known (or likely) to end after 2001 (start of the RNE)")
		
		# get the mandate years
		mdt.start.years <- as.integer(elect.table[,COL_SENAT_MDT_ADBT])
		mdt.end.years <- as.integer(elect.table[,COL_SENAT_MDT_AFIN])
		
		# limit date (for start) 
		if(type=="CD")
			limit.date <- as.Date("1994/01/01")
		else if(type=="CM" || type=="M")
			limit.date <- as.Date("2001/01/01")
		else if(type=="CR")
			limit.date <- as.Date("1998/01/01")
		else if(type=="D")
			limit.date <- as.Date("1997/01/01")
		else if(type=="DE")
			limit.date <- as.Date("1999/01/01")
		
		# indices of the discarded mandates
		pre.mandates <- which(sapply(1:nrow(sen.tab), function(i)
				{	# there is an end date
					if(!is.na(mdt.end.dates[i]))
						result <- mdt.end.dates[i]<as.Date("2001/01/01")
					# no end date but an end year
					else if(!is.na(mdt.end.years[i]))
						result <- mdt.end.years[i]<2001
					# no end date/year but a start date
					else if(!is.na(mdt.start.dates[i]))
						result <- mdt.start.dates[i]<limit.date
					# no start date, but a start year
					else if(!is.na(mdt.start.years[i]))
						result <- mdt.start.years[i]<as.integer(get.year(limit.date))
					# no temporal info at all: we just ignore the row
					else
						result <- TRUE
					return(result)
				}))
		
		# update table and vectors
		sen.tab <- sen.tab[-pre.mandates,]
		place.order <- place.order[-pre.mandates]
		mdt.start.years <- mdt.start.years[-pre.mandates]
		mdt.end.years <- mdt.end.years[-pre.mandates]
		fct.start.years <- as.integer(elect.table[-pre.mandates, COL_SENAT_FCT_ADBT])
		fct.end.years <- as.integer(elect.table[-pre.mandates, COL_SENAT_FCT_AFIN])
	}
	
	# order the Senate table
	tlog(4,"Ordering the table rows")
	order.idx <- order(place.order, 
			sen.tab[,COL_ATT_ELU_NOM], sen.tab[,COL_ATT_ELU_PRENOM], 
			sen.tab[,COL_ATT_MDT_DBT], sen.tab[,COL_ATT_MDT_FIN])
	sen.tab <- sen.tab[order.idx,]
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(sen.tab))
	sen.tab <- sen.tab[,norm.cols]
	
	# record the Senate table
	folder <- file.path(FOLDER_COMP_SRC_SEN, type)
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	sen.tab.file <- file.path(folder, "data_senat.txt")
	tlog(4,"Recording the new table in ",sen.tab.file)
	write.table(x=sen.tab,
		file=sen.tab.file,		# name of file containing the new table
		quote=FALSE,			# no double quote around strings
		se="\t",				# use tabulations as separators
#		fileEncoding="UTF-8",	# character encoding
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	
	# add years columns for non-senatorial tables
	if(type!="S")
	{	tlog(4,"Adding years columns")
		sen.tab <- cbind(sen.tab, 
			mdt.start.years[order.idx], mdt.end.years[order.idx], 
			fct.start.years[order.idx], fct.end.years[order.idx])
		colnames(sen.tab)[(ncol(sen.tab)-3):ncol(sen.tab)] <- c(
			COL_SENAT_MDT_ADBT, COL_SENAT_MDT_AFIN, 
			COL_SENAT_FCT_ADBT, COL_SENAT_FCT_AFIN)
	}
	
	return(sen.tab)
}




#############################################################################################
# Matches the rows from the Senate table into the RNE table. The process is quite similar to
# the one permored when comparing several versions of the same table (script compare_tables.R).
#
# sen.tab: senate data.
# rne.tab: RNE data.
# tolerance: number of days for which to dates are still consired equivalent.
# 
# returns: numbers of the RNE rows matching each Senate row (or NA if no RNE row matches, in which
#		   case the senate data is new to the RNE).
#############################################################################################
senate.match.senate.vs.rne.rows <- function(sen.tab, rne.tab, tolerance)
{	senators <- rne.tab[1,COL_ATT_MDT_NOM]=="SENATEUR"
	
	# get all the RNE ids
	ids <- sort(unique(rne.tab[,COL_ATT_ELU_ID]))
	# keep only the senators
	if(!senators)
		ids <- intersect(ids, sen.tab[,COL_ATT_ELU_ID])
	
	tlog(2,"Matching rows between both tables")
	row.conv <- rep(NA,nrow(sen.tab))	# Senate->RNE conversion map
	# compute each RNE id
	for(i in 1:length(ids))
	{	tlog(4,"Processing id ",ids[i]," (",i,"/",length(ids),")")
		
		# get all senate and RNE rows for the current RNE id
		idx.rne <- which(rne.tab[,COL_ATT_ELU_ID]==ids[i])
		idx.sen <- which(sen.tab[,COL_ATT_ELU_ID]==ids[i])
		
		# match each RNE row, possibly correcting its mandate dates
		for(j in 1:length(idx.rne))
		{	idx1 <- idx.rne[j]
			year1 <- get.year(rne.tab[idx1,COL_ATT_MDT_DBT])
			tlog(6,"Processing RNE row ",idx1," (",j,"/",length(idx.rne),")")
			
			# get the Senate row(s) matching the mandate start date
			idx2 <- idx.sen[which(sen.tab[idx.sen,COL_ATT_MDT_DBT]==rne.tab[idx1,COL_ATT_MDT_DBT])]
			
			# if there is exactly one: match done
			if(length(idx2)==1)
			{	tlog(8,"Found 1 matching Senate row, nothing to do:")
				tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
				tlog(10,paste(sen.tab[idx2,],collapse=","),",",format(sen.tab[idx2,COL_ATT_MDT_DBT]))
				row.conv[idx2] <- idx1
			}
			
			# if there are several, we have a problem (manual correction)
			else if(length(idx2)>1)
			{	tlog(8,"Found several (",length(idx2),") matching Senate rows, stopping")
				tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
				for(i2 in idx2)
					tlog(10,paste(sen.tab[i2,],collapse=","),",",format(sen.tab[i2,COL_ATT_MDT_DBT]))
				stop("Found several matching Senate rows")
			}
			
			# if there are none, compare dates in an approximate way
			else if(length(idx2)<1)
			{	failed <- TRUE
				# difference in days between the dates
				gaps <- abs(sen.tab[idx.sen,COL_ATT_MDT_DBT] - rne.tab[idx1,COL_ATT_MDT_DBT])
				# get the closest date
				g <- which.min(gaps)
				# check it is within the tolerance
				if(length(g)>0 && gaps[g]<=tolerance)
				{	idx2 <- idx.sen[g]
					tlog(8,"Found a matching Senate row using approximate date (replacing the first by the second):")
					tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
					tlog(10,paste(sen.tab[idx2,],collapse=","),",",format(sen.tab[idx2,COL_ATT_MDT_DBT]))
					row.conv[idx2] <- idx1
					failed <- FALSE
				}
				
				# otherwise, try to compare the years when the precise date is missing
# TODO the above part was added to handle other tables than the senatorial mandates
# but not completely tested before we decided to finally not use this feature 
#				else if(!senators)
#				{	idx2 <- idx.sen[is.na(sen.tab[idx.sen,COL_ATT_MDT_DBT])]
#					if(length(idx2)>0)
#					{	idx2 <- idx2[sen.tab[idx2,COL_SENAT_MDT_ADBT]==year1]
#						# if there is exactly one: match done
#						if(length(idx2)==1)
#						{	tlog(8,"Found 1 matching Senate row using year, nothing to do:")
#							tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
#							tlog(10,paste(sen.tab[idx2,],collapse=","),",",format(sen.tab[idx2,COL_ATT_MDT_DBT]))
#							row.conv[idx2] <- idx1
#							failed <- FALSE
#						}
#						# if there are several, we have a problem (manual correction)
#						else if(length(idx2)>1)
#						{	tlog(8,"Found several (",length(idx2),") matching Senate rows (year), stopping")
#							tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
#							for(i2 in idx2)
#								tlog(10,paste(sen.tab[i2,],collapse=","),",",format(sen.tab[i2,COL_ATT_MDT_DBT]))
#							stop("Found several matching Senate rows (year)")
#						}
#					}
#				}
		
				# if everything failed, the row is probably wrong and should be corrected manually
				if(failed)
				{	tlog(8,"Did not find any matching Senate row, even with approximate start date or year only")
					tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
					for(i2 in idx.sen)
						tlog(10,paste(sen.tab[i2,],collapse=","),",",format(sen.tab[i2,COL_ATT_MDT_DBT]))
				}
			}
		}
	}
	
	# each value represents a Senate row, and indicates the matching RNE row, or NA if no RNE row matches
	return(row.conv)
}




#############################################################################################
# Adjusts the columns of the specified RNE table, in order to make it comparable with the
# currently processed Senate table.
#
# data: original RNE table.
#
# returns: adjusted RNE table.
#############################################################################################
senate.adjust.rne.table <- function(data)
{	tlog(2,"Adjusting RNE table")
	
	# add missing columns to the RNE table
	tlog(4,"Adding missing columns to the RNE table")
	cols <- data.frame(rep(NA,nrow(data)), as.Date(rep(NA,nrow(data))), stringsAsFactors=FALSE)
	colnames(cols) <- c(COL_ATT_ELU_ID_SENAT, COL_ATT_ELU_DDD)
	rne.tab <- cbind(data, cols)
	
	# possibly add the nationality column
	if(!(COL_ATT_ELU_NAT %in% colnames(data)))
	{	cols <- data.frame(rep(NA,nrow(data)), stringsAsFactors=FALSE)
		colnames(cols) <- COL_ATT_ELU_NAT
		rne.tab <- cbind(rne.tab, cols)
	}
	
	# reorder its columns
	tlog(4,"Sorting RNE table rows")
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(rne.tab))
	rne.tab <- rne.tab[,norm.cols]
	
	# record temp RNE table
	rne.tab.file <- file.path(FOLDER_COMP_SRC_SEN, type, "data_rne1.txt")
	tlog(4,"Recording the new RNE table in ",rne.tab.file)
	write.table(x=rne.tab,
		file=rne.tab.file,		# name of file containing the new table
		quote=FALSE,			# no double quote around strings
		se="\t",				# use tabulations as separators
#		fileEncoding="UTF-8",	# character encoding
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	
	return(rne.tab)
}




#############################################################################################
# Complete/correct the specified RNE table, using the specified Senate table, and based
# on the row matching described by row.conv.
#
# rne.tab: RNE table to update.
# sen.tab: reference Senate table used for updating the RNE table.
# row.conv: Senate2RNE row map.
#
# returns: updated RNE table.
#############################################################################################
senate.update.rne.table <- function(rne.tab, sen.tab, row.conv)
{	tlog(2,"Updating the RNE table")
	result <- rne.tab
	
	# process the Senate rows that are matched in the RNE table
	exist.rows <- which(!is.na(row.conv))
	tlog(4,"Updating the ",length(exist.rows)," matching rows in the RNE table")
	for(idx2 in exist.rows)
	{	idx1 <- row.conv[idx2]
		tlog(6,"Processing row ",idx1)
		
		# overwrite NA values using Senate data
		cols <- which(is.na(result[idx1,]))
		result[idx1, cols] <- sen.tab[idx2, cols]
		
		# update source column
		result[idx1, COL_ATT_SOURCES] <- paste(result[idx1, COL_ATT_SOURCES], sen.tab[idx2, COL_ATT_SOURCES], sep=",")
		
# TODO actually, this is true only for the senate mandates, not the others
# for them, the date may be NA, and only the year may be available (or even nothing at all)
# This part was not finished as we finally decided not use non-senatorial tables (from the Senate DB)
		# force both mandate dates to the Senate one (considered more reliable)
		if(!is.na(sen.tab[idx2, COL_ATT_MDT_DBT]))
			result[idx1, COL_ATT_MDT_DBT] <- sen.tab[idx2, COL_ATT_MDT_DBT]
		if(!is.na(sen.tab[idx2, COL_ATT_MDT_FIN]))
			result[idx1, COL_ATT_MDT_FIN] <- sen.tab[idx2, COL_ATT_MDT_FIN]
	}
	
	# insert new Senate rows into existing RNE table
	missing.rows <- which(is.na(row.conv))
	#missing.rows <- which(is.na(row.conv) & get.year(sen.tab[,COL_ATT_MDT_DBT])>=2001)
	tlog(4,"Inserting ",length(missing.rows)," missing rows in the RNE table")
	result <- rbind(result, sen.tab[missing.rows,])
	# sort the resulting table
	result <- result[order(result[,COL_ATT_DPT_CODE], 
					result[,COL_ATT_ELU_NOM], result[,COL_ATT_ELU_PRENOM], 
					result[,COL_ATT_MDT_DBT], result[,COL_ATT_MDT_FIN]) ,]
	
	# record the corrected/completed table
	rne.tab.file <- file.path(FOLDER_COMP_SRC_SEN, type, "data_rne2.txt")
	tlog(4,"Recording the corrected/completed table: ",rne.tab.file)
	write.table(x=result,
		file=rne.tab.file,		# name of file containing the new table
		quote=FALSE,			# no double quote around strings
		se="\t",				# use tabulations as separators
#		fileEncoding="UTF-8",	# character encoding
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	
	return(result)
}




#############################################################################################
# Complete the RNE data with the mandates found in the Senate database.
# 
# NOTE: in the end, only the Senatorial data was leveraged, as the other tables seem
# declarative, are sometimes imprecise and appear to be unreliable.
#
# data: one of the RNE tables.
# type: nature of the table (CD, CM, CR, D, DE, M, S).
# cache: whether or not to cache the map allowing to match people in the RNE and Senate tables.
#        If FALSE, this operation is performed each time, otherwise it is computed only once.
# compare: whether or not to perform a comparison of the resulting RNE and Senate tables
#          (used for debugging only).
#
# returns: the same table, completed using the Senate DB.
#############################################################################################
senate.integrate.data <- function(data, type, cache=TRUE, compare=FALSE)
{	# load the general senate table, containing individual information
	general.table <- senate.load.general.table(cache)
	
	# load the mandate table
	elect.table <- senate.load.elect.table(type)
	
	# build a mandate table comparable to a RNE table
	sen.tab <- senate.convert.mandate.table(general.table, elect.table, type)
	
	# adjust the RNE table
	rne.tab <- senate.adjust.rne.table(data)
	
	# match rows using a tolerance of a few days for dates
	row.conv <- senate.match.senate.vs.rne.rows(sen.tab, rne.tab, tolerance=14)
	
	# use Senate data to correct/complete existing RNE rows
	result <- senate.update.rne.table(rne.tab, sen.tab, row.conv)
	
	# compare both tables
	if(compare)
	{	out.folder <- file.path(FOLDER_COMP_SRC_SEN, type)
		sen.tab.file <- file.path(out.folder, "data_senat.txt")
		rne.tab.file <- file.path(out.folder, "data_rne2.txt")
		source("src/comparison/compare_tables.R")
		compare.tables(files0=rne.tab.file, files1=sen.tab.file, out.folder)
	}
	
	return(result)
}
