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
# everything that needs to be. This function uses a cache system in order to speed the process.
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
		
		# retrieve department codes
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
		
		# retrieve political group
		# NOTE: stored in the general table, so associated to each individual by opposition to each *mandate*
		indiv.pol.nuances <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_NUANCE])))
		nuances.conv <- senate.load.conversion.file(FILE_SENAT_CONV_NUANCES)
		idx <- match(indiv.pol.nuances, nuances.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.pol.nuances[idx2] <- nuances.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve occupations names
		indiv.occ.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[idx,COL_SENAT_PRO_NOM])))
		occup.conv <- senate.load.conversion.file(FILE_SENAT_CONV_PRO)
		idx <- match(indiv.occ.names, occup.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.occ.names[idx2] <- occup.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve occupations codes
		# NOTE we could retrieve codes from the merged main table, but is it really necessary?
		indiv.occ.codes <- rep(NA,nrow(indiv.table))
		
		# retrieve functions
		indiv.fonct.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[idx,COL_SENAT_FCT_BUR])))
		# NOTE "function" also seems to be available under a dynamic form in the "Groupes" table
#		indiv.fonct.names <- rep(NA,nrow(indiv.table))
		
		##################### cross referencing with RNE to get ids
		# load RNE senate table
		tlog(4,"Loading RNE Senate table")
		data <- load.s.data(correct.data=TRUE)
		
		# match senate ids with RNE ids
		tlog(4,"Matching Senate and RNE people ids")
		indiv.ids.rne <- rep(NA, nrow(indiv.table))
		indiv.ids.univ <- paste("SEN",indiv.table[,COL_SENAT_ELU_MATRI],sep="_")
		for(i in 1:nrow(indiv.table))
		{	tlog(6, "Processing senator ",i,"/",nrow(indiv.table))
			tmp <- which(data[,COL_ATT_ELU_NOM]==indiv.last.names[i]
							& data[,COL_ATT_ELU_PRENOM]==indiv.first.names[i]
							& data[,COL_ATT_ELU_DDN]==indiv.birth.dates[i])
#			& data[,COL_ATT_DPT_NOM]==indiv.dpt.names[i])
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
							paste(data[tmp[1],c(COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_DDN)],collapse=","),
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
#					& indiv.birth.dates==data[rne.rows[i],COL_ATT_ELU_DDN])
#				#			& indiv.dpt.names==data[rne.rows[i],COL_ATT_DPT_NOM])
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
			COL_ATT_DPT_CODE,
			COL_ATT_DPT_NOM,
			COL_ATT_ELU_ID,
			COL_ATT_ELU_ID_RNE,
			COL_ATT_ELU_ID_SENAT,
			COL_ATT_ELU_NOM,
			COL_ATT_ELU_PRENOM,
			COL_ATT_ELU_DDN,
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
{	# match the general and mandate tables
	idx <- match(elect.table[,COL_SENAT_ELU_MATRI], general.table[,COL_ATT_ELU_ID_SENAT])
	
	# convert mandate dates
	mdt.start.dates <- as.Date(elect.table[,COL_SENAT_MDT_DBT], "%d/%m/%Y")
	mdt.end.dates <- as.Date(elect.table[,COL_SENAT_MDT_FIN], "%d/%m/%Y")
	
	# mandate name
	mdt.name <- c()
	mdt.name["CD"] <- "CONSEILLER DEPARTEMENTAL"
	mdt.name["CM"] <- "CONSEILLER MUNICIPAL"
	mdt.name["CR"] <- "CONSEILLER REGIONAL"
	mdt.name["D"] <- "DEPUTE"
	mdt.name["DE"] <- "REPRESENTANT AU PARLEMENT EUROPEEN"
	mdt.name["S"] <- "SENATEUR"
	mdt.names <- rep(mdt.name[type], length(idx))
	
	# possibly get motive of end of mandate
	mdt.motives <- rep(NA,length(idx))
	if(COL_SENAT_MDT_MOTIF %in% colnames(elect.table))
		mdt.motives <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_MDT_MOTIF])))
	
	# possibly get and convert function dates
	fct.start.dates <- rep(NA, length(idx))
	if(COL_SENAT_FCT_DBT %in% colnames(elect.table))
		fct.start.dates <- as.Date(elect.table[,COL_SENAT_FCT_DBT], "%d/%m/%Y")
	fct.end.dates <- rep(NA, length(idx))
	if(COL_SENAT_FCT_FIN %in% colnames(elect.table))
		fct.end.dates <- as.Date(elect.table[,COL_SENAT_FCT_FIN], "%d/%m/%Y")
	
	# possibly get function name
	function.names <- general.table[idx,COL_ATT_FCT_NOM]
	if(COL_SENAT_FCT_NOM %in% colnames(elect.table))
		function.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_FCT_NOM])))
	
	# build senate table
	sen.tab <- data.frame(
		general.table[idx,COL_ATT_ELU_ID],			# universal id
		general.table[idx,COL_ATT_ELU_ID_RNE],		# RNE id
		general.table[idx,COL_ATT_ELU_ID_SENAT],	# senate id
		general.table[idx,COL_ATT_ELU_NOM],			# last name
		general.table[idx,COL_ATT_ELU_PRENOM],		# first name
		general.table[idx,COL_ATT_ELU_DDN],			# birth date
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
		COL_ATT_ELU_DDN,
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
	if(type=="CR")
	{	# regions
		regions <- data.frame(trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_REG_NOM]))), stringsAsFactors=FALSE)
		sen.tab <- cbind(sen.tab, regions)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_REG_NOM
		# NOTE we could get the region code from the RNE table
		# TODO check whether it is necessery to adjust certain regions names from the CR Senate table
	}
	else if(type=="CD")
	{	# departments
		departments <- data.frame(trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_DPT_NOM2]))), stringsAsFactors=FALSE)
		sen.tab <- cbind(sen.tab, departments)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_DPT_NOM
		# NOTE we could get the department code from the RNE table
		# TODO check whether it is necessery to adjust certain department names from the CD Senate table
		
		# cantons
		cantons <- data.frame(trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_CANT_NOM]))), stringsAsFactors=FALSE)
		sen.tab <- cbind(sen.tab, cantons)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_CANT_NOM
		# NOTE we could get the canton code from the RNE table
		# TODO check whether it is necessery to adjust certain canton names from the CD Senate table
	}
	else if(type=="CM")
	{	# municipalities
		municipality <- data.frame(trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_COM_NOM]))), stringsAsFactors=FALSE)
		sen.tab <- cbind(sen.tab, municipality)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_COM_NOM
		# NOTE we could get the department and other missing columns from the RNE table
		#      but we might also need to check the postgresql version of the senate DB
		# TODO check whether it is necessery to adjust certain municipality names from the CM Senate table
	}
	else if(type=="S")
	{	# department codes
		dpt.codes <- general.table[idx, COL_ATT_DPT_CODE]
		sen.tab <- cbind(sen.tab, dpt.codes)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_DPT_CODE
		
		# department names
		dpt.names <- general.table[idx, COL_ATT_DPT_NOM]
		sen.tab <- cbind(sen.tab, dpt.names)
		colnames(sen.tab)[ncol(sen.tab)] <- COL_ATT_DPT_NOM
	}
	
#	# keep only mandates starting before 2001
#	pre.mandates <- which(mdt.start.dates<as.Date("2001/01/01")) 
#	sen.tab <- sen.tab[-pre.mandates,]
	
	# order the Senate table
	sen.tab <- sen.tab[order(sen.tab[,COL_ATT_DPT_CODE], 
					sen.tab[,COL_ATT_ELU_NOM], sen.tab[,COL_ATT_ELU_PRENOM], 
					sen.tab[,COL_ATT_MDT_DBT], sen.tab[,COL_ATT_MDT_FIN]) ,]
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(sen.tab))
	sen.tab <- sen.tab[,norm.cols]
	
	# record Senate table
	folder <- file.path(FOLDER_COMP_SRC_SEN, type)
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	sen.tab.file <- file.path(folder, "data_senat.txt")
	write.table(x=sen.tab,
		file=sen.tab.file,		# name of file containing the new table
		quote=FALSE,			# no double quote around strings
		se="\t",				# use tabulations as separators
#		fileEncoding="UTF-8",	# character encoding
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	
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
{	# get all the RNE ids
	ids <- sort(unique(rne.tab[,COL_ATT_ELU_ID]))
	
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
			tlog(6,"Processing RNE row ",idx1," (",j,"/",length(idx.rne),")")
			
			# get the Senate row(s) matching the mandate start date
			idx2 <- idx.sen[sen.tab[idx.sen,COL_ATT_MDT_DBT]==rne.tab[idx1,COL_ATT_MDT_DBT]]
			
			# if there is exactly one: match done
			if(length(idx2)==1)
			{	tlog(8,"Found 1 matching Senate row, nothing to do")
				row.conv[idx2] <- idx1
			}
			
			# if there are several, we have a problem (manual correction)
			else if(length(idx2)>1)
			{	tlog(8,"Found several matching Senate rows")
				stop("Found several matching Senate rows")
			}
			
			# if there are none, compare dates in an approximate way
			else if(length(idx2)<1)
			{	# difference in days between the dates
				gaps <- abs(sen.tab[idx.sen,COL_ATT_MDT_DBT] - rne.tab[idx1,COL_ATT_MDT_DBT])
				# get the closest date
				g <- which.min(gaps)
				# check it is within the tolerance
				if(gaps[g] <= tolerance)
				{	idx2 <- idx.sen[g]
					tlog(8,"Found a matching Senate row using approximate date replacing the first by the second:")
#					tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
#					tlog(10,paste(sen.tab[idx2,],collapse=","),",",format(sen.tab[idx2,COL_ATT_MDT_DBT]))
					row.conv[idx2] <- idx1
				}
				# otherwise, the row is probably wrong and should be corrected manually
				else
				{	tlog(8,"Did not find any matching Senate row, even with approximate start date")
					tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]))
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
{	# add missing columns to the RNE table
	rne.tab <- cbind(data, rep(NA,nrow(data)), as.Date(rep(NA,nrow(data))), rep(NA,nrow(data)))
	colnames(rne.tab)[(ncol(rne.tab)-2):ncol(rne.tab)] <- c(COL_ATT_ELU_ID_SENAT, COL_ATT_ELU_DDD, COL_ATT_ELU_NAT)
	rne.tab <- rne.tab[,colnames(rne.tab)]
	
	# reorder its columns
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(rne.tab))
	rne.tab <- rne.tab[,norm.cols]
	
	# record temp RNE table
	rne.tab.file <- file.path(FOLDER_COMP_SRC_SEN, type, "data_rne1.txt")
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
{	result <- rne.tab
	
	# process the Senate rows that are matched in the RNE table
	exist.rows <- which(!is.na(row.conv))
	tlog(2,"Updating the ",length(exist.rows)," matching rows in the RNE table")
	for(idx2 in exist.rows)
	{	idx1 <- row.conv[idx2]
		tlog(4,"Processing row ",idx1)
		
		# overwrite NA values using Senate data
		cols <- which(is.na(result[idx1,]))
		result[idx1, cols] <- sen.tab[idx2, cols]
		
		# update source column
		result[idx1, COL_ATT_SOURCES] <- paste(result[idx1, COL_ATT_SOURCES], sen.tab[idx2, COL_ATT_SOURCES], sep=",")
		
		# force both mandate dates to the Senate one (considered more reliable)
		result[idx1, c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)] <- sen.tab[idx2, c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)]
	}
	
	# insert new Senate rows into existing RNE table
	missing.rows <- which(is.na(row.conv))
	tlog(2,"Inserting ",length(missing.rows)," missing rows in the RNE table")
	result <- rbind(result, sen.tab[missing.rows,])
	# sort the resulting table
	result <- result[order(result[,COL_ATT_DPT_CODE], 
					result[,COL_ATT_ELU_NOM], result[,COL_ATT_ELU_PRENOM], 
					result[,COL_ATT_MDT_DBT], result[,COL_ATT_MDT_FIN]) ,]
	
	# record the corrected/completed table
	rne.tab.file <- file.path(FOLDER_COMP_SRC_SEN, type, "data_rne2.txt")
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
# data: one of the RNE table.
# type: nature of the table (CD, CM, CR, D, DE, M, S).
# cache: whether or not to cache the map allowing to match people in the RNE and Senate tables.
#        If FALSE, this operation is performed each time, otherwise it is computed only once.
# compare: whether or not to perform a comparison of the resulting RNE and Senate tables
#          (used for debugging only).
#
# returns: the same table, completed using the Senate DB.
#############################################################################################
senate.integrate.data <- function(data, type, cache=TRUE, compare=FALSE) 	# debug type="S";cache=FALSE;compare=TRUE
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
