#############################################################################################
# Functions used to inject data from the National Assembly database into the RNE.
# 
# 02/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Reads the set of XML files constituting the Assembly database, and extract the information
# we need. It is recorded as a separate table used in the rest of the processing.
#############################################################################################
assembly.convert.xml <- function()
{	# retrieve the list of all XML files
	tlog(2, "Listing all XML files:")
	xml.files <- list.files(path=FOLDER_ASSEMB_RAW, pattern=".\\.xml", all.files=TRUE,
			full.names=TRUE, recursive=FALSE,
			ignore.case=TRUE, include.dirs=FALSE, no..=TRUE)
	tlog(4, "Found ",length(xml.files)," files")
	
	# init personal table
	perso.tab <- data.frame(
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		as.character(rep(NA,length(xml.files))),
		#
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
	colnames(perso.tab) <- c(
		COL_ATT_ELU_ID_ASSEMB,
		COL_ATT_ELU_SEXE,
		COL_ATT_ELU_PRENOM,
		COL_ATT_ELU_NOM,
		COL_ATT_ELU_NAIS_DATE,
		COL_ATT_ELU_DDD,
		COL_ATT_ELU_NAIS_COM,
		COL_ATT_ELU_NAIS_DPT,
		COL_ATT_ELU_NAIS_PAYS,
		COL_ASSEMB_PRO_NOM,
		COL_ASSEMB_PRO_CAT,
		COL_ASSEMB_PRO_FAM
	)	
	
	# init mandate table
	mandate.tab <- data.frame(
		"",
		"",
		"",
		"",
		"",
		"",
		"",
		"",
		"",
		#
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
	colnames(mandate.tab) <- c(
		COL_ATT_ELU_ID_ASSEMB,
		COL_ATT_REG_NOM,
		COL_ATT_DPT_NOM,
		COL_ATT_DPT_CODE,
		COL_ATT_CIRC_CODE,
		COL_ATT_MDT_DBT,
		COL_ATT_MDT_FIN,
		COL_ATT_MDT_MOTIF,
		COL_ATT_FCT_NOM
	)
	mandate.tab <- mandate.tab[-(1:nrow(mandate.tab)),]
	
	# process each XML file one after the other
	tlog(2, "Processing each file separately")
	for(i in 1:length(xml.files))
	{	tlog(4, "Processing file ",i,"/",length(xml.files)," (",xml.files[i],")")
		
		doc <- xmlParse(file=xml.files[i])
#		root <- xmlRoot(doc)
		
		# retrieve personal info
		perso.tab[i,COL_ATT_ELU_ID_ASSEMB] <- xmlValue(getNodeSet(doc,"/d:acteur/d:uid", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		tlog(6, "ID: ",perso.tab[i,COL_ATT_ELU_ID_ASSEMB])
		perso.tab[i,COL_ATT_ELU_SEXE] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:civ", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		perso.tab[i,COL_ATT_ELU_PRENOM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:prenom", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		perso.tab[i,COL_ATT_ELU_NOM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:nom", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		tlog(6, "Name: ",perso.tab[i,COL_ATT_ELU_PRENOM]," ",perso.tab[i,COL_ATT_ELU_NOM])
#		tlog(6, "trigramme: \"",xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:trigramme", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),"\"")
		
		# retrieve bith-related info
		perso.tab[i,COL_ATT_ELU_NAIS_DATE] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:dateNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		tlog(6, "Birth date: ",perso.tab[i,COL_ATT_ELU_NAIS_DATE])
		perso.tab[i,COL_ATT_ELU_NAIS_COM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:villeNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		perso.tab[i,COL_ATT_ELU_NAIS_DPT] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:depNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		perso.tab[i,COL_ATT_ELU_NAIS_PAYS] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:paysNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		
		# retrieve death-related info
		perso.tab[i,COL_ATT_ELU_DDD] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:dateDeces", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		tlog(6,"Death date: ",xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:dateDeces", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]))
		
		# retrieve occupation-related info
		perso.tab[i,COL_ASSEMB_PRO_NOM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:profession/d:libelleCourant", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		tlog(6, "Occupation: ",perso.tab[i,COL_ASSEMB_PRO_NOM])
		perso.tab[i,COL_ASSEMB_PRO_CAT] <- xmlValue(getNodeSet(doc,"/d:acteur/d:profession/d:socProcINSEE/d:catSocPro", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		perso.tab[i,COL_ASSEMB_PRO_FAM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:profession/d:socProcINSEE/d:famSocPro", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		
		# retrieve list of mandates
		mandate.nodes <- getNodeSet(doc,"/d:acteur/d:mandats/d:mandat[@xsi:type='MandatParlementaire_type']", c(d="http://schemas.assemblee-nationale.fr/referentiel", xsi="http://www.w3.org/2001/XMLSchema-instance"))
#		mandate.nodes <- getNodeSet(doc,"/d:acteur/d:mandats/d:mandat[@xsi:type='MandatParlementaire_type' and ./d:typeOrgane/text()='ASSEMBLEE']", c(d="http://schemas.assemblee-nationale.fr/referentiel", xsi="http://www.w3.org/2001/XMLSchema-instance"))
		tlog(6, "Computing ",length(mandate.nodes)," mandates")
		for(j in 1:length(mandate.nodes))
		{	tlog(8, "Processing mandate ",j,"/",length(mandate.nodes))
			
			org.type <- xmlValue(getNodeSet(mandate.nodes[[j]],"d:typeOrgane", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
			if(org.type=="ASSEMBLEE")
			{	row <- data.frame(
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:acteurRef", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:region", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:departement", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:numDepartement", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:numCirco", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:dateDebut", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:dateFin", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:mandature/d:causeFin", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					xmlValue(getNodeSet(mandate.nodes[[j]],"d:infosQualite/d:codeQualite", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
					#
					check.names=FALSE,
					stringsAsFactors=FALSE
				)
				colnames(row) <- c(
					COL_ATT_ELU_ID_ASSEMB,
					COL_ATT_REG_NOM,
					COL_ATT_DPT_NOM,
					COL_ATT_DPT_CODE,
					COL_ATT_CIRC_CODE,
					COL_ATT_MDT_DBT,
					COL_ATT_MDT_FIN,
					COL_ATT_MDT_MOTIF,
					COL_ATT_FCT_NOM
				)
				mandate.tab <- rbind(mandate.tab, row)
			}
		}
	}
	
	# get the list of equivalent ids
	tlog(0,"Loading the table of equivalent ids (",FILE_ASSEMB_CONV_IDS,")")
	equiv.tab<- read.table(
		file=FILE_ASSEMB_CONV_IDS,	# name of the equivalence file
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
	
	# fix equivalent ids
	tlog(0,"Convert table of equivalent ids to map")
	unique.ids <- unique(perso.tab[,COL_ATT_ELU_ID_ASSEMB])
	conv.map <- c()
	for(r in 1:nrow(equiv.tab))
	{	main.id <- equiv.tab[r,1]
		other.ids <- strsplit(x=equiv.tab[r,2], split=",", fixed=TRUE)[[1]]
		other.ids <- intersect(other.ids,unique.ids)
		if(length(other.ids)>0)
			conv.map[other.ids] <- rep(main.id, length(other.ids))
	}
	plan(multiprocess, workers=CORE.NBR/2)
	# substitute correct ids
	tlog(0,"Fixing duplicate ids")
	perso.tab[,COL_ATT_ELU_ID_ASSEMB] <- future_sapply(perso.tab[,COL_ATT_ELU_ID_ASSEMB], function(id)
		{	new.id <- conv.map[id]
			if(is.na(new.id))
				return(id)
			else
				return(new.id)
		})
	mandate.tab[,COL_ATT_ELU_ID_ASSEMB] <- future_sapply(mandate.tab[,COL_ATT_ELU_ID_ASSEMB], function(id)
		{	new.id <- conv.map[id]
			if(is.na(new.id))
				return(id)
			else
				return(new.id)
		})

	
	# check if the table contains homonyms (lastname, firstname, birthdate)
	tlog(2, "Looking for homonyms")
	unique.ids <- sort(unique(perso.tab[,COL_ATT_ELU_ID_ASSEMB]))
	for(i in 1:length(unique.ids))
	{	tlog(4, "Processing id ",unique.ids[i]," (",i,"/",length(unique.ids),")")
		idx <- which(perso.tab[,COL_ATT_ELU_ID_ASSEMB]==unique.ids[i])[1]
		candidates <- which(perso.tab[,COL_ATT_ELU_NOM]==perso.tab[idx,COL_ATT_ELU_NOM]
				& perso.tab[,COL_ATT_ELU_PRENOM]==perso.tab[idx,COL_ATT_ELU_PRENOM]
				& perso.tab[,COL_ATT_ELU_NAIS_DATE]==perso.tab[idx,COL_ATT_ELU_NAIS_DATE])
		ids <- sort(unique(perso.tab[candidates,COL_ATT_ELU_ID_ASSEMB]))
		if(length(ids)==1)
			tlog(6, "No problem with this id")
		else if(length(ids)>1)
		{	tlog(6, "Found several person matching this id info (last/first names, birthdate): ",paste(ids,collapse=","))
			stop("Found several person matching this id info")
		}
		else
		{	tlog(6, "Did not find any entry for this id")
			stop("Did not find any entry for this id")
		}
	}
	
	# record personal table
	tlog(2, "Recording personal table in ",FILE_ASSEMB_GENERAL)
	write.table(x=perso.tab,
		file=FILE_ASSEMB_GENERAL,	# name of file containing the new table
		quote=FALSE,				# no double quote around strings
		se="\t",					# use tabulations as separators
#		fileEncoding="UTF-8",		# character encoding
		row.names=FALSE,			# no names for rows
		col.names=TRUE				# record table headers
	)
	
	# record mandate table
	tlog(2, "Recording mandate table in ",FILE_ASSEMB_MANDATS)
	write.table(x=mandate.tab,
		file=FILE_ASSEMB_MANDATS,	# name of file containing the new table
		quote=FALSE,				# no double quote around strings
		se="\t",					# use tabulations as separators
#		fileEncoding="UTF-8",		# character encoding
		row.names=FALSE,			# no names for rows
		col.names=TRUE				# record table headers
	)
	
	# NOTE: for some reason, both tables need manual conversion to UTF-8
}




#############################################################################################
# Loads the Assembly table containing the description of individuals, and converts/normalizes
# everything that needs to be. This function uses a cache system in order to speed up the process.
#
# cache: whether or not to use the cache system.
# 
# returns: the loaded table.
#############################################################################################
assembly.load.general.table <- function(cache)
{	tlog(2,"Loading the Assembly general table ")
	
	# possibly use the cached file
	if(cache & file.exists(FILE_ASSEMB_GENERAL_CACHE))
	{	tlog(4,"Cache enabled and found cached file >> using cache (",FILE_ASSEMB_GENERAL_CACHE,")")
		result <- read.table(
			file=FILE_ASSEMB_GENERAL_CACHE,	# name of the data file
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
		tlog(4,"Read raw file ",FILE_ASSEMB_GENERAL)
		indiv.table <- read.table(
			file=FILE_ASSEMB_GENERAL,	# name of the data file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE,					# don't convert strings to factors
			colClasses="character"		# all column originally read as characters, then converted later if needed
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		)
		for(c in 1:ncol(indiv.table))
			indiv.table[which(indiv.table[,c]==""),c] <- NA
		
		tlog(4,"Normalizing content")
		# normalize last names
		indiv.last.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ATT_ELU_NOM])))
#indiv.last.names <- trimws(normalize.proper.nouns(remove.diacritics(iconv(x=indiv.table[,COL_ATT_ELU_NOM], from="Latin1", to="UTF8"))))
		lastname.conv <- senate.load.conversion.file(FILE_ASSEMB_CONV_NOMSFAM)
		idx <- match(indiv.last.names, lastname.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.last.names[idx2] <- lastname.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# normalize first names
		indiv.first.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ATT_ELU_PRENOM])))
		firstname.conv <- senate.load.conversion.file(FILE_ASSEMB_CONV_PRENOMS)
		idx <- as.integer(firstname.conv[, COL_CORREC_ROW])
		indiv.first.names[idx] <- firstname.conv[, COL_CORREC_VALAPR]
		
		# init sex
		indiv.sex <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ATT_ELU_SEXE])))
		indiv.sex[indiv.sex=="M"] <- "M"
		indiv.sex[indiv.sex=="MME" | indiv.sex=="MLLE"] <- "F"
		
		# init nationality
		indiv.nationality <- rep("FRANCAISE", nrow(indiv.table))
		# NOTE all members of the parliament must be French, by law
		
		# convert birth/death dates
		indiv.birth.dates <- as.Date(indiv.table[,COL_ATT_ELU_NAIS_DATE], "%Y-%m-%d")
		indiv.death.dates <- as.Date(indiv.table[,COL_ATT_ELU_DDD], "%Y-%m-%d")
		
		# normalize municipality names
		indiv.com.names <- normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ATT_ELU_NAIS_COM]))

		# normalize department names
		indiv.dpt.names <- normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ATT_ELU_NAIS_DPT]))
		dpts.conv <- senate.load.conversion.file(FILE_ASSEMB_CONV_DPTS)
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
		#sort(unique(indiv.dpt.names[which(is.na(indiv.dpt.idx))]))	# for debug
		
		# normalize country names
		indiv.country.names <- normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ATT_ELU_NAIS_PAYS]))
		country.conv <- senate.load.conversion.file(FILE_ASSEMB_CONV_PAYS)
		idx <- match(indiv.country.names, country.conv[, COL_CORREC_VALAVT])
		idx2 <- which(!is.na(idx))
		indiv.country.names[idx2] <- country.conv[idx[idx2], COL_CORREC_VALAPR]
		# complete missing country name
		idx <- which(is.na(indiv.country.names) & !is.na(indiv.dpt.codes))
		indiv.country.names[idx] <- "FRANCE"
		
		# retrieve occupations names
		indiv.occ.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_ASSEMB_PRO_CAT])))
#		occup.conv <- senate.load.conversion.file(FILE_ASSEMB_CONV_PRO)
#		idx <- match(indiv.occ.names, occup.conv[, COL_CORREC_VALAVT])
#		idx2 <- which(!is.na(idx))
#		indiv.occ.names[idx2] <- occup.conv[idx[idx2], COL_CORREC_VALAPR]
		
		# retrieve occupations codes
		# NOTE we could retrieve codes from the merged main table, but is it really necessary?
		indiv.occ.codes <- rep(NA,nrow(indiv.table))
		
		##################### cross referencing with RNE to get ids
		# load RNE assembly table
		tlog(4,"Loading RNE Assembly table")
		data <- load.d.data(correct.data=TRUE, complete.data=FALSE)
		
		# match assembly ids with RNE ids
		tlog(4,"Matching Assembly and RNE people ids")
		indiv.ids.rne <- rep(NA, nrow(indiv.table))				# meant to contain the RNE ids of the MPs
		indiv.ids.univ <- sapply(indiv.table[,COL_ATT_ELU_ID_ASSEMB], function(id) 
					paste0("ASN_",paste(rep("0",9-nchar(id)),collapse=""), substr(id, start=3, stop=nchar(id))))
		for(i in 1:nrow(indiv.table))
		{	tlog(6, "Processing MP ",indiv.ids.univ[i]," (",i,"/",nrow(indiv.table),")")
			tmp <- which(data[,COL_ATT_ELU_NOM]==indiv.last.names[i]
							& data[,COL_ATT_ELU_PRENOM]==indiv.first.names[i]
							& data[,COL_ATT_ELU_NAIS_DATE]==indiv.birth.dates[i])
#							& data[,COL_ATT_DPT_NOM]==indiv.dpt.names[i])
			if(length(tmp)==0)
				tlog(6, "No match for MP: ",paste(indiv.table[i,],collapse=","))
			else 
			{	tmp.ids <- sort(unique(data[tmp,COL_ATT_ELU_ID_RNE]))
				if(length(tmp.ids)>1)
				{	tlog(8, "Found several matches (ids ",paste(tmp.ids,collapse=","),") for MP: ",paste(indiv.table[i,],collapse=","))
					# display more details?
					stop("Found several matches")
				}
				else
				{	tlog(8, "Found a single RNE entry (id ",tmp.ids,", name ",
							paste(data[tmp[1],c(COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM)],collapse=","),",",
							format(data[tmp[1],COL_ATT_ELU_NAIS_DATE]),
							") for MP: ",paste(indiv.table[i,],collapse=","))
					indiv.ids.rne[i] <- tmp.ids
					indiv.ids.univ[i] <- data[tmp[1],COL_ATT_ELU_ID]
				}	
			}
		}
		
#		# match RNE ids with assembly ids (just for checking)
#		rne.ids <- setdiff(sort(unique(data[,COL_ATT_ELU_ID])),"RNE_1513599")
#		rne.rows <- match(rne.ids,data[,COL_ATT_ELU_ID])
#		asn.ids <- rep(NA,length(rne.ids))				# meant to contain the parliamentary ids of the RNE MPs
#		for(i in 1:length(rne.ids))
#		{	tlog(2, "Processing row ",i,"/",length(rne.ids))
#			tmp <- which(indiv.last.names==data[rne.rows[i],COL_ATT_ELU_NOM]
#					& indiv.first.names==data[rne.rows[i],COL_ATT_ELU_PRENOM]
#					& indiv.birth.dates==data[rne.rows[i],COL_ATT_ELU_NAIS_DATE])
##					& indiv.dpt.names==data[rne.rows[i],COL_ATT_DPT_NOM])
#			if(length(tmp)==0)
#				tlog(4, "No match for row: ",paste(data[rne.rows[i],],collapse=","))
#			else 
#			{	tmp.ids <- sort(unique(indiv.table[tmp,COL_ATT_ELU_ID_ASSEMB]))
#				if(length(tmp.ids)>1)
#				{	tlog(4, "Found several matches (ids ",paste(tmp.ids,collapse=","),") for row: ",paste(data[rne.rows[i],],collapse=","))
#					stop("Found several matches (ids ",paste(tmp.ids,collapse=","),") for row: ",paste(data[rne.rows[i],],collapse=","))
#				}
#				else
#				{	tlog(4, "Found a single Assembly entry (id ",tmp.ids,", name ",
#							paste(indiv.last.names[tmp[1]],indiv.first.names[tmp[1]],indiv.birth.dates[tmp[1]],sep=","),
#							") for row: ",paste(indiv.table[i,],collapse=","))
#					asn.ids[i] <- tmp.ids
#				}	
#			}
#		}
##		cbind(which(is.na(asn.ids)), rne.ids[which(is.na(asn.ids))])	# for debug
		
		##################### build clean table
		tlog(4,"Building clean individual table")
		result <- data.frame(
				indiv.ids.univ,							# universal id
				indiv.ids.rne,							# RNE id
				indiv.table[,COL_ATT_ELU_ID_ASSEMB],	# assembly id
				indiv.last.names,						# last name
				indiv.first.names,						# first name
				indiv.birth.dates,						# birth date
				indiv.death.dates,						# death date
				indiv.sex,								# sex
				indiv.nationality,						# country
				indiv.com.names,						# municipality name
#				indiv.dpt.ids,							# department id
#				indiv.dpt.codes,						# department code
				indiv.dpt.names,						# department name
				indiv.country.names,					# country name
				indiv.occ.codes,						# occupation code
				indiv.occ.names,						# occupation name
				#
				check.names=FALSE,
				stringsAsFactors=FALSE
		)
		colnames(result) <- c(
				COL_ATT_ELU_ID,
				COL_ATT_ELU_ID_RNE,
				COL_ATT_ELU_ID_ASSEMB,
				COL_ATT_ELU_NOM,
				COL_ATT_ELU_PRENOM,
				COL_ATT_ELU_NAIS_DATE,
				COL_ATT_ELU_DDD,
				COL_ATT_ELU_SEXE,
				COL_ATT_ELU_NAT,
				COL_ATT_ELU_NAIS_COM,
				COL_ATT_ELU_NAIS_DPT,
				COL_ATT_ELU_NAIS_PAYS,
				COL_ATT_PRO_CODE,
				COL_ATT_PRO_NOM
		)
		
		# record the table for later use
		#if(cache)
		{	tlog(4,"Caching the resulting table in file ",FILE_ASSEMB_GENERAL_CACHE)
			write.table(x=result,
					file=FILE_ASSEMB_GENERAL_CACHE,	# name of file containing the new table
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
# Loads the Assembly table containing the description of mandates.
# 
# returns: the loaded table.
#############################################################################################
assembly.load.elect.table <- function()
{	tlog(2,"Loading the Assembly madate table: ",FILE_ASSEMB_MANDATS)
	
	# load the corresponding assembly mandate table(s)
	elect.table <- read.table(
		file=FILE_ASSEMB_MANDATS,	# name of the data file
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
	tlog(4,"Cleaning empty cells")
	for(c in 1:ncol(elect.table))
		elect.table[which(elect.table[,c]==""),c] <- NA
	
	# make a few corrections
	tlog(4,"Loading correction table")
	correc.table <- read.table(
		file=FILE_ASSEMB_CORRECS,	# name of the data file
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
	correc.table[correc.table=="NA"] <- NA
	idx.rm <- c()
	tlog(4,"Applying corrections to the data")
	for(i in 1:nrow(correc.table))
	{	tlog(6, "Correction ",i,"/",nrow(correc.table))
		idx <- which(elect.table[,COL_ATT_ELU_ID_ASSEMB]==correc.table[i,COL_ATT_ELU_ID_ASSEMB]
						& elect.table[,COL_ATT_MDT_DBT]==correc.table[i,COL_ATT_MDT_DBT]
						& (elect.table[,COL_ATT_MDT_FIN]==correc.table[i,COL_ATT_MDT_FIN] 
							| is.na(elect.table[,COL_ATT_MDT_FIN]) & is.na(correc.table[i,COL_ATT_MDT_FIN])))
		if(length(idx)!=1)
		{	tlog(8, "ERROR: found ",length(idx)," matching rows while correcting the table")
			stop("ERROR: found ",length(idx)," matching rows while correcting the table")
		}
		else
		{	if(elect.table[idx,correc.table[i,COL_CORREC_ATTR]]==correc.table[i,COL_CORREC_VALAVT])
			{	if(correc.table[i,COL_CORREC_ATTR]==COL_ATT_ELU_ID_ASSEMB && is.na(correc.table[i,COL_CORREC_VALAPR]))
				{	tlog(8, "Row ",idx," marked for removal")
					idx.rm <- c(idx.rm, idx)
				}
				else
				{	tlog(8, "Replacing \"",correc.table[i,COL_CORREC_VALAVT],"\" by \"",correc.table[i,COL_CORREC_VALAPR],"\"")
					elect.table[idx,correc.table[i,COL_CORREC_ATTR]] <- correc.table[i,COL_CORREC_VALAPR]
				}
			}
			else
			{	tlog(8, "ERROR: the found value (",elect.table[idx,correc.table[i,COL_CORREC_ATTR]],") does not match the correction file (",correc.table[i,COL_CORREC_VALAVT],")")
				stop("ERROR: the found value (",elect.table[idx,correc.table[i,COL_CORREC_ATTR]],") does not match the correction file (",correc.table[i,COL_CORREC_VALAVT],")")
			}
		}
	}
	# remove the marked rows
	if(length(idx.rm)>0)
	{	tlog(6,"Removing ",length(idx.rm)," from the table")
		elect.table <- elect.table[-idx.rm,]
	}
	
	return(elect.table)
}




#############################################################################################
# Convert a data table originating from the Assembly DB into one comparable to a RNE table.
#
# general.table: table containing the personal data.
# elect.table: table containing the mandate data.
# 
# returns: the table resulting from the conversion.
#############################################################################################
assembly.convert.mandate.table <- function(general.table, elect.table)
{	tlog(2,"Converting the Assembly mandate table")
	
	# match the general and mandate tables
	indiv.idx <- match(elect.table[,COL_ATT_ELU_ID_ASSEMB], general.table[,COL_ATT_ELU_ID_ASSEMB])
	
	# get departments
	tlog(4,"Normalizing department names")
#	region.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_ATT_REG_NOM])))	# we actually don't need regions
	dpt.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_ATT_DPT_NOM])))
	dpts.conv <- senate.load.conversion.file(FILE_ASSEMB_CONV_DPTS)
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
#	dpt.codes <- trimws(elect.table[,COL_ATT_DPT_CODE])		# better to force using the codes from the RNE, as long as the names match
	dpt.codes <- sapply(dpt.table[dpt.idx,COL_ATT_DPT_CODE], function(x) 
			{	str <- strsplit(x, ",", fixed=TRUE)[[1]]
				str[length(str)]
			})
	dpt.ids <- dpt.table[dpt.idx,COL_ATT_DPT_ID]
	
	# retrieve circonscription codes
	circo.codes <- trimws(elect.table[,COL_ATT_CIRC_CODE])
	
	# convert mandate dates
	tlog(4,"Converting mandate dates")
	mdt.start.dates <- as.Date(elect.table[,COL_ATT_MDT_DBT], "%Y-%m-%d")
	mdt.end.dates <- as.Date(elect.table[,COL_ATT_MDT_FIN], "%Y-%m-%d")
	
	# mandate name
	tlog(4,"Inserting mandate names")
	mdt.names <- rep("DEPUTE", length(idx))
	
	# get motive of end of mandate
	tlog(4,"Processing end of mandate motive")
	mdt.motives <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_ATT_MDT_MOTIF])))
	
	# set up empty function dates
	fct.start.dates <- as.Date(rep(NA,length(mdt.start.dates)))
	fct.end.dates <- as.Date(rep(NA,length(mdt.end.dates)))
	
	# set up function names
	fct.names <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_ATT_FCT_NOM])))
	fct.names[fct.names=="MEMBRE"] <- NA		# member is not a function in the RNE
	
	# distinguish mandate rows from function rows
	# (the latter do not have any territorial information)
	idx.mandates <- which(!is.na(dpt.names))
	idx.functions <- which(is.na(dpt.names))
	
	# init vectors for new rows
	new.indiv.idx <- c()
	new.dpt.names <- c()
	new.dpt.codes <- c()
	new.dpt.ids <- c()
	new.circo.codes <- c()
	new.mdt.start.dates <- c()
	new.mdt.end.dates <- c()
	new.mdt.names <- c()
	new.mdt.motives <- c()
	new.fct.start.dates <- c()
	new.fct.end.dates <- c()
	new.fct.names <- c()
	
	# get function dates and names
	tlog(4,"Processing function dates and names, for each id in the table")
	ids <- sort(unique(elect.table[,COL_ATT_ELU_ID_ASSEMB]))
	for(i in 1:length(ids))
	{	tlog(6,"Processing id ",ids[i]," (",i,"/",length(ids),")")
		fct.idx <- which(elect.table[,COL_ATT_ELU_ID_ASSEMB]==ids[i] & is.na(dpt.names))
		if(length(fct.idx)>0)
		{	for(j in 1:length(fct.idx))
			{	tlog(8,"Processing function ",j,"/",length(fct.idx))
				mdt.idx <- which(elect.table[,COL_ATT_ELU_ID_ASSEMB]==ids[i] & !is.na(dpt.names)
							& mdt.start.dates<=mdt.start.dates[fct.idx[j]]
							& (is.na(mdt.end.dates) | mdt.end.dates>=mdt.end.dates[fct.idx[j]]))
				if(length(mdt.idx)==0)
				{	tlog(10,"ERROR: did not find any matching mandate for ",paste(elect.table[fct.idx[j],],collapse=","))
					stop("ERROR: did not find any matching mandate for ",paste(elect.table[fct.idx[j],],collapse=","))
				}
				else if(length(mdt.idx)>1)
				{	tlog(10,"ERROR: found several matching mandates:")
					tlog(10,paste(elect.table[fct.idx[j],],collapse=","))
					for(mdtidx in mdt.idx)
						tlog(10,paste(elect.table[mdtidx,],collapse=","))
					stop("ERROR: found several matching mandates")
				}
				else
				{	tlog(10,"Found a single matching mandate, now updating it")
					# if the mandate has no function yet: just update it
					if(is.na(fct.start.dates[mdt.idx]))
					{	fct.start.dates[mdt.idx] <- mdt.start.dates[fct.idx[j]]
						fct.end.dates[mdt.idx] <- mdt.end.dates[fct.idx[j]]
						fct.names[mdt.idx] <- fct.names[fct.idx[j]]
					}
					# else, if it already has a function: must insert new row
					else
					{	new.indiv.idx <- c(new.indiv.idx, indiv.idx[mdt.idx])
						new.dpt.names <- c(new.dpt.names, dpt.names[mdt.idx])
						new.dpt.codes <- c(new.dpt.codes, dpt.codes[mdt.idx])
						new.dpt.ids <- c(new.dpt.ids, dpt.ids[mdt.idx])
						new.circo.codes <- c(new.circo.codes, circo.codes[mdt.idx])
						new.mdt.start.dates <- c(new.mdt.start.dates, mdt.start.dates[mdt.idx])
						new.mdt.end.dates <- c(new.mdt.end.dates, mdt.end.dates[mdt.idx])
						new.mdt.names <- c(new.mdt.names, mdt.names[mdt.idx])
						new.mdt.motives <- c(new.mdt.motives, mdt.motives[mdt.idx])
						new.fct.start.dates <- c(new.fct.start.dates, mdt.start.dates[fct.idx[j]])
						new.fct.end.dates <- c(new.fct.end.dates, mdt.end.dates[fct.idx[j]])
						new.fct.names <- c(new.fct.names, fct.names[fct.idx[j]])
					}
				}
			}
		}
		else
			tlog(8,"No function to process")
	}
	
	# update vectors with new rows
	indiv.idx <- c(indiv.idx[-idx.functions], new.indiv.idx)
	dpt.names <- c(dpt.names[-idx.functions], new.dpt.names)
	dpt.codes <- c(dpt.codes[-idx.functions], new.dpt.codes)
	dpt.ids <- c(dpt.ids[-idx.functions], new.dpt.ids)
	circo.codes <- c(circo.codes[-idx.functions], new.circo.codes)
	mdt.start.dates <- c(mdt.start.dates[-idx.functions], new.mdt.start.dates)
	mdt.end.dates <- c(mdt.end.dates[-idx.functions], new.mdt.end.dates)
	mdt.names <- c(mdt.names[-idx.functions], new.mdt.names)
	mdt.motives <- c(mdt.motives[-idx.functions], new.mdt.motives)
	fct.start.dates <- c(fct.start.dates[-idx.functions], new.fct.start.dates)
	fct.end.dates <- c(fct.end.dates[-idx.functions], new.fct.end.dates)
	fct.names <- c(fct.names[-idx.functions], new.fct.names)
	
	# build assembly table
	tlog(4,"Building new Assembly mandate table")
	asn.tab <- data.frame(
			general.table[indiv.idx,COL_ATT_ELU_ID],		# universal id
			general.table[indiv.idx,COL_ATT_ELU_ID_RNE],	# RNE id
			general.table[indiv.idx,COL_ATT_ELU_ID_ASSEMB],	# assembly id
			general.table[indiv.idx,COL_ATT_ELU_NOM],		# last name
			general.table[indiv.idx,COL_ATT_ELU_PRENOM],	# first name
			general.table[indiv.idx,COL_ATT_ELU_NAIS_DATE],	# birth date
			general.table[indiv.idx,COL_ATT_ELU_DDD],		# death date
			general.table[indiv.idx,COL_ATT_ELU_SEXE],		# sex
			general.table[indiv.idx,COL_ATT_ELU_NAT],		# country
			general.table[indiv.idx,COL_ATT_ELU_NAIS_COM],	# birth city
			general.table[indiv.idx,COL_ATT_ELU_NAIS_DPT],	# birth department
			general.table[indiv.idx,COL_ATT_ELU_NAIS_PAYS],	# birth country
			general.table[indiv.idx,COL_ATT_PRO_CODE],		# occupation code
			general.table[indiv.idx,COL_ATT_PRO_NOM],		# occupation name
			dpt.ids,										# mandate department id
			dpt.codes,										# mandate department code
			dpt.names,										# mandate department name
			circo.codes,									# mandate circonscription
			mdt.names,										# mandate name
			mdt.start.dates,								# mandate start date
			mdt.end.dates,									# mandate end date
			mdt.motives,									# mandate end motive
			fct.names,										# function name
			fct.start.dates,								# function start date
			fct.end.dates,									# function end date
			rep("ASSEMBLEE", length(indiv.idx)),			# data source
			#
			check.names=FALSE,
			stringsAsFactors=FALSE
	)
	colnames(asn.tab) <- c(		
			COL_ATT_ELU_ID,
			COL_ATT_ELU_ID_RNE,
			COL_ATT_ELU_ID_ASSEMB,
			COL_ATT_ELU_NOM,
			COL_ATT_ELU_PRENOM,
			COL_ATT_ELU_NAIS_DATE,
			COL_ATT_ELU_DDD,
			COL_ATT_ELU_SEXE,
			COL_ATT_ELU_NAT,
			COL_ATT_ELU_NAIS_COM,
			COL_ATT_ELU_NAIS_DPT,
			COL_ATT_ELU_NAIS_PAYS,
			COL_ATT_PRO_CODE,
			COL_ATT_PRO_NOM,
			COL_ATT_DPT_ID,
			COL_ATT_DPT_CODE,
			COL_ATT_DPT_NOM,
			COL_ATT_CIRC_CODE,
			COL_ATT_MDT_NOM,
			COL_ATT_MDT_DBT,
			COL_ATT_MDT_FIN,
			COL_ATT_MDT_MOTIF,
			COL_ATT_FCT_NOM,
			COL_ATT_FCT_DBT,
			COL_ATT_FCT_FIN,
			COL_ATT_SOURCES
	)
	
	# order the Assembly table
	tlog(4,"Ordering the table rows")
	order.idx <- order(
		asn.tab[,COL_ATT_ELU_NOM], asn.tab[,COL_ATT_ELU_PRENOM], 
		asn.tab[,COL_ATT_MDT_DBT], asn.tab[,COL_ATT_MDT_FIN])
	asn.tab <- asn.tab[order.idx,]
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(asn.tab))
	asn.tab <- asn.tab[,norm.cols]
	
	# record the Assembly table
	folder <- file.path(FOLDER_COMP_SRC_ASSEMB, "D")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	asn.tab.file <- file.path(folder, "data_assemb.txt")
	tlog(4,"Recording the new table in ",asn.tab.file)
	write.table(x=asn.tab,
		file=asn.tab.file,		# name of file containing the new table
		quote=FALSE,			# no double quote around strings
		se="\t",				# use tabulations as separators
#		fileEncoding="UTF-8",	# character encoding
		row.names=FALSE,		# no names for rows
		col.names=TRUE			# record table headers
	)
	
	return(asn.tab)
}




#############################################################################################
# Adjusts the columns of the specified RNE table, in order to make it comparable with the
# currently processed Assembly table.
#
# data: original RNE table.
#
# returns: adjusted RNE table.
#############################################################################################
assembly.adjust.rne.table <- function(data)
{	tlog(2,"Adjusting RNE table")
	
	# add missing columns to the RNE table
	tlog(4,"Adding missing columns to the RNE table")
	cols <- data.frame(as.character(rep(NA,nrow(data))), as.Date(rep(NA,nrow(data))), as.character(rep(NA,nrow(data))),
			as.character(rep(NA,nrow(data))), as.character(rep(NA,nrow(data))), as.character(rep(NA,nrow(data))),
			stringsAsFactors=FALSE)
	colnames(cols) <- c(COL_ATT_ELU_ID_ASSEMB, COL_ATT_ELU_DDD, COL_ATT_ELU_NAT,
			COL_ATT_ELU_NAIS_COM, COL_ATT_ELU_NAIS_DPT, COL_ATT_ELU_NAIS_PAYS)
	rne.tab <- cbind(data, cols)
	
	# reorder its columns
	tlog(4,"Sorting RNE table rows")
	norm.cols <- intersect(COLS_ATT_NORMALIZED, colnames(rne.tab))
	rne.tab <- rne.tab[,norm.cols]
	
	# record temp RNE table
	out.folder <- file.path(FOLDER_COMP_SRC_ASSEMB, "D")
	dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)
	rne.tab.file <- file.path(out.folder, "data_rne1.txt")
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
# Compares the function dates in the RNE and Assembly tables. This function is only used in 
# function assembly.match.assembly.vs.rne.rows.
#
# asn.tab: rows matching the id in the Assembly table.
# rne.tab: rows matching the id in the RNE table.
# tolerance: number of days below which to dates are still consired equivalent.
# idx1: currently considered row in the RNE table.
# idx2: currently considered rows in the Assembly table (i.e. mandates matching with idx1).
# row.conv: map matching the RNE rows to the Assembly ones, to be updated by this function.
# 
# returns: the updated row.conv matching map.
#############################################################################################
assembly.check.function.dates <- function(asn.tab, rne.tab, tolerance, idx1, idx2, row.conv)
{	# get Assembly row(s) matching the function dates
	idx3 <- idx2[which((asn.tab[idx2,COL_ATT_FCT_DBT]==rne.tab[idx1,COL_ATT_FCT_DBT]
				| is.na(asn.tab[idx2,COL_ATT_FCT_DBT]) & is.na(rne.tab[idx1,COL_ATT_FCT_DBT]))
			& (asn.tab[idx2,COL_ATT_FCT_FIN]==rne.tab[idx1,COL_ATT_FCT_FIN]
				| is.na(asn.tab[idx2,COL_ATT_FCT_FIN]) & is.na(rne.tab[idx1,COL_ATT_FCT_FIN])))]
	
	# if there is exactly one: match done
	if(length(idx3)==1)
	{	tlog(8,"Found 1 matching Assembly row, nothing to do:")
		tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]),",",format(rne.tab[idx1,COL_ATT_MDT_FIN]))
		tlog(10,paste(asn.tab[idx3,],collapse=","),",",format(asn.tab[idx3,COL_ATT_MDT_DBT]),",",format(asn.tab[idx3,COL_ATT_MDT_FIN]))
		row.conv[idx3] <- idx1
	}
	
	# if there are several, we have a problem (manual correction)
	else if(length(idx3)>1)
	{	tlog(8,"Found several (",length(idx2),") matching Assembly rows, stopping")
		tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]),",",format(rne.tab[idx1,COL_ATT_MDT_FIN]))
		for(i2 in idx2)
			tlog(10,paste(asn.tab[i2,],collapse=","),",",format(asn.tab[i2,COL_ATT_MDT_DBT]),",",format(asn.tab[i2,COL_ATT_MDT_FIN]))
		stop("Found several matching Assembly rows")
	}
	
	# if there are none, compare function dates in an approximate way
	else if(length(idx3)<1)
	{	# difference in days between the start dates
		start.gaps <- sapply(asn.tab[idx2,COL_ATT_FCT_DBT], function(date)
					if(is.na(date))
					{	if(is.na(rne.tab[idx1,COL_ATT_FCT_DBT]))
							0
						else
							.Machine$integer.max/2
					}
					else
					{	if(is.na(rne.tab[idx1,COL_ATT_FCT_DBT]))
							.Machine$integer.max/2
						else
							abs(date-rne.tab[idx1,COL_ATT_FCT_DBT])
					})
		end.gaps <- sapply(asn.tab[idx2,COL_ATT_FCT_FIN], function(date)
					if(is.na(date))
					{	if(is.na(rne.tab[idx1,COL_ATT_FCT_FIN]))
							0
						else
							.Machine$integer.max/2
					}
					else
					{	if(is.na(rne.tab[idx1,COL_ATT_FCT_FIN]))
							.Machine$integer.max/2
						else
							abs(date-rne.tab[idx1,COL_ATT_FCT_FIN])
					})
		total.gaps <- start.gaps + end.gaps
		# get the closest period
		g <- which(total.gaps==min(total.gaps))
		
		# check it is within the tolerance
		if(length(g)==1 && start.gaps[g]<=tolerance && end.gaps[g]<=tolerance)
		{	idx3 <- idx2[g]
			tlog(8,"Found a matching Assembly row using approximate dates (replacing the first by the second):")
			tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_FCT_DBT]),",",format(rne.tab[idx1,COL_ATT_FCT_FIN]))
			tlog(10,paste(asn.tab[idx3,],collapse=","),",",format(asn.tab[idx3,COL_ATT_FCT_DBT]),",",format(asn.tab[idx3,COL_ATT_FCT_FIN]))
			row.conv[idx3] <- idx1
		}
		
		# if there are several rows matching: problem requiring a manual correction
		else if(length(g)>1 && all(start.gaps[g]<=tolerance & end.gaps[g]<=tolerance))
		{	idx3 <- idx2[g]
			tlog(8,"Found several (",length(idx3),") matching Assembly rows using approximate dates, stopping (",paste(idx3,collapse=","),")")
			tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_FCT_DBT]),",",format(rne.tab[idx1,COL_ATT_FCT_FIN]))
			for(i3 in idx3)
				tlog(10,paste(asn.tab[i3,],collapse=","),",",format(asn.tab[i3,COL_ATT_FCT_DBT]),",",format(asn.tab[i3,COL_ATT_FCT_FIN]))
			stop("Found several matching Assembly rows using approximate matching")
			
		}
		
		# still no matching row: check whether the RNE row has any function dates at all
		else if(is.na(rne.tab[idx1,COL_ATT_FCT_DBT]) && is.na(rne.tab[idx1,COL_ATT_FCT_FIN]))
		{	# just pick the first matching row, the other(s) will be added as new rows eventually
			idx3 <- idx2[g[1]]
			tlog(8,"Found several matching row, picking the first one as the function is missing in the RNE anyways):")
			tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_FCT_DBT]),",",format(rne.tab[idx1,COL_ATT_FCT_FIN]))
			tlog(10,paste(asn.tab[idx3,],collapse=","),",",format(asn.tab[idx3,COL_ATT_FCT_DBT]),",",format(asn.tab[idx3,COL_ATT_FCT_FIN]))
			row.conv[idx3] <- idx1
		}
		
		# if everything failed, the row is probably wrong and should be corrected manually
		else
		{	tlog(8,"Did not find any matching Assembly row, even with approximate start date (function)")
			tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_FCT_DBT]),",",format(rne.tab[idx1,COL_ATT_FCT_FIN]))
			for(i3 in idx3)
				tlog(10,paste(asn.tab[i3,],collapse=","),",",format(asn.tab[i3,COL_ATT_FCT_DBT]),",",format(asn.tab[i3,COL_ATT_FCT_FIN]))
			stop(8,"Did not find any matching Assembly row, even with approximate start date (function)")
		}
	}
	
	return(row.conv)
}




#############################################################################################
# Matches the rows from the Assembly table into the RNE table. The process is quite similar to
# the one permored when comparing several versions of the same table (script compare_tables.R).
#
# asn.tab: assembly data.
# rne.tab: RNE data.
# tolerance: number of days below which to dates are still consired equivalent.
# 
# returns: numbers of the RNE rows matching each Assembly row (or NA if no RNE row matches, in 
#		   which case the assembly data is new to the RNE).
#############################################################################################
assembly.match.assembly.vs.rne.rows <- function(asn.tab, rne.tab, tolerance)
{	# get all the RNE ids
	ids <- sort(unique(rne.tab[,COL_ATT_ELU_ID]))
	
	tlog(2,"Matching rows between both tables")
	row.conv <- rep(NA,nrow(asn.tab))	# Assembly->RNE conversion map
	# compute each RNE id
	for(i in 1:length(ids))
	{	tlog(4,"Processing id ",ids[i]," (",i,"/",length(ids),")")
		
		# get all assembly and RNE rows for the current RNE id
		idx.rne <- which(rne.tab[,COL_ATT_ELU_ID]==ids[i])
		idx.asn <- which(asn.tab[,COL_ATT_ELU_ID]==ids[i])
		
		if(!(rne.tab[idx.rne[1],COL_ATT_ELU_ID_RNE] %in% c(1513599)))		#(length(idx.asn)>0)
		{	# match each RNE row, possibly correcting its mandate dates
			for(j in 1:length(idx.rne))
			{	idx1 <- idx.rne[j]
				tlog(6,"Processing RNE row ",idx1," (",j,"/",length(idx.rne),")")
				
				# get the Assembly row(s) matching the mandate dates
				idx2 <- idx.asn[which(asn.tab[idx.asn,COL_ATT_MDT_DBT]==rne.tab[idx1,COL_ATT_MDT_DBT]
							& (asn.tab[idx.asn,COL_ATT_MDT_FIN]==rne.tab[idx1,COL_ATT_MDT_FIN]
								| is.na(asn.tab[idx.asn,COL_ATT_MDT_FIN]) & is.na(rne.tab[idx1,COL_ATT_MDT_FIN])))]
				#rbind(rne.tab[idx1,c(COL_ATT_ELU_ID,COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)], asn.tab[idx2,c(COL_ATT_ELU_ID,COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)])	# for debut
				
				# if there is exactly one: match done
				if(length(idx2)==1)
				{	tlog(8,"Found 1 exactly matching Assembly row, nothing to do:")
					tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]),",",format(rne.tab[idx1,COL_ATT_MDT_FIN]))
					tlog(10,paste(asn.tab[idx2,],collapse=","),",",format(asn.tab[idx2,COL_ATT_MDT_DBT]),",",format(asn.tab[idx2,COL_ATT_MDT_FIN]))
					row.conv[idx2] <- idx1
				}
				
				# if there are several, then check the function dates
				else if(length(idx2)>1)
					row.conv <- assembly.check.function.dates(asn.tab, rne.tab, tolerance, idx1, idx2, row.conv)
					
				# if there are none, compare dates in an approximate way
				else if(length(idx2)<1)
				{	# difference in days between the start dates
					start.gaps <- abs(asn.tab[idx.asn,COL_ATT_MDT_DBT] - rne.tab[idx1,COL_ATT_MDT_DBT])
					end.gaps <- sapply(asn.tab[idx.asn,COL_ATT_MDT_FIN], function(date)
								if(is.na(date))
								{	if(is.na(rne.tab[idx1,COL_ATT_MDT_FIN]))
										0
									else
										.Machine$integer.max
								}
								else
								{	if(is.na(rne.tab[idx1,COL_ATT_MDT_FIN]))
										.Machine$integer.max
									else
										abs(date-rne.tab[idx1,COL_ATT_MDT_FIN])
								})
					total.gaps <- start.gaps + end.gaps
					# get the closest period
					g <- which(total.gaps==min(total.gaps))
					
					# check it is within the tolerance
					if(length(g)==1 && start.gaps[g]<=tolerance && end.gaps[g]<=tolerance)
					{	idx2 <- idx.asn[g]
						tlog(8,"Found a matching Assembly row using approximate dates:")
						tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]),",",format(rne.tab[idx1,COL_ATT_MDT_FIN]))
						tlog(10,paste(asn.tab[idx2,],collapse=","),",",format(asn.tab[idx2,COL_ATT_MDT_DBT]),",",format(asn.tab[idx2,COL_ATT_MDT_FIN]))
						row.conv[idx2] <- idx1
					}
					
					# if there are several rows matching, check the function dates
					else if(length(g)>1 && all(start.gaps[g]<=tolerance & end.gaps[g]<=tolerance))
					{	idx2 <- idx.asn[g]
						row.conv <- assembly.check.function.dates(asn.tab, rne.tab, tolerance, idx1, idx2, row.conv)
					}
					
					# if everything failed, the row is probably wrong and should be corrected manually
					else
					{	tlog(8,"Did not find any matching Assembly row, even with approximate start date (mandate)")
						tlog(10,paste(rne.tab[idx1,],collapse=","),",",format(rne.tab[idx1,COL_ATT_MDT_DBT]),",",format(rne.tab[idx1,COL_ATT_MDT_FIN]))
						for(i2 in idx.asn)
							tlog(10,paste(asn.tab[i2,],collapse=","),",",format(asn.tab[i2,COL_ATT_MDT_DBT]),",",format(asn.tab[i2,COL_ATT_MDT_FIN]))
						stop(8,"Did not find any matching Assembly row, even with approximate start date (mandate)")						
					}
				}
			}
		}
	}
	
	# each value represents an Assembly row, and indicates the matching RNE row, or NA if no RNE row matches
	return(row.conv)
}




#############################################################################################
# Complete/correct the specified RNE table, using the specified Senate table, and based
# on the row matching described by row.conv.
#
# rne.tab: RNE table to update.
# asn.tab: reference Senate table used for updating the RNE table.
# row.conv: Senate2RNE row map.
#
# returns: updated RNE table.
#############################################################################################
assembly.update.rne.table <- function(rne.tab, asn.tab, row.conv)
{	tlog(2,"Updating the RNE table")
	result <- rne.tab
	
	# process the Assembly rows that are matched in the RNE table
	exist.rows <- which(!is.na(row.conv))
	tlog(4,"Updating the ",length(exist.rows)," matching rows in the RNE table")
	for(idx2 in exist.rows)
	{	idx1 <- row.conv[idx2]
		tlog(6,"Processing row ",idx1)
		
		# overwrite NA values using Assembly data
		cols <- which(is.na(result[idx1,]))
		result[idx1, cols] <- asn.tab[idx2, cols]
		
		# update source column
		result[idx1, COL_ATT_SOURCES] <- paste(result[idx1, COL_ATT_SOURCES], asn.tab[idx2, COL_ATT_SOURCES], sep=",")
		
		# force the mandate/function dates to the Assembly ones (considered more reliable)
		if(!is.na(asn.tab[idx2, COL_ATT_MDT_DBT]))
			result[idx1, COL_ATT_MDT_DBT] <- asn.tab[idx2, COL_ATT_MDT_DBT]
		if(!is.na(asn.tab[idx2, COL_ATT_MDT_FIN]))
			result[idx1, COL_ATT_MDT_FIN] <- asn.tab[idx2, COL_ATT_MDT_FIN]
		if(!is.na(asn.tab[idx2, COL_ATT_MDT_DBT]))
			result[idx1, COL_ATT_MDT_DBT] <- asn.tab[idx2, COL_ATT_MDT_DBT]
		if(!is.na(asn.tab[idx2, COL_ATT_MDT_FIN]))
			result[idx1, COL_ATT_MDT_FIN] <- asn.tab[idx2, COL_ATT_MDT_FIN]
	}
	
	# insert new Assembly rows into existing RNE table
	missing.rows <- which(is.na(row.conv))
	#missing.rows <- which(is.na(row.conv) & get.year(asn.tab[,COL_ATT_MDT_DBT])>=2001)
	tlog(4,"Inserting ",length(missing.rows)," missing rows in the RNE table")
	result <- rbind(result, asn.tab[missing.rows,])
	# sort the resulting table
	result <- result[order(result[,COL_ATT_DPT_CODE], 
					result[,COL_ATT_ELU_NOM], result[,COL_ATT_ELU_PRENOM], 
					result[,COL_ATT_MDT_DBT], result[,COL_ATT_MDT_FIN],
					result[,COL_ATT_FCT_DBT], result[,COL_ATT_FCT_FIN]) ,]
	
	# record the corrected/completed table
	rne.tab.file <- file.path(FOLDER_COMP_SRC_ASSEMB, type, "data_rne2.txt")
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
# Complete the RNE data with the mandates found in the Assembly database.
# 
# data: D table of the RNE.
# cache: whether or not to cache the map allowing to match people in the RNE and Assembly tables.
#        If FALSE, this operation is performed each time, otherwise it is computed only once.
# compare: whether or not to perform a comparison of the resulting RNE and Assembly tables
#          (used for debugging only).
#
# returns: the same table, completed using the Assembly DB.
#############################################################################################
assembly.integrate.data <- function(data, cache=TRUE, compare=FALSE)
{	# parse the large collection of XML files and create the raw tables
#	assembly.convert.xml()	# already done, do not execute anymore
	
	# load the general assembly table, containing individual information
	general.table <- assembly.load.general.table(cache)
	
	# load the mandate table
	elect.table <- assembly.load.elect.table()
	
	# build a mandate table comparable to a RNE table
	asn.tab <- assembly.convert.mandate.table(general.table, elect.table)
	
	# adjust the RNE table
	rne.tab <- assembly.adjust.rne.table(data)

	# match rows using a tolerance of a few days for dates
	row.conv <- assembly.match.assembly.vs.rne.rows(asn.tab, rne.tab, tolerance=14)

# use Assembly data to correct/complete existing RNE rows
result <- assembly.update.rne.table(rne.tab, asn.tab, row.conv)

# compare both tables
if(compare)
{	out.folder <- file.path(FOLDER_COMP_SRC_ASSEMB, "D")
	asn.tab.file <- file.path(out.folder, "data_assemb.txt")
	rne.tab.file <- file.path(out.folder, "data_rne2.txt")
	source("src/comparison/compare_tables.R")
	compare.tables(files0=rne.tab.file, files1=asn.tab.file, out.folder)
}
	
	return(result)
}
