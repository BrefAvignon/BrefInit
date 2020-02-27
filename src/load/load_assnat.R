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
