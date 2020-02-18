#############################################################################################
# Functions used to inject data from secondary sources into the RNE.
# 
# 02/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# 
#############################################################################################
# start logging
start.rec.log(text="SenateSource")

# load senate table
data <- load.s.data(correct.data=TRUE)
dir.create(path=FOLDER_COMP_SRC_SEN, showWarnings=FALSE, recursive=TRUE)

# load individual table
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

# manually correct certain names
indiv.table[which(indiv.table[,COL_SENAT_ELU_NOM]=="Mathon-Poinat"),COL_SENAT_ELU_NOM] <- "MATHON"
indiv.table[which(indiv.table[,COL_SENAT_ELU_NOM]=="Ango Ela"),COL_SENAT_ELU_NOM] <- "ANGO ELA PAVLOPULOS"
indiv.table[which(indiv.table[,COL_SENAT_ELU_NOM]=="Arnell"),COL_SENAT_ELU_PRENOM] <- "Guillaume Jacques"
indiv.table[which(indiv.table[,COL_SENAT_ELU_NOM]=="San Vicente-Baudrin"),COL_SENAT_ELU_NOM] <- "SAN VICENTE"
indiv.table[which(indiv.table[,COL_SENAT_ELU_NOM]=="Ginesta"),COL_SENAT_ELU_PRENOM] <- "Jordi Dit Georges"
indiv.table[which(indiv.table[,COL_SENAT_ELU_NOM]=="Borvo Cohen-Seat"),COL_SENAT_ELU_NOM] <- "BORVO"

# load senatorial mandates table
elect.table <- read.table(
	file=FILE_SENAT_ELEC_S,		# name of the data file
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
for(c in 1:ncol(elect.table))
	elect.table[which(elect.table[,c]==""),c] <- NA

# match the general and mandate tables
idx <- match(elect.table[,COL_SENAT_ELU_MATRI],indiv.table[,COL_SENAT_ELU_MATRI])

# normalize names
indiv.last.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_NOM])))
last.names <- indiv.last.names[idx]
indiv.first.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_ELU_PRENOM])))
first.names <- indiv.first.names[idx]

# convert birth/death dates
indiv.birth.dates <- as.Date(indiv.table[,COL_SENAT_ELU_DDN], "%d/%m/%Y")
birth.dates <- indiv.birth.dates[idx]
indiv.death.dates <- as.Date(indiv.table[,COL_SENAT_ELU_DDD], "%d/%m/%Y")
death.dates <- indiv.death.dates[idx]

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
#	fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
)
indiv.dpt.names <- normalize.proper.nouns(remove.diacritics(indiv.table[,COL_SENAT_DPT_NOM]))
indiv.dpt.names[indiv.dpt.names=="CORSE DU SUD"] <- "CORSE SUD"
indiv.dpt.names[indiv.dpt.names=="ILES WALLIS ET FUTUNA"] <- "WALLIS ET FUTUNA"
indiv.dpt.names[indiv.dpt.names=="FRANCAIS ETABLIS HORS DE FRANCE"] <- "FRANCAIS DE L ETRANGER"
indiv.dpt.names[indiv.dpt.names=="MOSTAGANEM TIARET"] <- "MOSTAGANEM"
indiv.dpt.names[indiv.dpt.names=="ORAN TLEMCEN"] <- "ORAN"
indiv.dpt.names[indiv.dpt.names=="ORLEANSVILLE MEDEA"] <- "ORLEANSVILLE"
indiv.dpt.names[indiv.dpt.names=="SETIF BATNA"] <- "SETIF"
dpt.names <- indiv.dpt.names[idx]
indiv.dpt.idx <- match(indiv.dpt.names, dpt.table[,COL_ATT_DPT_NOM])
indiv.dpt.codes <- sapply(dpt.table[indiv.dpt.idx,COL_ATT_DPT_CODE], function(x) 
		{	str <- strsplit(x, ",", fixed=TRUE)[[1]]
			str[length(str)]
		})
dpt.codes <- indiv.dpt.codes[idx]

# retrieve political group
# NOTE: stored in the general table, so associated to each individual by opposition to each *mandate*
indiv.pol.nuances <- indiv.table[,COL_SENAT_ELU_NUANCE]
indiv.pol.nuances[indiv.pol.nuances=="Écologiste"] <- "ECO"
indiv.pol.nuances[indiv.pol.nuances=="Aucun"] <- NA
indiv.pol.nuances[indiv.pol.nuances=="C"] <- "COM"
indiv.pol.nuances[indiv.pol.nuances=="C.N.I.P."] <- "CNIP"		# missing
indiv.pol.nuances[indiv.pol.nuances=="CRARS"] <- "CRARS"		# missing
indiv.pol.nuances[indiv.pol.nuances=="CRCE"] <- "CRCE"			# missing
indiv.pol.nuances[indiv.pol.nuances=="G.D."] <- "GD"			# missing
indiv.pol.nuances[indiv.pol.nuances=="G.D.S.R.G."] <- "GDSRG"	# missing
indiv.pol.nuances[indiv.pol.nuances=="Indetermine(e)/Indefini(e)"] <- NA
indiv.pol.nuances[indiv.pol.nuances=="LaREM"] <- "REM"
indiv.pol.nuances[indiv.pol.nuances=="Les Indépendants"] <- "LI"# missing
indiv.pol.nuances[indiv.pol.nuances=="Les Républicains"] <- "LR"
indiv.pol.nuances[indiv.pol.nuances=="MRP"] <- "MRP"			# missing
indiv.pol.nuances[indiv.pol.nuances=="NI"] <- "NI"				# missing
indiv.pol.nuances[indiv.pol.nuances=="R.D.E."] <- "RDE"			# missing
indiv.pol.nuances[indiv.pol.nuances=="R.I.A.S."] <- "RIAS"		# missing
indiv.pol.nuances[indiv.pol.nuances=="RDSE"] <- "RDSE"			# missing
indiv.pol.nuances[indiv.pol.nuances=="RI"] <- "RI"				# missing
indiv.pol.nuances[indiv.pol.nuances=="RP"] <- "RP"				# missing
indiv.pol.nuances[indiv.pol.nuances=="RPCD"] <- "RPCD"			# missing
indiv.pol.nuances[indiv.pol.nuances=="RPR"] <- "RPR"			# missing
indiv.pol.nuances[indiv.pol.nuances=="SOCR"] <- "SOCR"			# missing
indiv.pol.nuances[indiv.pol.nuances=="U.C.D.P."] <- "UCDP"		# missing
indiv.pol.nuances[indiv.pol.nuances=="U.D.R."] <- "UDR"			# missing
indiv.pol.nuances[indiv.pol.nuances=="U.N.R."] <- "UNR"			# missing
indiv.pol.nuances[indiv.pol.nuances=="U.R.E.I."] <- "UREI"		# missing
indiv.pol.nuances[indiv.pol.nuances=="UC"] <- "UC"				# missing
pol.nuances <- indiv.pol.nuances[idx]
#	pol.nuances <- rep(NA,length(idx))

# retrieve occupations
indiv.occ.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[idx,COL_SENAT_PRO_NOM])))
#indiv.occ.names[indiv.occ.names=="AGRICULTEURS EXPLOITANTS"] <- "AGRICULTEURS PROPRIETAIRES EXPLOIT"
#indiv.occ.names[indiv.occ.names=="ANCIENS AGRICULTEURS EXPLOITANTS"] <- "RETRAITES AGRICOLES"
#indiv.occ.names[indiv.occ.names=="ANCIENS ARTISANS COMMERCANTS CHEFS D ENTREPRISE"] <- "RETR ARTIS COMMERC CHEFS D ENTREP"
#indiv.occ.names[indiv.occ.names=="ANCIENS CADRES"] <- "RETRAITES SALARIES PRIVES"
#indiv.occ.names[indiv.occ.names=="ANCIENS CADRES ET PROFESSIONS LIBERALES"] <- "RETRAITES DES PROFESSIONS LIBERALES"
#indiv.occ.names[indiv.occ.names=="ANCIENS PROFESSIONS INTERMEDIAIRES"] <- "RETRAITES SALARIES PRIVES"
#indiv.occ.names[indiv.occ.names=="ARTISANS"] <- "ARTISANS"
#indiv.occ.names[indiv.occ.names=="ARTISANS COMMERCANTS ET CHEFS D ENTREPRISE"] <- "COMMERCANTS"
#indiv.occ.names[indiv.occ.names=="AUTRES PERSONNES SANS ACTIVITE PROFESSIONNELLE"] <- "SANS PROFESSION DECLAREE"
#indiv.occ.names[indiv.occ.names=="AVOCATS"] <- "AVOCATS"
#indiv.occ.names[indiv.occ.names=="CADRES ADMINISTRATIFS ET COMMERCIAUX D ENTREPRISE"] <- "AUTRES CADRES SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="CADRES D ENTREPRISE"] <- "CADRES SUPERIEURS SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="CADRES DE LA FONCTION PUBLIQUE"] <- "CADRES ENTREPRISES PUBLIQUES"
#indiv.occ.names[indiv.occ.names=="CADRES DE LA PUBLICITE CADRES DES RELATIONS PUBLIQUES"] <- "AUTRES CADRES SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="CADRES ET PROFESSIONS INTELLECTUELLES SUPERIEURES"] <- "AUTRES CADRES SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="CHEFS D ENTREPRISE DE 1 SALARIES OU PLUS"] <- "ADMINISTRATEURS DE SOCIETES"
#indiv.occ.names[indiv.occ.names=="CHIRURGIENS DENTISTES LIBERAUX OU SALARIES"] <- "CHIRURGIENS"
#indiv.occ.names[indiv.occ.names=="CLERGE RELIGIEUX"] <- "MINISTRES DU CULTE"
#indiv.occ.names[indiv.occ.names=="COMMERCANTS ET ASSIMILES"] <- "COMMERCANTS"
#indiv.occ.names[indiv.occ.names=="CONTREMAITRE ET AGENTS DE MAITRISE"] <- "CONTREMAITRES"
#indiv.occ.names[indiv.occ.names=="EMPLOYES ADMINISTRATIFS D ENTREPRISE"] <- "EMPLOYES SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="EMPLOYES CIVILS ET AGENTS DE SERVICE DE LA FONCTION PUBLIQUE"] <- "AGENTS SUBALTERNES ENTR PUBLIQUES"
#indiv.occ.names[indiv.occ.names=="EMPLOYES DE COMMERCE"] <- "REPRESENTANTS DE COMMERCE"
#indiv.occ.names[indiv.occ.names=="HUISSIERS DE JUSTICE OFFICIERS MINISTERIELS ET PROFESSIONS LIBERALES DIVERS"] <- "HUISSIERS"
#indiv.occ.names[indiv.occ.names=="INGENIEURS ET CADRES TECHNIQUES D ENTREPRISE"] <- "INGENIEURS"
#indiv.occ.names[indiv.occ.names=="INSTITUTEURS ET ASSIMILES"] <- "ENSEIGNANTS 1ER DEG DIRECTEURS ECOLE"
#indiv.occ.names[indiv.occ.names=="JOURNALISTES SECRETAIRES DE REDACTION"] <- "JOURNALISTES ET AUTRES MEDIAS"
#indiv.occ.names[indiv.occ.names=="MAGISTRATS"] <- "MAGISTRATS"
#indiv.occ.names[indiv.occ.names=="MEDECINS HOSPITALIERS SANS ACTIVITE LIBERALE"] <- "MEDECINS"
#indiv.occ.names[indiv.occ.names=="MEDECINS LIBERAUX GENERALISTES"] <- "MEDECINS"
#indiv.occ.names[indiv.occ.names=="OUVRIERS"] <- "OUVRIERS SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="OUVRIERS QUALIFIES DE TYPE INDUSTRIEL"] <- "AGENTS TECHNIQUE ET TECHNICIENS"
#indiv.occ.names[indiv.occ.names=="PERSONNELS DE DIRECTION DE LA FONCTION PUBLIQUE"] <- "CADRES SUP ENTREPRISES PUBLIQUES"
#indiv.occ.names[indiv.occ.names=="PERSONNES DIVERSES SANS ACTIVITE PROFESSIONNELLE DE MOINS DE 6 ANS SAUF RETRAITES"] <- "SANS PROFESSION DECLAREE" 
#indiv.occ.names[indiv.occ.names=="PHARMACIENS LIBERAUX"] <- "PHARMACIENS"
#indiv.occ.names[indiv.occ.names=="POLICIERS ET MILITAIRES"] <- "AUTRES PROFESSIONS"
#indiv.occ.names[indiv.occ.names=="PROFESSEURS PROFESSIONS SCIENTIFIQUES"] <- "PROFESSEURS DE FACULTE"
#indiv.occ.names[indiv.occ.names=="PROFESSIONS DE L INFORMATION DES ARTS ET SPECTACLES"] <- "HOMMES DE LETTRES ET ARTISTES"
#indiv.occ.names[indiv.occ.names=="PROFESSIONS INTERMEDIAIRES ADMINISTRATIVES DE LA FONCTION PUBLIQUE"] <- "EMPLOYES AUTRES ENTREP PUBLIQUES"
#indiv.occ.names[indiv.occ.names=="PROFESSIONS INTERMEDIAIRES ADMINISTRATIVES ET COMMERCIALES D ENTREPRISE"] <- "AUTRES CADRES SECTEUR PRIVE"
#indiv.occ.names[indiv.occ.names=="PROFESSIONS INTERMEDIAIRES DE LA SANTE ET DU TRAVAIL SOCIAL"] <- "SALARIES DU SECTEUR MEDICAL"
#indiv.occ.names[indiv.occ.names=="PROFESSIONS LIBERALES"] <- "AUTRES PROFESSIONS LIBERALES"
#indiv.occ.names[indiv.occ.names=="RETRAITES"] <- "AUTRES RETRAITES"
#indiv.occ.names[indiv.occ.names=="TECHNICIENS"] <- "AGENTS TECHNIQUE ET TECHNICIENS"
#indiv.occ.names[indiv.occ.names=="VETERINAIRES LIBERAUX OU SALARIES"] <- "VETERINAIRES"
occ.names <- indiv.occ.names[idx]
#	occ.names <- rep(NA,length(idx))
# TODO: we could retrieve codes from the merged main table
	occ.codes <- rep(NA,length(idx))

# convert mandate dates
start.dates <- as.Date(elect.table[,COL_SENAT_MDT_DBT], "%d/%m/%Y")
end.dates <- as.Date(elect.table[,COL_SENAT_MDT_FIN], "%d/%m/%Y")

# motive of end of mandate
motives <- trimws(normalize.proper.nouns(remove.diacritics(elect.table[,COL_SENAT_MDT_MOTIF])))
#	motives <- rep(NA,length(idx))

# fonction names
#fonct.names <- trimws(normalize.proper.nouns(remove.diacritics(indiv.table[idx,COL_SENAT_FCT_NOM])))
	fonct.names <- rep(NA,length(idx))
# TODO "function" also seems to be available under a dynamic form in the Groupes table

# match senate ids with RNE ids
rne.ids <- rep(NA, nrow(indiv.table))
univ.ids <- paste("SEN",indiv.table[,COL_SENAT_ELU_MATRI],sep="_")
for(i in 1:nrow(indiv.table))
{	tlog(2, "Processing row ",i,"/",nrow(indiv.table))
	tmp <- which(data[,COL_ATT_ELU_NOM]==indiv.last.names[i]
			& data[,COL_ATT_ELU_PRENOM]==indiv.first.names[i]
			& data[,COL_ATT_ELU_DDN]==indiv.birth.dates[i])
#			& data[,COL_ATT_DPT_NOM]==indiv.dpt.names[i])
	if(length(tmp)==0)
		tlog(4, "No match for row: ",paste(indiv.table[i,],collapse=","))
	else 
	{	tmp.ids <- sort(unique(data[tmp,COL_ATT_ELU_ID_RNE]))
		if(length(tmp.ids)>1)
		{	tlog(4, "Found several matches (ids ",paste(tmp.ids,collapse=","),") for row: ",paste(indiv.table[i,],collapse=","))
			# display more details?
			stop("Found several matches")
		}
		else
		{	tlog(4, "Found a single RNE entry (id ",tmp.ids,", name ",
					paste(data[tmp[1],c(COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_DDN)],collapse=","),
					") for row: ",paste(indiv.table[i,],collapse=","))
			rne.ids[i] <- tmp.ids
			univ.ids[i] <- data[tmp[1],COL_ATT_ELU_ID]
		}	
	}
}
ids.rne <- rne.ids[idx]
ids.univ <- univ.ids[idx]

## match RNE ids with senate ids
#rne.ids <- sort(unique(data[,COL_ATT_ELU_ID]))
#rne.rows <- match(rne.ids,data[,COL_ATT_ELU_ID])
#sen.ids <- rep(NA,length(rne.ids))
#for(i in 1:length(rne.ids))
#{	tlog(2, "Processing row ",i,"/",length(rne.ids))
#	tmp <- which(indiv.last.names==data[rne.rows[i],COL_ATT_ELU_NOM]
#			& indiv.first.names==data[rne.rows[i],COL_ATT_ELU_PRENOM]
#			& indiv.birth.dates==data[rne.rows[i],COL_ATT_ELU_DDN])
##			& indiv.dpt.names==data[rne.rows[i],COL_ATT_DPT_NOM])
#	if(length(tmp)==0)
#		tlog(4, "No match for row: ",paste(data[rne.rows[i],],collapse=","))
#	else 
#	{	tmp.ids <- sort(unique(indiv.table[tmp,COL_SENAT_ELU_MATRI]))
#		if(length(tmp.ids)>1)
#		{	tlog(4, "Found several matches (ids ",paste(tmp.ids,collapse=","),") for row: ",paste(data[rne.rows[i],],collapse=","))
#			# display more details?
#		}
#		else
#		{	tlog(4, "Found a single Senate entry (id ",tmp.ids,", name ",
#					paste(indiv.last.names[tmp[1]],indiv.first.names[tmp[1]],indiv.birth.dates[tmp[1]],sep=","),
#					") for row: ",paste(indiv.table[i,],collapse=","))
#			sen.ids[i] <- tmp.ids
#		}	
#	}
#}

# build senate table
tab <- data.frame(
	dpt.codes,								# department code
	dpt.names,								# department name
	ids.univ,								# universal id
	ids.rne,								# RNE id
	indiv.table[idx,COL_SENAT_ELU_MATRI],	# senate id
	last.names,								# last name
	first.names,							# first name
	birth.dates,							# birth date
	death.dates,							# death date
	rep(NA, length(idx)),					# sex
	rep("FRANCAISE", length(idx)),			# country
	pol.nuances,							# political group
	occ.codes,								# occupation code
	occ.names,								# occupation name
	rep("SENATEUR", length(idx)),			# mandate name
	start.dates,							# mandate start date
	end.dates,								# mandate end date
	motives,								# mandate end motive
	fonct.names,							# function name
	rep(NA, length(idx)),					# function start date
	rep(NA, length(idx)),					# function end date
	rep(NA, length(idx)),					# function end motive
	#
	check.names=FALSE,
	stringsAsFactors=FALSE,
	rep("SENAT", length(idx))
)
colnames(tab) <- c(		
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

# keep only mandates starting before 2001
#pre.mandates <- which(start.dates<as.Date("2001/01/01")) 
#tab <- tab[-pre.mandates,]

# order Senate table
tab <- tab[order(tab[,COL_ATT_DPT_CODE], 
				tab[,COL_ATT_ELU_NOM], tab[,COL_ATT_ELU_PRENOM], 
				tab[,COL_ATT_MDT_DBT], tab[,COL_ATT_MDT_FIN]) ,]
# record Senate table
tab.sen.file <- file.path(FOLDER_COMP_SRC_SEN, "senat_senat.txt")
write.table(x=tab,
	file=tab.sen.file,		# name of file containing the new table
	quote=FALSE,			# no double quote around strings
	se="\t",				# use tabulations as separators
#	fileEncoding="UTF-8",	# character encoding
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)
# complete RNE table
tmp.data <- cbind(data, rep(NA,nrow(data)), as.Date(rep(NA,nrow(data))), rep(NA,nrow(data)))
colnames(tmp.data)[(ncol(tmp.data)-2):ncol(tmp.data)] <- c(COL_ATT_ELU_ID_SENAT,COL_ATT_ELU_DDD,COL_ATT_ELU_NAT)
tmp.data <- tmp.data[,colnames(tab)]
# record RNE table
tab.rne.file <- file.path(FOLDER_COMP_SRC_SEN, "rne_senat.txt")
write.table(x=tmp.data,
	file=tab.rne.file,		# name of file containing the new table
	quote=FALSE,			# no double quote around strings
	se="\t",				# use tabulations as separators
#	fileEncoding="UTF-8",	# character encoding
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)

# match rows using a tolerance of a few days in dates
TOLERANCE <- 14
ids <- sort(unique(tmp.data[,COL_ATT_ELU_ID]))
tlog(2,"Matching rows between both tables")
row.conv <- rep(NA,nrow(tab))
for(i in 1:length(ids))
{	tlog(4,"Processing id ",ids[i]," (",i,"/",length(ids),")")
	
	# get all rows for the current id
	idx.data <- which(tmp.data[,COL_ATT_ELU_ID]==ids[i])
	idx.tab <- which(tab[,COL_ATT_ELU_ID]==ids[i])
	# match the rows, possibly correcting the RNE date
	for(j in 1:length(idx.data))
	{	idx1 <- idx.data[j]
		tlog(6,"Processing row ",idx1," (",j,"/",length(idx.data),")")
		
		idx2 <- idx.tab[tab[idx.tab,COL_ATT_MDT_DBT]==tmp.data[idx1,COL_ATT_MDT_DBT]]
		if(length(idx2)==1)
		{	tlog(8,"Found 1 matching row, nothing to do")
			row.conv[idx2] <- idx1
		}
		else if(length(idx2)>1)
		{	tlog(8,"Found several matching rows")
			stop("Found several matching rows")
		}
		else if(length(idx2)<1)
		{	gaps <- abs(tab[idx.tab,COL_ATT_MDT_DBT] - tmp.data[idx1,COL_ATT_MDT_DBT])
			g <- which.min(gaps)
			if(gaps[g]<=TOLERANCE)
			{	idx2 <- idx.tab[g]
				tlog(8,"Found a matching row using approximate date replacing the first by the second:")
#				tlog(10,paste(tmp.data[idx1,],collapse=","),",",format(tmp.data[idx1,COL_ATT_MDT_DBT]))
#				tlog(10,paste(tab[idx2,],collapse=","),",",format(tab[idx2,COL_ATT_MDT_DBT]))
				row.conv[idx2] <- idx1
			}
			else
			{	tlog(8,"Did not find any matching row, even with approximate start date")
				tlog(10,paste(tmp.data[idx1,],collapse=","),",",format(tmp.data[idx1,COL_ATT_MDT_DBT]))
			}
		}
	}
}

# use Senate data to correct/complete existing RNE rows
exist.rows <- which(!is.na(row.conv))
tlog(2,"Updating the ",length(exist.rows)," matching rows in the RNE table")
for(idx2 in exist.rows)
{	idx1 <- row.conv[idx2]
	tlog(4,"Processing row ",idx1)
	
	# overwrite NA values using Senate data
	cols <- which(is.na(tmp.data[idx1,]))
	tmp.data[idx1, cols] <- tab[idx2, cols]
	
	# update source column
	tmp.data[idx1, COL_ATT_SOURCES] <- paste(tmp.data[idx1, COL_ATT_SOURCES], tab[idx2, COL_ATT_SOURCES], sep=",")
	
	# force both mandate dates to the Senate one
	tmp.data[idx1, c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)] <- tab[idx2, c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)]
}

# insert new Senate rows into existing RNE table
missing.rows <- which(is.na(row.conv))
tlog(2,"Inserting ",length(missing.rows)," missing rows in the RNE table")
tmp.data <- rbind(tmp.data, tab[missing.rows,])
# sort the resulting table
tmp.data <- tmp.data[order(tmp.data[,COL_ATT_DPT_CODE], 
				tmp.data[,COL_ATT_ELU_NOM], tmp.data[,COL_ATT_ELU_PRENOM], 
				tmp.data[,COL_ATT_MDT_DBT], tmp.data[,COL_ATT_MDT_FIN]) ,]

# record the corrected/completed table
tab.rne.file <- file.path(FOLDER_COMP_SRC_SEN, "rne_senat2.txt")
write.table(x=tmp.data,
	file=tab.rne.file,		# name of file containing the new table
	quote=FALSE,			# no double quote around strings
	se="\t",				# use tabulations as separators
#	fileEncoding="UTF-8",	# character encoding
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)

# compare both tables
source("src/comparison/compare_tables.R")
compare.tables(files0=tab.rne.file, files1=tab.sen.file, out.folder=FOLDER_COMP_SRC_SEN)

# close the log file
tlog(0,"Comparison done")
end.rec.log()

# TODO only required if person completely missing from RNE table:
# - firstnames to sex (?) using majority sex from merged table (?)
