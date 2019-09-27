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
#
# returns: data frame made of the cleaned data contained in the files.
#############################################################################################
load.data <- function(filenames, cols)
{	# load all the tables
	data <- NULL
	for(filename in filenames)
	{	fn <- file.path(FOLDER_IN, filename)
		tlog(0,"Loading file \"",fn,"\"")
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
				fileEncoding="Latin1"
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

#col <- colnames(data)[14]
#print(col)
#col0 <- cols[[14]]$name
#print(col0)
#print(col==col0)
#print(Encoding(col))
#print(Encoding(col0))
#
#	# convert headers to unicode
##	colnames(data) <- iconv(colnames(data), from="latin1", to="UTF-8")
#	colnames(data) <- enc2utf8(colnames(data))
#	
#col <- colnames(data)[14]
#print(col)
#print(col0)
#print(col==col0)
#print(Encoding(col))
#print(Encoding(col0))

	# convert date columns to R dates
	tlog(0,"Converting date columns to actual R dates")
	for(col in cols)
	{	if(col$tp=="dat")
		{	tlog(2,"Col. \"",col$name,"\": CONVERTING")
			vals <- as.Date(data[,col$name], "%d/%m/%Y")
			#format(x, format="%Y/%m/%d")
			data <- data[, names(data)!=col$name]
			data <- cbind(data,vals)
			names(data)[ncol(data)] <- col$name
print(class(data[,col$name]))			
		}
		else
			tlog(2,"Col. \"",col$name,"\": not a date")
	}
	
	# setting appropriate encoding of string columns
	tlog(0,"Converting string encoding")
	for(c in 1:ncol(data))
	{	if(is.character(data[,c]))
		{	tlog(2,"Col. \"",colnames(data)[c],"\": CONVERTING")
			data[,c] <- iconv(x=data[,c], from="Latin1", to="UTF8")
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
{	# filenames to process
	filenames <- c(
		"F Tous CD.txt"
	)
	
	# names of the columns
	cols <- list(
		list(name="Code du département", basename="dpt_code", tp="cat"),
		list(name="Libellé du département", basename="dpt_nom", tp="nom"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Code du canton", basename="canton_code", tp="cat"),
		list(name="Libellé du canton", basename="canton_nom", tp="nom"),
		list(name="Nuance politique (C. Gén.)", basename="nuance_pol", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Libellé de mandat", basename="mandat_nom", tp="cat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat")
	)
	for(col in cols)
	{	name <- col$name
		print(name)
		print(Encoding(name))
		name <- enc2utf8(as(name, "character"))
		print(name)
		print(Encoding(name))
	}
	
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for municipal counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cm.data <- function()
{	# filenames to process
	filenames <- c(
		"A Tous CM 01 30.txt",	# mainland departments #1
		"B Tous CM 31 60.txt",	# mainland departments #2
		"C Tous CM 61 95.txt",	# mainland departments #3
		"D Tous CM OM.txt"		# overseas territories
	)
	
	# names of the columns
	cols <- list(
		list(name="Code du département (Maire)", basename="dpt_code", tp="cat"),
		list(name="Libellé de département (Maires)", basename="dpt_code", tp="nom"),
		list(name="Code Insee de la commune", basename="ville_code", tp="cat"),
		list(name="Libellé de la commune", basename="ville_nom", tp="nom"),
		list(name="Population de la commune", basename="ville_population", tp="num"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat"),
		list(name="Nuance politique (C. Mun.)", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for regional counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.cr.data <- function()
{	# filenames to process
	filenames <- c(
		"G Tous CR.txt"
	)
	
	# names of the columns
	cols <- list(
		list(name="Code région", basename="region_code", tp="cat"),
		list(name="Libellé de la région", basename="region_code", tp="nom"),
		list(name="Code du département", basename="dpt_code", tp="cat"),
		list(name="Libellé de département", basename="dpt_nom", tp="nom"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Libellé de mandat", basename="mandat_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat"),
		list(name="Nuance mandat", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for members of the parliament.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.d.data <- function()
{	# filenames to process
	filenames <- c(
		"H Tous Deputes.txt"
	)
	
	# names of the columns
	cols <- list(
		list(name="Code du département", basename="dpt_code", tp="cat"),
		list(name="Libellé du département", basename="dpt_code", tp="nom"),
		list(name="Code de la cir.législative", basename="circo_code", tp="cat"),
		list(name="Libellé de la cir.législative", basename="circo_code", tp="nom"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Libellé de mandat", basename="mandat_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat"),
		list(name="Nuance politique (Député)", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for European counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.de.data <- function()
{	# filenames to process
	filenames <- c(
		"J Tous RPE.txt"
	)
	
	# names of the columns
	cols <- list(
		list(name="CodeCirER", basename="circo_code", tp="cat"),
		list(name="LibelléCirER", basename="circo_code", tp="nom"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Libellé de mandat", basename="mandat_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Nuance politique (Rep. P.E.)", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for EPCI counsilors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.epci.data <- function()
{	# filenames to process
	filenames <- c(
		"E Tous Membres EPCI.txt"
	)
	
# names of the columns
	cols <- list(
		list(name="Code département EPCI", basename="dpt_code_epci", tp="cat"),
		list(name="Code département commune rattachée", basename="dpt_code_com", tp="cat"),
		list(name="Code de la commune", basename="ville_code", tp="cat"),
		list(name="Libellé commune rattachée", basename="ville_nom", tp="nom"),
		list(name="N° SIREN", basename="epci_siren", tp="cat"),
		list(name="Libellé de l'EPCI", basename="epci_nom", tp="nom"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat"),
		list(name="Nuance mandat", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}




#############################################################################################
# Loads the tables for mayors.
#
# returns: data frame made of the cleaned data contained in the appropriate files.
#############################################################################################
load.m.data <- function()
{	# filenames to process
	filenames <- c(
		"K Tous Maires.txt"
	)
	
	# names of the columns
	cols <- list(
		list(name="Code du département (Maire)", basename="dpt_code", tp="cat"),
		list(name="Libellé de département (Maires)", basename="dpt_code", tp="nom"),
		list(name="Code Insee de la commune", basename="ville_code", tp="cat"),
		list(name="Libellé de la commune", basename="ville_nom", tp="nom"),
		list(name="Population de la commune", basename="ville_population", tp="num"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Libellé de mandat", basename="mandat_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat"),
		list(name="Nuance politique (C. Mun.)", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	# convert population numbers to actual integers
	cn <- "Population de la commune"
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
{	# filenames to process
	filenames <- c(
		"I Tous Senateurs.txt"
	)
	
# names of the columns
	cols <- list(
		list(name="Code du département", basename="dpt_code", tp="cat"),
		list(name="Libellé du département", basename="dpt_code", tp="nom"),
		list(name="Nom de l'élu", basename="patronyme", tp="nom"),
		list(name="Prénom de l'élu", basename="prenom", tp="nom"),
		list(name="Date de naissance", basename="naissance_date", tp="dat"),
		list(name="Code sexe", basename="sexe", tp="cat"),
		list(name="Code profession", basename="profession_code", tp="cat"),
		list(name="Libellé de la profession", basename="profession_nom", tp="cat"),
		list(name="Libellé de mandat", basename="mandat_nom", tp="cat"),
		list(name="Date de début du mandat", basename="mandat_debut", tp="dat"),
		list(name="Date de fin du mandat", basename="mandat_fin", tp="dat"),
		list(name="Motif de fin de mandat", basename="mandat_motif", tp="cat"),
		list(name="Libellé de fonction", basename="fonction_nom", tp="cat"),
		list(name="Date de début de la fonction", basename="fonction_debut", tp="dat"),
		list(name="Date de fin de la fonction", basename="fonction_fin", tp="dat"),
		list(name="Motif de fin de fonction", basename="fonction_motif", tp="cat"),
		list(name="Nuance politique (Sénateur)", basename="nuance_pol", tp="cat"),
		list(name="N° Identification d'un élu", basename="elu_id", tp="cat")
	)
	
	# load the data
	data <- load.data(filenames, cols)
	
	res <- list(data=data,cols=cols)
	return(res)
}
