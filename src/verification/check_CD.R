#############################################################################################
# Checks the departmental councilor tables. 
# 
# 07/2019 Vincent Labatut
#############################################################################################
source("src/common/include.R")
source("src/verification/sumup_col.R")

# start logging
start.rec.log(text="CD")

# create output folder
out.folder <- file.path(FOLDER_OUT, "CD")
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# filenames to process
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

# load all the tables
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
		as.is=TRUE					# don't convert strings to factors
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
		vals <- as.Date(data[,col$name], "%d/%m/%Y")
		#format(x, format="%Y/%m/%d")
		data <- data[, names(data)!=col$name]
		data <- cbind(data,vals)
		names(data)[ncol(data)] <- col$name
	}
	else
		tlog(2,"Col. \"",col$name,"\": not a date")
}

# summarizes each column separately
tlog(0,"Examining each column separately")
check.cols(data=data, cols=cols, out.folder=out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
