#############################################################################################
# Checks the municipal councilor tables. 
# 
# 07/2019 Vincent Labatut
#############################################################################################
source("src/common.R")

# start logging
start.rec.log(text="CM")

# filenames to process
filenames <- c(
	"A Tous CM 01 30.txt",
	"B Tous CM 31 60.txt",
	"C Tous CM 61 95.txt",
	"D Tous CM OM.txt"
)

# names of the columns
cols <- c(
	"Code du département (Maire)",
	"Libellé de département (Maires)",
	"Code Insee de la commune",
	"Libellé de la commune",
	"Population de la commune",
	"Nom de l'élu",
	"Prénom de l'élu",
	"Code sexe",
	"Date de naissance",
	"Code profession",
	"Libellé de la profession",
	"Date de début du mandat",
	"Date de fin du mandat",
	"Motif de fin de mandat",
	"Libellé de fonction",
	"Date de début de la fonction",
	"Date de fin de la fonction",
	"Motif de fin de fonction",
	"Nuance politique (C. Mun.)",
	"N° Identification d'un élu"
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

# display unique values and their distribution
tlog(0, "Distribution of each column")
for(col in cols)
{	tlog(2, "Column \"",col,"\"")
#	tlog(toString(table(data[,col])))
	print(table(data[,col]))
}
