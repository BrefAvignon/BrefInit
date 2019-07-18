#############################################################################################
# Checks the municipal councilor tables. 
# 
# 07/2019 Vincent Labatut
#############################################################################################
source("src/common.R")
source("src/verification.R")

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
cols <- list(
	list(name="Code du département (Maire)", tp="cat"),
	list(name="Libellé de département (Maires)", tp="nom"),
	list(name="Code Insee de la commune", tp="cat"),
	list(name="Libellé de la commune", tp="nom"),
	list(name="Population de la commune", tp="num"),
	list(name="Nom de l'élu", tp="nom"),
	list(name="Prénom de l'élu", tp="nom"),
	list(name="Code sexe", tp="cat"),
	list(name="Date de naissance", tp="dat"),
	list(name="Code profession", tp="cat"),
	list(name="Libellé de la profession", tp="nom"),
	list(name="Date de début du mandat", tp="dat"),
	list(name="Date de fin du mandat", tp="dat"),
	list(name="Motif de fin de mandat", tp="cat"),
	list(name="Libellé de fonction", tp="cat"),
	list(name="Date de début de la fonction", tp="dat"),
	list(name="Date de fin de la fonction", tp="dat"),
	list(name="Motif de fin de fonction", tp="cat"),
	list(name="Nuance politique (C. Mun.)", tp="cat"),
	list(name="N° Identification d'un élu", tp="key")
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
#tlog(0, "Distribution of each column")
#for(col in cols)
#{	tlog(2, "Column \"",col,"\"")
#	tlog(0, capture.output(print(table(data[,col]))))
#}

check.col.numerical(data=data, col="Population de la commune", basename="CM_population_commune")
check.col.categorical(data=data, col="Libellé de département (Maires)", basename="CM_code_dpt")
