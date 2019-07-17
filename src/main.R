#############################################################################################
# Merges the RNE files (historical 2018/07/17 version)
# 
# 07/2019 Vincent Labatut
# setwd("D:/Users/Vincent/Documents/Travail/Ecrits/_Projets/Trajectoires pol/07. RNE/RNE 20180717")
#############################################################################################

# filenames to process
filenames <- c(
	"A Tous CM 01 30.txt",
	"B Tous CM 31 60.txt",
	"C Tous CM 61 95.txt",
	"D Tous CM OM.txt",
	"E Tous Membres EPCI.txt",
	"F Tous CD.txt",
	"G Tous CR.txt",
	"H Tous Deputes.txt",
	"I Tous Senateurs.txt",
	"J Tous RPE.txt",
	"K Tous Maires.txt"
)

# load all the data
for(filename in filenames)
{	cat("Loading file",filename,"\n")
	# read the data
	data <- read.table(
		file=filename, 				# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		skip=1						# ignore the first line of the file ("Titre du rapport")
	)
}

Code du département (Maire)	
Libellé de département (Maires)	
Code Insee de la commune	
Libellé de la commune	
Population de la commune	
Nom de l'élu	
Prénom de l'élu	
Code sexe	
Date de naissance	
Code profession	
Libellé de la profession	
Date de début du mandat	
Date de fin du mandat	
Motif de fin de mandat	
Libellé de fonction	
Date de début de la fonction	
Date de fin de la fonction	
Motif de fin de fonction	
Nuance politique (C. Mun.)	
N° Identification d'un élu
		


