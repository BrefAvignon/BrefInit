#############################################################################################
# Main script to process the RNE (historical version from 2018/07/17).
# 
# 07/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")




# install stringdist




# filenames to process
filenames <- c(
	"A Tous CM 01 30.txt",		# municipal councilors part #1
	"B Tous CM 31 60.txt",		# municipal councilors part #2
	"C Tous CM 61 95.txt",		# municipal councilors part #3
	"D Tous CM OM.txt",			# municipal councilors part #4
	"E Tous Membres EPCI.txt",	# public establishment councilors
	"F Tous CD.txt",			# departmental councilors
	"G Tous CR.txt",			# regional councilors
	"H Tous Deputes.txt",		# members of the (national) parliament
	"I Tous Senateurs.txt",		# senators
	"J Tous RPE.txt",			# members of the European parliament
	"K Tous Maires.txt"			# mayors
)




# load all the tables
for(filename in filenames)
{	fn <- file.path(FOLDER_IN,filename)
	cat("Loading file",filename,"\n")
	# read the data
	data <- read.table(
		file=fn, 					# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		skip=1						# ignore the first line of the file ("Titre du rapport")
	)
}


# TODO
# - redondance entre maire dans M et dans CM ? (à tester)
# - problème d'année dans certaines dates (apparait dans valeurs uniques, ex.201 probablement au lieu de de 2001)
# - Développer script pour les tests de cohérence
# - Script de fusion des tables
