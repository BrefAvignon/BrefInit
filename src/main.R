#############################################################################################
# Merges all the RNE files (historical 2018/07/17 version).
# 
# 07/2019 Vincent Labatut
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

# load all the tables
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
