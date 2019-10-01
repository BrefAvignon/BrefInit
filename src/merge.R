#############################################################################################
# Merges all the RNE files (historical version from 2018/07/17).
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")




#############################################################################################
# load the departmental councilor table
tmp <- load.cd.data()
cd.data <- tmp$data
cd.cols <- tmp$cols

# load the municipal councilor tables
tmp <- load.cm.data()
md.data <- tmp$data
md.cols <- tmp$cols

# load the regional councilor table
tmp <- load.cr.data()
cr.data <- tmp$data
cr.cols <- tmp$cols

# load the members of the parliament table
tmp <- load.d.data()
d.data <- tmp$data
d.cols <- tmp$cols

# load the members of the European parliament table
tmp <- load.de.data()
de.data <- tmp$data
de.cols <- tmp$cols

# load the EPCI councilor table
tmp <- load.epci.data()
epci.data <- tmp$data
epci.cols <- tmp$cols

# load the mayor table
tmp <- load.m.data()
m.data <- tmp$data
m.cols <- tmp$cols

# load the senator table
tmp <- load.s.data()
s.data <- tmp$data
s.cols <- tmp$cols




#############################################################################################
# merge the appropriate columns
cols <- c(
	"CodeCirER",
	"LibelléCirER",
	"Code région",
	"Libellé de la région",
	"Code du département",
	"Libellé du département",
	"Code de la cir.législative",
	"Libellé de la cir.législative",
	"Code du canton",
	"Libellé du canton",
	"Code Insee de la commune",
	"Libellé de la commune",
	"Population de la commune",
	"N° SIREN",
	"Libellé de l'EPCI",
	"Nom de l'élu",
	"Prénom de l'élu",
	"Date de naissance",
	"Code sexe",
	"Code profession",
	"Libellé de la profession",
	"Libellé de mandat",
	"Date de début du mandat",
	"Date de fin du mandat",
	"Motif de fin de mandat",
	"Libellé de fonction",
	"Date de début de la fonction",
	"Date de fin de la fonction",
	"Motif de fin de fonction",
	"Nuance politique",
	"N° Identification d'un élu"
)

# create empty data frame
data <- data.frame(
			matrix(vector(), 0, length(cols), dimnames=list(c(), cols)),
			check.names=FALSE,
			stringsAsFactors=FALSE
	)

# add departmental data
data <- cd.data[,idx]
