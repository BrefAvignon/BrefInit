#############################################################################################
# Merges all the RNE files (historical version from 2018/07/17).
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")




#############################################################################################
cat("Loading all the data tables\n")

# load the departmental councilor table
cat("Loading departmental data\n")
tmp <- load.cd.data()
cd.data <- tmp$data
cd.cols <- tmp$cols

# load the municipal councilor tables
cat("Loading municipal data\n")
tmp <- load.cm.data()
md.data <- tmp$data
md.cols <- tmp$cols

# load the regional councilor table
cat("Loading regional data\n")
tmp <- load.cr.data()
cr.data <- tmp$data
cr.cols <- tmp$cols

# load the parliamentary table
cat("Loading parliamentary data\n")
tmp <- load.d.data()
d.data <- tmp$data
d.cols <- tmp$cols

# load the European parliamentary table
cat("Loading European parliamentary data\n")
tmp <- load.de.data()
de.data <- tmp$data
de.cols <- tmp$cols

# load the EPCI councilor table
cat("Loading EPCI data\n")
tmp <- load.epci.data()
epci.data <- tmp$data
epci.cols <- tmp$cols

# load the mayor table
cat("Loading mayoral data\n")
tmp <- load.m.data()
m.data <- tmp$data
m.cols <- tmp$cols

# load the senator table
cat("Loading senatorial data\n")
tmp <- load.s.data()
s.data <- tmp$data
s.cols <- tmp$cols




#############################################################################################
# merge the appropriate columns
cat("Init main table\n")
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
cat("Merge departmental data\n")
tmp <- data.frame(
		matrix(NA, nrow(cd.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(cd.cols, get, x="names"))
tmp[,col.inter] <- cd.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(cd.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- cd.data[,"Nuance politique (C. Gén.)"]
data <- rbind(data, tmp)

# add municipal data
cat("Merge municipal data\n")
tmp <- data.frame(
		matrix(NA, nrow(cm.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(cm.cols, get, x="names"))
tmp[,col.inter] <- cm.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(cm.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- cm.data[,"Nuance politique (C. Mun.)"]
data <- rbind(data, tmp)

# add regional data
cat("Merge regional data\n")
tmp <- data.frame(
		matrix(NA, nrow(cr.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(cr.cols, get, x="names"))
tmp[,col.inter] <- cr.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(cr.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- cr.data[,"Nuance mandat"]
data <- rbind(data, tmp)

# add parliamentary data
cat("Merge parliamentary data\n")
tmp <- data.frame(
		matrix(NA, nrow(d.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(d.cols, get, x="names"))
tmp[,col.inter] <- d.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(d.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- d.data[,"Nuance politique (Député)"]
data <- rbind(data, tmp)

# add European parliamentary data
cat("Merge parliamentary data\n")
tmp <- data.frame(
		matrix(NA, nrow(de.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(de.cols, get, x="names"))
tmp[,col.inter] <- de.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(de.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- de.data[,"Nuance politique (Rep. P.E.)"]
data <- rbind(data, tmp)

# add EPCI data
cat("Merge EPCI data\n")
tmp <- data.frame(
		matrix(NA, nrow(epci.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(epci.cols, get, x="names"))
tmp[,col.inter] <- epci.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(epci.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- epci.data[,"Nuance mandat"]
data <- rbind(data, tmp)

# add mayoral data
cat("Merge mayoral data\n")
tmp <- data.frame(
		matrix(NA, nrow(m.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(m.cols, get, x="names"))
tmp[,col.inter] <- m.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(m.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- m.data[,"Nuance politique (C. Mun.)"]
data <- rbind(data, tmp)

# add senatorial data
cat("Merge senatorial data\n")
tmp <- data.frame(
		matrix(NA, nrow(s.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(s.cols, get, x="names"))
tmp[,col.inter] <- s.data[,col.inter]
cat("  Remaining columns: ",paste(setdiff(sapply(s.cols, get, x="names"), col.inter), collapse=", "),"\n")
tmp[,"Nuance politique"] <- s.data[,"Nuance politique (Sénateur)"]
data <- rbind(data, tmp)
