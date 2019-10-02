#############################################################################################
# Merges all the RNE files (historical version from 2018/07/17).
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")




#############################################################################################
# start logging
start.rec.log(text="MERGE")




#############################################################################################
tlog(0,"Loading all the data tables")

# load the departmental councilor table
tlog(2,"Loading departmental data")
tmp <- load.cd.data()
cd.data <- tmp$data
cd.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(cd.data),collapse="x"))

# load the municipal councilor tables
tlog(2,"Loading municipal data")
tmp <- load.cm.data()
cm.data <- tmp$data
cm.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(cm.data),collapse="x"))

# load the regional councilor table
tlog(2,"Loading regional data")
tmp <- load.cr.data()
cr.data <- tmp$data
cr.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(cr.data),collapse="x"))

# load the parliamentary table
tlog(2,"Loading parliamentary data")
tmp <- load.d.data()
d.data <- tmp$data
d.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(d.data),collapse="x"))

# load the European parliamentary table
tlog(2,"Loading European parliamentary data")
tmp <- load.de.data()
de.data <- tmp$data
de.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(de.data),collapse="x"))

# load the EPCI councilor table
tlog(2,"Loading EPCI data")
tmp <- load.epci.data()
epci.data <- tmp$data
epci.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(epci.data),collapse="x"))

# load the mayor table
tlog(2,"Loading mayoral data")
tmp <- load.m.data()
m.data <- tmp$data
m.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(m.data),collapse="x"))

# load the senator table
tlog(2,"Loading senatorial data")
tmp <- load.s.data()
s.data <- tmp$data
s.cols <- tmp$cols
tlog(4,"Dimensions of the table: ",paste(dim(s.data),collapse="x"))




#############################################################################################
# merge the appropriate columns
tlog(0,"Start merging the partial tables")
tlog(2,"Init main table")
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
tlog(2,"Merge departmental data")
tmp <- data.frame(
		matrix(NA, nrow(cd.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(cd.cols, get, x="name"))
tmp[,col.inter] <- cd.data[,col.inter]
tlog(2,"  Remaining columns: ",paste(setdiff(sapply(cd.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Nuance politique"] <- cd.data[,"Nuance politique (C. Gén.)"]
data <- rbind(data, tmp)

# add municipal data
tlog(2,"Merge municipal data")
tmp <- data.frame(
		matrix(NA, nrow(cm.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(cm.cols, get, x="name"))
tmp[,col.inter] <- cm.data[,col.inter]
tlog(2,"  Remaining columns: ",paste(setdiff(sapply(cm.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Libellé du département"] <- cm.data[,"Libellé de département (Maires)"]
tmp[,"Code du département"] <- cm.data[,"Code du département (Maire)"]
tmp[,"Nuance politique"] <- cm.data[,"Nuance politique (C. Mun.)"]
data <- rbind(data, tmp)

# add regional data
tlog(2,"Merge regional data")
tmp <- data.frame(
		matrix(NA, nrow(cr.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(cr.cols, get, x="name"))
tmp[,col.inter] <- cr.data[,col.inter]
tlog(2,"  Remaining columns: ",paste(setdiff(sapply(cr.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Libellé du département"] <- cr.data[,"Libellé de département"]
tmp[,"Nuance politique"] <- cr.data[,"Nuance mandat"]
data <- rbind(data, tmp)

# add parliamentary data
tlog(2,"Merge parliamentary data")
tmp <- data.frame(
		matrix(NA, nrow(d.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(d.cols, get, x="name"))
tmp[,col.inter] <- d.data[,col.inter]
tlog(3,"  Remaining columns: ",paste(setdiff(sapply(d.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Nuance politique"] <- d.data[,"Nuance politique (Député)"]
data <- rbind(data, tmp)

# add European parliamentary data
tlog(2,"Merge parliamentary data")
tmp <- data.frame(
		matrix(NA, nrow(de.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(de.cols, get, x="name"))
tmp[,col.inter] <- de.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(sapply(de.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Nuance politique"] <- de.data[,"Nuance politique (Rep. P.E.)"]
data <- rbind(data, tmp)

# add EPCI data
tlog(2,"Merge EPCI data")
tmp <- data.frame(
		matrix(NA, nrow(epci.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(epci.cols, get, x="name"))
tmp[,col.inter] <- epci.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(sapply(epci.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Nuance politique"] <- epci.data[,"Nuance mandat"]
data <- rbind(data, tmp)

# add mayoral data
tlog(2,"Merge mayoral data")
tmp <- data.frame(
		matrix(NA, nrow(m.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(m.cols, get, x="name"))
tmp[,col.inter] <- m.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(sapply(m.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Code du département"] <- m.data[,"Code du département (Maire)"]
tmp[,"Libellé du département"] <- m.data[,"Libellé de département (Maires)"]
tmp[,"Nuance politique"] <- m.data[,"Nuance politique (C. Mun.)"]
data <- rbind(data, tmp)

# add senatorial data
tlog(2,"Merge senatorial data")
tmp <- data.frame(
		matrix(NA, nrow(s.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, sapply(s.cols, get, x="name"))
tmp[,col.inter] <- s.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(sapply(s.cols, get, x="name"), col.inter), collapse=", "))
tmp[,"Nuance politique"] <- s.data[,"Nuance politique (Sénateur)"]
data <- rbind(data, tmp)

tlog(0,"Merge over")
tlog(2,"Expected dimensions of the full table: ",dim(cd.data)[1]+dim(cm.data)[1]+dim(cr.data)[1]+dim(d.data)[1]+dim(de.data)[1]+dim(epci.data)[1]+dim(m.data)[1]+dim(s.data)[1],"x",length(cols))
tlog(2,"Actual dimensions of the full table: ",paste(dim(data),collapse="x"))




#############################################################################################
# record everything in a new single table
table.file <- file.path(FOLDER_OUT, "all_data.txt")
tlog(0,"Recording the full table in file \"",table.file,"\"")
write.table(x=data,
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	se="\t",				# use tabulations as separators
	row.names=FALSE,		# no names for rows
	col.names=TRUE,			# record table headers
	fileEncoding="UTF8"		# character encoding
)
tlog(0,"Recording over")




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
