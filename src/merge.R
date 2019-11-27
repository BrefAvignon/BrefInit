#############################################################################################
# Merges all the RNE files (historical version from 2018/07/17).
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")
source("src/verification/test_duplicates.R")




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
	COL_ATT_CIRCE_CODE,
	COL_ATT_CIRCE_NOM,
	COL_ATT_REG_CODE,
	COL_ATT_REG_NOM,
	COL_ATT_DPT_CODE,
	COL_ATT_DPT_NOM,
	COL_ATT_CIRC_CODE,
	COL_ATT_CIRC_NOM,
	COL_ATT_CANT_CODE,
	COL_ATT_CANT_NOM,
	COL_ATT_COM_CODE,
	COL_ATT_COM_NOM,
	COL_ATT_COM_POP,
	COL_ATT_EPCI_SIREN,
	COL_ATT_EPCI_NOM,
	COL_ATT_ELU_NOM,
	COL_ATT_ELU_PRENOM,
	COL_ATT_ELU_DDN,
	COL_ATT_ELU_SEXE,
	COL_ATT_PRO_CODE,
	COL_ATT_PRO_NOM,
	COL_ATT_MDT_NOM,
	COL_ATT_MDT_DBT,
	COL_ATT_MDT_FIN,
	COL_ATT_MDT_MOTIF,
	COL_ATT_FCT_NOM,
	COL_ATT_FCT_DBT,
	COL_ATT_FCT_FIN,
	COL_ATT_FCT_MOTIF,
	COL_ATT_ELU_NUANCE,
	COL_ATT_ELU_ID
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
tmp[,COL_ATT_ELU_NUANCE] <- cd.data[,COL_ATT_ELU_NUANCE_CD]
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
tmp[,COL_ATT_DPT_NOM] <- cm.data[,COL_ATT_DPT_NOM_M]
tmp[,COL_ATT_DPT_CODE] <- cm.data[,COL_ATT_DPT_CODE_M]
tmp[,COL_ATT_ELU_NUANCE] <- cm.data[,COL_ATT_ELU_NUANCE_CM]
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
tmp[,COL_ATT_DPT_NOM] <- cr.data[,COL_ATT_DPT_NOM_CR]
tmp[,COL_ATT_ELU_NUANCE] <- cr.data[,COL_ATT_ELU_NUANCE_CR]
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
tmp[,COL_ATT_ELU_NUANCE] <- d.data[,COL_ATT_ELU_NUANCE_D]
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
tmp[,COL_ATT_ELU_NUANCE] <- de.data[,COL_ATT_ELU_NUANCE_DE]
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
tmp[,COL_ATT_ELU_NUANCE] <- epci.data[,COL_ATT_ELU_NUANCE_CR]
tmp[,COL_ATT_DPT_CODE] <- epci.data[,COL_ATT_DPT_CODE_COM]
tmp[,COL_ATT_COM_NOM] <- epci.data[,COL_ATT_COM_NOM_EPCI]
tmp[,COL_ATT_COM_CODE] <- epci.data[,COL_ATT_COM_CODE_EPCI]
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
tmp[,COL_ATT_DPT_CODE] <- m.data[,COL_ATT_DPT_CODE_M]
tmp[,COL_ATT_DPT_NOM] <- m.data[,COL_ATT_DPT_NOM_M]
tmp[,COL_ATT_ELU_NUANCE] <- m.data[,COL_ATT_ELU_NUANCE_CM]
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
tmp[,COL_ATT_ELU_NUANCE] <- s.data[,COL_ATT_ELU_NUANCE_S]
data <- rbind(data, tmp)

tlog(0,"Merge over")
tlog(2,"Expected dimensions of the full table: ",dim(cd.data)[1]+dim(cm.data)[1]+dim(cr.data)[1]+dim(d.data)[1]+dim(de.data)[1]+dim(epci.data)[1]+dim(m.data)[1]+dim(s.data)[1],"x",length(cols))
tlog(2,"Actual dimensions of the full table: ",paste(dim(data),collapse="x"))




#############################################################################################
# record everything in a new single table
table.file <- file.path(FOLDER_OUT_ALL, "merged_data.txt")
tlog(0,"Recording the full table in file \"",table.file,"\"")
write.table(x=data,
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	se="\t",				# use tabulations as separators
#	fileEncoding="UTF-8",	# character encoding
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)
tlog(0,"Recording over")




#############################################################################################
# check for ID duplicates (different persons with the same id)
test.duplicates(data, FOLDER_OUT_ALL)




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
