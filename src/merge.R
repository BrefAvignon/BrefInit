#############################################################################################
# Merges all the RNE files (historical version from 2018/07/17).
# 
# 10/2019 Vincent Labatut
#
# source("src/merge.R")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_locations.R")
source("src/verification/test_persoinf.R")
source("src/verification/test_positions.R")
source("src/verification/test_duplicates.R")




#############################################################################################
# set up the extraction
extraction <- 1 # 1 or 2
# start logging
start.rec.log(text="MERGE")




#############################################################################################
tlog(0,"Loading all the data tables")
correct.data <- TRUE

# load the departmental councilor table
tlog(2,"Loading departmental data")
cd.data <- load.cd.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(cd.data),collapse="x"))

# load the municipal councilor tables
tlog(2,"Loading municipal data")
cm.data <- load.cm.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(cm.data),collapse="x"))

# load the regional councilor table
tlog(2,"Loading regional data")
cr.data <- load.cr.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(cr.data),collapse="x"))

# load the parliamentary table
tlog(2,"Loading parliamentary data")
d.data <- load.d.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(d.data),collapse="x"))

# load the European parliamentary table
tlog(2,"Loading European parliamentary data")
de.data <- load.de.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(de.data),collapse="x"))

# load the EPCI councilor table
tlog(2,"Loading EPCI data")
epci.data <- load.epci.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(epci.data),collapse="x"))

# load the mayor table
tlog(2,"Loading mayoral data")
m.data <- load.m.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(m.data),collapse="x"))

# load the senator table
tlog(2,"Loading senatorial data")
s.data <- load.s.data(correct.data)
tlog(4,"Dimensions of the table: ",paste(dim(s.data),collapse="x"))




#############################################################################################
# merge the appropriate columns
tlog(0,"Start merging the partial tables")
tlog(2,"Init main table")
#cols <- c(
#	COL_ATT_CIRCE_CODE,
#	COL_ATT_CIRCE_NOM,
#	COL_ATT_REG_CODE,
#	COL_ATT_REG_NOM,
#	COL_ATT_DPT_ID,
#	COL_ATT_DPT_CODE,
#	COL_ATT_DPT_NOM,
#	COL_ATT_CIRC_CODE,
#	COL_ATT_CIRC_NOM,
#	COL_ATT_CANT_ID,
#	COL_ATT_CANT_CODE,
#	COL_ATT_CANT_NOM,
#	COL_ATT_COM_CODE,
#	COL_ATT_COM_NOM,
#	COL_ATT_COM_POP,
#	COL_ATT_EPCI_SIREN,
#	COL_ATT_EPCI_NOM,
#	COL_ATT_EPCI_DPT,
#	COL_ATT_ELU_ID,
#	COL_ATT_ELU_ID_RNE,
#	COL_ATT_ELU_ID_SENAT,
#	COL_ATT_ELU_NOM,
#	COL_ATT_ELU_PRENOM,
#	COL_ATT_ELU_NAIS_DATE,
#	COL_ATT_ELU_DDD,
#	COL_ATT_ELU_SEXE,
#	COL_ATT_ELU_NAT,
#	COL_ATT_ELU_NUANCE,
#	COL_ATT_PRO_CODE,
#	COL_ATT_PRO_NOM,
#	COL_ATT_MDT_NOM,
#	COL_ATT_MDT_DBT,
#	COL_ATT_MDT_FIN,
#	COL_ATT_MDT_MOTIF,
#	COL_ATT_FCT_NOM,
#	COL_ATT_FCT_DBT,
#	COL_ATT_FCT_FIN,
#	COL_ATT_FCT_MOTIF,
#	COL_ATT_SOURCES
#)
cols <- COLS_ATT_NORMALIZED
		
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
col.inter <- intersect(cols, colnames(cd.data))
tmp[,col.inter] <- cd.data[,col.inter]
tlog(2,"  Remaining columns: ",paste(setdiff(colnames(cd.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add municipal data
tlog(2,"Merge municipal data")
tmp <- data.frame(
		matrix(NA, nrow(cm.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(cm.data))
tmp[,col.inter] <- cm.data[,col.inter]
tlog(2,"  Remaining columns: ",paste(setdiff(colnames(cm.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add regional data
tlog(2,"Merge regional data")
tmp <- data.frame(
		matrix(NA, nrow(cr.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(cr.data))
tmp[,col.inter] <- cr.data[,col.inter]
tlog(2,"  Remaining columns: ",paste(setdiff(colnames(cr.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add parliamentary data
tlog(2,"Merge parliamentary data")
tmp <- data.frame(
		matrix(NA, nrow(d.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(d.data))
tmp[,col.inter] <- d.data[,col.inter]
tlog(3,"  Remaining columns: ",paste(setdiff(colnames(d.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add European parliamentary data
tlog(2,"Merge parliamentary data")
tmp <- data.frame(
		matrix(NA, nrow(de.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(de.data))
tmp[,col.inter] <- de.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(colnames(de.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add EPCI data
tlog(2,"Merge EPCI data")
tmp <- data.frame(
		matrix(NA, nrow(epci.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(epci.data))
tmp[,col.inter] <- epci.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(colnames(epci.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add mayoral data
tlog(2,"Merge mayoral data")
tmp <- data.frame(
		matrix(NA, nrow(m.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(m.data))
tmp[,col.inter] <- m.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(colnames(m.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add senatorial data
tlog(2,"Merge senatorial data")
tmp <- data.frame(
		matrix(NA, nrow(s.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
col.inter <- intersect(cols, colnames(s.data))
tmp[,col.inter] <- s.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(colnames(s.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

tlog(0,"Merge over")
tlog(2,"Expected dimensions of the full table: ",dim(cd.data)[1]+dim(cm.data)[1]+dim(cr.data)[1]+dim(d.data)[1]+dim(de.data)[1]+dim(epci.data)[1]+dim(m.data)[1]+dim(s.data)[1],"x",length(cols))
tlog(2,"Actual dimensions of the full table: ",paste(dim(data),collapse="x"))




#############################################################################################
# record everything in a new single table
dir.create(path=FOLDER_OUT_ALL, showWarnings=FALSE, recursive=TRUE)
table.file <- file.path(FOLDER_OUT_ALL, "merged_data.txt")
tlog(0,"Recording the full table in file \"",table.file,"\"")
write.table(x=data,
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	sep="\t",				# use tabulations as separators
#	fileEncoding="UTF-8",	# character encoding
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)
tlog(0,"Recording over")

# record the table sorted by id then mandate date
table.file <- file.path(FOLDER_OUT_ALL, "merged_data_byperson.txt")
tlog(0,"Sorting full table by person, then recording in file \"",table.file,"\"")
idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
		data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], data[,COL_ATT_FCT_DBT], data[,COL_ATT_FCT_FIN])
write.table(x=data[idx,],		# sorted data
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	sep="\t",				# use tabulations as separators
#	fileEncoding="UTF-8",	# character encoding
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)
tlog(0,"Recording over")



#############################################################################################
out.folder <- FOLDER_OUT_ALL

# look for duplicate rows
test.duplicate.rows(data=data, out.folder=out.folder)

# check personal information
test.personal.info(data=data, out.folder=out.folder)

# check dates
test.col.dates.cm(data=data, out.folder=out.folder)

# check locations
test.col.locations(data=data, out.folder=out.folder, merged=TRUE)

# check overlapping mandates for the same position
###test.position.cm(data=data, out.folder=out.folder)

# check for ID duplicates (different persons with the same id)
test.duplicates(data=data, out.folder=out.folder)




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()

#data <- read.table(
#		file=table.file,			# name of the data file
#		header=TRUE, 				# look for a header
#		sep="\t", 					# character used to separate columns 
#		check.names=FALSE, 			# don't change the column names from the file
#		comment.char="", 			# ignore possible comments in the content
#		row.names=NULL, 			# don't look for row names in the file
##		quote="", 					# don't expect double quotes "..." around text fields
#		as.is=TRUE,					# don't convert strings to factors
#		colClasses="character"		# all column originally read as characters, then converted later if needed
#)
