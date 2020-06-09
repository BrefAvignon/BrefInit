#############################################################################################
# Merges all the RNE files (historical version from 2018/07/17).
# 
# 10/2019 Vincent Labatut
#
# source("src/merge/merge.R")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")
source("src/merge/merge_municipal.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_locations.R")
source("src/verification/test_persoinf.R")
source("src/verification/test_positions.R")
source("src/verification/test_duplicates.R")




#############################################################################################
# set up parameters
extraction <- 1 		# 1 or 2
correct.data <- TRUE	# load raw or corrected data
complete.data <- TRUE	# whether to use secondary source (except EPCI)

# start logging
start.rec.log(text=paste0("MERGE",extraction))
tlog(0,"Merging all tables to get a unique one containing all data")

# init stats table
dir.create(path=FOLDER_OUT_ALL, showWarnings=FALSE, recursive=TRUE)
init.stat.table(FOLDER_OUT_ALL)




#############################################################################################
tlog(0,"Loading all the data tables")

# load the departmental councilor table
tlog(2,"Loading departmental data")
cd.data <- if(extraction==1) load.cd.data(correct.data, complete.data) else load.cd2.data(correct.data, complete.data=FALSE) 
tlog(4,"Dimensions of the table: ",paste(dim(cd.data),collapse="x"))

# load the municipal councilor tables
tlog(2,"Loading municipal data")
cm.data <- if(extraction==1) load.cm.data(correct.data, complete.data) else load.cm2.data(correct.data, complete.data=FALSE)
tlog(4,"Dimensions of the table: ",paste(dim(cm.data),collapse="x"))

# load the regional councilor table
tlog(2,"Loading regional data")
cr.data <- if(extraction==1) load.cr.data(correct.data, complete.data) else load.cr2.data(correct.data, complete.data=FALSE)
tlog(4,"Dimensions of the table: ",paste(dim(cr.data),collapse="x"))

# load the parliamentary table
tlog(2,"Loading parliamentary data")
d.data <- load.d.data(correct.data, complete.data)
tlog(4,"Dimensions of the table: ",paste(dim(d.data),collapse="x"))

# load the European parliamentary table
tlog(2,"Loading European parliamentary data")
de.data <- load.de.data(correct.data, complete.data)
tlog(4,"Dimensions of the table: ",paste(dim(de.data),collapse="x"))

# load the EPCI councilor table
tlog(2,"Loading EPCI data")
epci.data <- load.epci.data(correct.data, complete.data=FALSE)
tlog(4,"Dimensions of the table: ",paste(dim(epci.data),collapse="x"))

# load the mayor table
tlog(2,"Loading mayoral data")
m.data <- load.m.data(correct.data, complete.data)
tlog(4,"Dimensions of the table: ",paste(dim(m.data),collapse="x"))

# load the senator table
tlog(2,"Loading senatorial data")
s.data <- load.s.data(correct.data, complete.data)
tlog(4,"Dimensions of the table: ",paste(dim(s.data),collapse="x"))

# load the presidential table
if(complete.data)
{	tlog(2,"Loading presidential data")
	prf.data <- manual.integrate.data.prf()
	tlog(4,"Dimensions of the table: ",paste(dim(prf.data),collapse="x"))
}




#############################################################################################
# match tests
tlog(0,"Compare ids in municipal and EPCI tables")
cm.ids <- unique(cm.data[,COL_ATT_ELU_ID_RNE])
epci.ids <- unique(epci.data[,COL_ATT_ELU_ID_RNE])
m.ids <- unique(m.data[,COL_ATT_ELU_ID_RNE])
non.matching <- length(which(is.na(match(epci.ids, cm.ids))))
tlog(2,"EPCI ids not matching CM ids: ",non.matching,"/",length(epci.ids), "(",non.matching/length(epci.ids)*100,"%)")
#print(epci.ids[which(is.na(match(epci.ids, cm.ids)))])
non.matching <- length(which(is.na(match(m.ids, cm.ids))))
tlog(2,"M ids not matching CM ids: ",non.matching,"/",length(m.ids), "(",non.matching/length(m.ids)*100,"%)")




#############################################################################################
# merge M in CM
tlog(0,"Merging M and CM")																	# 1,224,194 vs. 114,193
cm.data <- merge.municipal(m.data, cm.data)
	write.cached.table(data=cm.data, cache.file=paste0(FILE_CACHE_CM,"_01.txt"))
tlog(2,"Size of CM after merging with M: ",nrow(cm.data)," rows")							# 1,224,198
cm.data <- merge.overlapping.mandates(data=cm.data, type="CM", strict=FALSE, log=TRUE)
	# 221,264 1,002,906
	write.cached.table(data=cm.data, cache.file=paste0(FILE_CACHE_CM,"_02.txt"))
	tlog(2,"Size of CM after merging overlapping mandates: ",nrow(cm.data)," rows")			# 1,002,829
cm.data <- split.long.mandates(data=cm.data, type="CM", election.file=FILE_VERIF_DATES_CM)
	write.cached.table(data=cm.data, cache.file=paste0(FILE_CACHE_CM,"_03.txt"))
	tlog(2,"Size of CM after splitting long mandates: ",nrow(cm.data)," rows")
cm.data <- shorten.overlapping.functions(cm.data, type="CM", tolerance=8)
	write.cached.table(data=cm.data, cache.file=paste0(FILE_CACHE_CM,"_04.txt"))
	tlog(2,"Size of CM after shortening overlapping functions: ",nrow(cm.data)," rows")
cm.data <- remove.micro.mdtfcts(cm.data, tolerance=7)
	write.cached.table(data=cm.data, cache.file=paste0(FILE_CACHE_CM,"_05.txt"))
	tlog(2,"Size of CM after removing micro-mandates: ",nrow(cm.data)," rows")
tlog(2,"Size of CM after post-processing: ",nrow(cm.data)," rows")




#############################################################################################
# merge the appropriate columns
tlog(0,"Start merging the partial tables")
tlog(2,"Init main table")
cols <- COLS_ATT_NORMALIZED
		
# create empty data frame
data <- data.frame(
	matrix(vector(), 0, length(cols), dimnames=list(c(), cols)),
	check.names=FALSE,
	stringsAsFactors=FALSE
)
data[,COL_ATT_ELU_DDD] <- as.Date(data[,COL_ATT_ELU_DDD])
class(data[,COL_ATT_ELU_DDD])

# add departmental data
tlog(2,"Merge departmental data")
tmp <- data.frame(
		matrix(NA, nrow(cd.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
col.inter <- intersect(cols, colnames(cd.data))
tmp[,col.inter] <- cd.data[,col.inter]	# TODO do that for all columns? define a function for that?
tlog(2,"  Remaining columns: ",paste(setdiff(colnames(cd.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add municipal data
tlog(2,"Merge municipal data")
tmp <- data.frame(
		matrix(NA, nrow(cm.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
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
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
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
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
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
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
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
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
col.inter <- intersect(cols, colnames(epci.data))
tmp[,col.inter] <- epci.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(colnames(epci.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add senatorial data
tlog(2,"Merge senatorial data")
tmp <- data.frame(
		matrix(NA, nrow(s.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
tmp[,COL_ATT_ELU_DDD] <- as.Date(tmp[,COL_ATT_ELU_DDD])
col.inter <- intersect(cols, colnames(s.data))
tmp[,col.inter] <- s.data[,col.inter]
tlog(4,"  Remaining columns: ",paste(setdiff(colnames(s.data), col.inter), collapse=", "))
data <- rbind(data, tmp)

# add presidential data
if(complete.data)
{	tlog(2,"Merge presidential data")
	tmp <- data.frame(
		matrix(NA, nrow(prf.data), length(cols), dimnames=list(c(), cols)),
		check.names=FALSE,
		stringsAsFactors=FALSE
	)
	col.inter <- intersect(cols, colnames(prf.data))
	tmp[,col.inter] <- prf.data[,col.inter]
	tlog(4,"  Remaining columns: ",paste(setdiff(colnames(prf.data), col.inter), collapse=", "))
	data <- rbind(data, tmp)
}

# merge over
tlog(0,"Merge over")
tlog(2,"Expected dimensions of the full table: ",dim(cd.data)[1]+dim(cm.data)[1]+dim(cr.data)[1]+dim(d.data)[1]+dim(de.data)[1]+dim(epci.data)[1]+dim(m.data)[1]+dim(s.data)[1],"x",length(cols))
tlog(2,"Actual dimensions of the full table: ",paste(dim(data),collapse="x"))




#############################################################################################
# merge rows considered as compatible
data <- merge.similar.rows(data)
# add missing ids
data <- complete.missing.ids(data)
# add missing personal information
data <- complete.missing.persinf(data)




#############################################################################################
# count the number of modified rows
idx <- which(data[,COL_ATT_CORREC_DATE] | data[,COL_ATT_CORREC_INFO])
tlog(0,"Total number of modified rows: ", length(idx))




#############################################################################################
# cache merged table
dir.create(path=FOLDER_OUT_ALL, showWarnings=FALSE, recursive=TRUE)
tlog(0,"Ordering the full table")
idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
		data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], data[,COL_ATT_FCT_DBT], data[,COL_ATT_FCT_FIN])
write.cached.table(data[idx,], cache.file=FILE_CACHE_ALL)




#############################################################################################
# export for EV
file.name <- paste0(FILE_CACHE_ALL,"_EV.csv")
tlog(0,"Exporting to ",file.name)
data.bis <- data
for(c in 1:ncol(data.bis))
{	if(class(data.bis[,c])=="character")
		repl <- ""
	else
	{	repl <- "NULL"
		data.bis[,c] <- as.character(data.bis[,c])
	}
	data.bis[is.na(data.bis[,c]),c] <- repl
}
write.table(x=data.bis,			# data to record
	file=file.name,				# name of file containing the new table
	quote=FALSE,				# put double quotes around strings
	sep=";",					# use semicolons as separators
#	fileEncoding="UTF-8",		# character encoding
	row.names=FALSE,			# no names for rows
	col.names=TRUE				# record table headers
)




#############################################################################################
# close the stat file
finalize.stat.table()

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

