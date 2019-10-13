#############################################################################################
# Compares two text files containing two tables, line by line.
# 
# 10/2019 Vincent Labatut
#############################################################################################
source("src/common/include.R")




#############################################################################################
# start logging
start.rec.log(text="TableComp")




#############################################################################################
# set the files to be compared
tmp.folder <- file.path(FOLDER_TABLES,"temp")
file0 <- file.path(tmp.folder,"F0 Tous CD.txt")
file1 <- file.path(FOLDER_TABLES,FILES_TAB_CD2)
tlog(0,"Comparing table \"",file0,"\" and table \"",file1,"\"")




#############################################################################################
# read everything as strings to avoid any conversion-related issue
tlog(0,"Reading v0 (",file0,")")
t0 <- read.table(
		file=file0, 				# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		skip=1,						# ignore the first line of the file ("Titre du rapport")
		as.is=TRUE,					# don't convert strings to factors
		fileEncoding="Latin1",		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses="character"		# forces to consider everything column as strings
)

tlog(0,"Reading v1 (",file1,")")
t1 <- read.table(
		file=file1, 				# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		skip=1,						# ignore the first line of the file ("Titre du rapport")
		as.is=TRUE,					# don't convert strings to factors
		fileEncoding="Latin1",		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses="character"		# forces to consider everything column as strings
)
# clean empty strings
for(c in 1:ncol(t0))
{	t0[which(t0[,c]==""),c] <- NA
	t1[which(t1[,c]==""),c] <- NA
}
# display row counts
tlog(2,"Numbers of rows: ",nrow(t0)," vs ",nrow(t1)," (delta: ",(nrow(t1)-nrow(t0)),")")




#############################################################################################
# functions used to compare the year of dates represented as strings
extract.year <- function(str) substr(str, nchar(str)-3, nchar(str))
extract.month <- function(str) sapply(strsplit(str, "/"), function(vect) vect[2])
extract.day <- function(str) sapply(strsplit(str, "/"), function(vect) vect[1])
# function used to compare rows from the two different tables
lookup.row <- function(tab1, i, tab2, map)
{	idx <- which((tab2[,COL_ATT_ELU_ID]==tab1[i,COL_ATT_ELU_ID] 									# same person id
					| tab2[,COL_ATT_ELU_PRENOM]==tab1[i,COL_ATT_ELU_PRENOM] 						# or same name
						& tab2[,COL_ATT_ELU_NOM]==tab1[i,COL_ATT_ELU_NOM])							# and first name
				& extract.year(tab2[,COL_ATT_MDT_DBT])==extract.year(tab1[i,COL_ATT_MDT_DBT]))		# same mandate start year
	if(hasArg(map))
		idx <- setdiff(idx,map)
	# if several matches, consider the month
	if(length(idx)>1)
	{	idx2 <- which(extract.month(tab2[idx,COL_ATT_MDT_DBT])==extract.month(tab1[i,COL_ATT_MDT_DBT]))	# having a look at the month
		idx <- idx[idx2]
		# if several matches, consider the day
		if(length(idx)>1)
		{	idx2 <- which(extract.day(tab2[idx,COL_ATT_MDT_DBT])==extract.day(tab1[i,COL_ATT_MDT_DBT]))	# having a look at the day
			idx <- idx[idx2]
			# if several matches, consider the end date
			if(length(idx)>1)
			{	# if the date is NA
				if(is.na(tab1[i,COL_ATT_MDT_FIN]))
				{	idx2 <- which(is.na(tab2[idx,COL_ATT_MDT_FIN]))
					idx <- idx[idx2]
				}
				# otherwise, consider the end year
				else
				{	idx2 <- which(extract.month(tab2[idx,COL_ATT_MDT_FIN])==extract.month(tab1[i,COL_ATT_MDT_FIN]))	# having a look at the year
					idx <- idx[idx2]
					# if several matches, consider the end month
					if(length(idx)>1)
					{	idx2 <- which(extract.month(tab2[idx,COL_ATT_MDT_FIN])==extract.month(tab1[i,COL_ATT_MDT_FIN]))	# having a look at the month
						idx <- idx[idx2]
						# if several matches, consider the day
						if(length(idx)>1)
						{	idx2 <- which(extract.day(tab2[idx,COL_ATT_MDT_FIN])==extract.day(tab1[i,COL_ATT_MDT_FIN]))	# having a look at the day
							idx <- idx[idx2]
						}
					}
				}
				
				# if several matches, be more strict on the id
				if(length(idx)>1)
				{	idx2 <- which(tab2[idx,COL_ATT_ELU_ID]==tab1[i,COL_ATT_ELU_ID])
					idx <- idx[idx2]
					# if several matches, check the motivation to end the mandate
					if(length(idx)>1)
					{	idx2 <- which(tab2[idx,COL_ATT_MDT_MOTIF]==tab1[i,COL_ATT_MDT_MOTIF])
						idx <- idx[idx2]
						
						# otherwise
						if(length(idx)>1)
						{	tlog(4,"Found several matches: comparison too loose")
							print(idx)
							print(rbind(tab1[i,],tab2[idx,]))
							stop("Several rows match: the criteria should be revised")
						}
					}
				}
			}
		}
	}
	
	return(idx)
}




#############################################################################################
# compare both
t2k <- t1[FALSE,]	# kepts entries
t2d <- t1[FALSE,]	# deleted entries
t2a <- t1[FALSE,]	# added entries
mp0 <- rep(NA,nrow(t0))
mp1 <- rep(NA,nrow(t1))
# scan table 0
tlog(0,"Parsing the rows of table 0")
for(i0 in 1:nrow(t0))
{	tlog(2,"Processing row ",i0,"/",nrow(t0))
	# cols <- c(COL_ATT_ELU_ID,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_MDT_DBT,COL_ATT_MDT_FIN)
	# print(rbind(t0[c(5232),cols],t1[c(5244,5245),cols]))
	
	# check whether rows are similar
	idx <- lookup.row(tab1=t0,i=i0,tab2=t1,map=mp0)
	if(length(idx)==1)
	{	tlog(4,"Found in t1: add to map for latter use")
		mp0[i0] <- idx
	}
	else if(length(idx)>1)
	{	tlog(4,"Found several times in t1 but could not match the month: check manually (",paste(idx,collapse=","),")")
		stop("Several rows match in t1: the criteria should be revised")			
	}
	else
	{	tlog(4,"Not found in t1: deleted row")
		t2d <- rbind(t2d,t0[i0,])
	}
}
# scan table 1
tlog(0,"Parsing the rows of table 1")
for(i1 in 1:nrow(t1))
{	tlog(2,"Processing row ",i1,"/",nrow(t1))
	
	# check whether rows are similar
	if(i1 %in% mp0)
	{	tlog(4,"Found in t0: kept row")
		t2k <- rbind(t2k,t1[i1,])
		mp1[i1] <- which(mp0==i1)
	}
	else
	{	tlog(4,"Not found in t0: added row")
		t2a <- rbind(t2a,t1[i1,])
	}
}
tlog(0,"Tables processed")
# record all resulting tables
tlog(2,"Similar rows: ",nrow(t2k))
tab <- cbind(which(!is.na(mp0)),mp0[!is.na(mp0)],t2k)
colnames(tab)[1:2] <- c("Ligne vx", "Ligne nv")
table.file <- file.path(tmp.folder, "similar_rows.txt")
write.table(x=tab,
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	se="\t",				# use tabulations as separators
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)
tlog(2,"Added rows: ",nrow(t2a))
tab <- cbind(setdiff(1:nrow(t1),mp0[!is.na(mp0)]),t2a)
colnames(tab)[1] <- c("Ligne nv")
table.file <- file.path(tmp.folder, "added_rows.txt")
write.table(x=tab,
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	se="\t",				# use tabulations as separators
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)
tlog(2,"Removed rows: ",nrow(t2d))
tab <- cbind(which(is.na(mp0)),t2d)
colnames(tab)[1] <- c("Ligne vx")
table.file <- file.path(tmp.folder, "removed_rows.txt")
write.table(x=tab,
	file=table.file,		# name of file containing the new table
	quote=TRUE,				# put double quotes around strings
	se="\t",				# use tabulations as separators
	row.names=FALSE,		# no names for rows
	col.names=TRUE			# record table headers
)




#############################################################################################
# compare each field
tmp <- 1*(t0==t1)




#############################################################################################
# record results




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
