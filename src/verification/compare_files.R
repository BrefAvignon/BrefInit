#############################################################################################
# Compares two text files line by line.
# 
# 10/2019 Vincent Labatut
#############################################################################################
source("src/common/include.R")




#############################################################################################
# start logging
start.rec.log(text="TableComp")




#############################################################################################
# set the files to be compared
file0 <- file.path(FOLDER_TABLES,"temp","F0 Tous CD.txt")
file1 <- file.path(FOLDER_TABLES,"temp","2019-10-09 extraction historique CD.txt")
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




#############################################################################################
# put new rows apart: use mandate start and end date
tlog(0,"Identifying new rows in table 1")
t1s <- t1[-1:nrow(t1),]
t1d <- t1[-1:nrow(t1),]
i0 <- 1
i1 <- 1
while(i0 <= ncol(t0))
{	tlog(2,"Processing row ",i0)
	
	# check whether rows are similar
	if(t0[i0,COL_ATT_ELU_ID]==t1[i1,COL_ATT_ELU_ID] 			# same person id
			&& t0[i0,COL_ATT_MDT_DBT]==t1[i1,COL_ATT_MDT_DBT] 	# same mandate start date
			&& (t0[i0,COL_ATT_MDT_FIN]!=t1[i1,COL_ATT_MDT_FIN]	# same mandate end date
				|| is.na(t0[i0,COL_ATT_MDT_DBT])))				# or no mandate end date in t0
	{	tlog(4,"Similar rows: just copying in t1s (same)")
		t1s <- rbind(t1s,t1[i1,])
		i0 <- i0 + 1
		i1 <- i1 + 1
	}
	else
	{	tlog(4,"Different rows: copying in t1d (diff)")
		t1d <- rbind(t1d,t1[i1,])
		i1 <- i1 + 1
	}	
}
tlog(0,"Tables processed")
# copying the remaning of t1 in t1d
if(i1<nrow(t1))
	t1d <- rbind(t1d,t1[i1:nrow(t1),])
tlog(2,"Similar rows: ",nrow(t1s))
tlog(2,"Different (=new) rows: ",nrow(t1d))




#############################################################################################
# compare each field
tmp <- 1*(t0==t1)




#############################################################################################
# record results




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
