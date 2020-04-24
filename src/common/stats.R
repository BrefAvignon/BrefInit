#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Computes the statistical mode(s) of a discrete variable.
# Code taken from https://stackoverflow.com/a/8189441/1254730
#
# vals: values of the variable.
#
# returns: a vector containing the mode(s) of the variable.
#############################################################################################
stat.mode <- function(vals)
{	uq <- unique(vals)
	tab <- tabulate(match(vals, uq))
	result <- uq[tab == max(tab)]
	result <- sort(result)
	return(result)
}




#############################################################################################
# Global variables used to store runtime-related stats.
#############################################################################################
# structure used to store runtime stats and other values
STATS_TABLE <- NA
# file used to record these stats
STATS_FILE <- NA
# time recorded at the start of the current step
STATS_TIME <- NA




#############################################################################################
# Initializes the table used to store runtime related stats.
#
# out.folder: folder used to record the stat file.
#############################################################################################
init.stat.table <- function(out.folder)
{	# init the table
	STATS_TABLE <<- data.frame(
		"Step number"=character(), 
		"Step name"=character(), 
		"Step duration"=character(), 
		"Number of deleted rows"=integer(), 
		"Proportion of deleted rows"=numeric(), 
		"Number of modified rows"=integer(), 
		"Proportion of modified rows"=numeric(), 
		"Number of inserted rows"=integer(), 
		"Proportion of inserted rows"=numeric(), 
		"Total number of changes"=integer(), 
		"Total proportion of changes"=numeric(), 
		stringsAsFactors=FALSE) 

	# init the file name
	STATS_FILE <<- file.path(out.folder, "_runtime_stats.txt")
	
	# init the time
	STATS_TIME <<- Sys.time()
}




#############################################################################################
# Updates the table used to store runtime related stats, by adding a new row corresponding
# to the specified processing step.
#
# s.nbr: step number.
# s.name: step name.
# del.nbr: number of deleted rows.
# mod.nbr: number of modified rows.
# add.nbr: number of inserted rows.
# size: total number of rows in the table before the step.
#############################################################################################
update.stat.table <- function(s.nbr, s.name, del.nbr=0, mod.nbr=0, add.nbr=0, size)
{	# get the duration
	end.time <- Sys.time()
	duration <- difftime(end.time, STATS_TIME, units="secs")
	
	# update the table
	STATS_TABLE <<- rbind(STATS_TABLE, data.frame(
		"Step number"=s.nbr, 
		"Step name"=s.name, 
		"Step duration"=format.duration(duration), 
		"Number of deleted rows"=del.nbr, 
		"Proportion of deleted rows"=del.nbr/size*100, 
		"Number of modified rows"=mod.nbr, 
		"Proportion of modified rows"=mod.nbr/size*100, 
		"Number of inserted rows"=add.nbr, 
		"Proportion of inserted rows"=add.nbr/size*100, 
		"Total number of changes"=(del.nbr+mod.nbr+add.nbr), 
		"Total proportion of changes"=(del.nbr+mod.nbr+add.nbr)/size*100, 
		stringsAsFactors=FALSE 
	))

	# record it
	write.table(STATS_TABLE, 
		file=STATS_FILE,
		quote=FALSE,
		sep="\t",
		row.names=FALSE,
		col.names=TRUE
	)
	
	# reset time for next step
	STATS_TIME <<- Sys.time()
}
