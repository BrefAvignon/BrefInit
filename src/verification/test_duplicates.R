#############################################################################################
# Functions performing various tests on person duplicates.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Checks that each ID is always associated to the same person. The person is identified through
# his/her lastname, firstname, birthdate, and sex.
#
# data: table containing the merged data.
# out.folder: folder where to output the results.
#############################################################################################
test.id.reuse <- function(data, out.folder)
{	tlog(0,"Trying to detect ID duplicate")
	tab <- data[FALSE,]
	count <- 0
	
	# identify all unique ID
	tlog(2,"Identifying all unique IDs")
	ids <- sort(unique(data[,COL_ATT_ELU_ID]))
	tlog(4,"Found ",length(ids)," of them")
	
	# process each unique ID
	tlog(2,"Processing each unique ID")
	count <- 0
	for(i in 1:length(ids))
	{	id <- ids[i]
		tlog(4,"Processing ID ",i,"/",length(ids))
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_ELU_ID]==id)
		tlog(6,"Found ",length(idx)," mandate(s)")
		# check if the individual info is the same
		if(length(idx)>1)
		{	differs <- FALSE
			
			# compare last names
			lastnames <- sort(unique(data[idx,COL_ATT_ELU_NOM]))
			if(length(lastnames)>1)
			{	tlog(8,"Found ",length(lastnames)," different last names: ",paste(lastnames,collapse=", "))
				differs <- TRUE
			}
			
			# compare first names 
			firstnames <- sort(unique(data[idx,COL_ATT_ELU_PRENOM]))
			if(length(firstnames)>1)
			{	tlog(8,"Found ",length(firstnames)," different first names: ",paste(firstnames,collapse=", "))
				differs <- TRUE
			}
			
			# compare birth dates
			birthdates <- sort(unique(data[idx,COL_ATT_ELU_DDN]))
			if(length(birthdates)>1)
			{	tlog(8,"Found ",length(birthdates)," different birth dates: ",paste(birthdates,collapse=", "))
				differs <- TRUE
			}
			
			# compare sexes
			sexes <- sort(unique(data[idx,COL_ATT_ELU_SEXE]))
			if(length(sexes)>1)
			{	tlog(8,"Found ",length(sexes)," different sexes: ",paste(sexes,collapse=", "))
				differs <- TRUE
			}
			
			# add to the table of problematic cases
			if(differs)
			{	tab <- rbind(tab, data[idx,], rep(NA,ncol(data)))
				count <- count + 1
			}
		}
	}
	tlog(2,"Processing of unique IDs over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"elu_id_problems_reuse.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,col.names=TRUE)
	}
	tlog(4,"Found a total of ",count," IDs associated with different personal information")
}




#############################################################################################
# Checks that each person only has a single ID. The person is identified through
# his/her lastname, firstname, birthdate, and sex.
#
# data: table containing the merged data.
# loc.col: name of the column containing the location (optional).
# out.folder: folder where to output the results.
#############################################################################################
test.multiple.id <- function(data, loc.col=NA, out.folder)
{	tlog(0,"Trying to detect ID duplicates")
	tab <- data[FALSE,]
	count <- 0
	
	# load table of equivalent ids
	if(extraction==1)
		tab.file <- FILE_EQUIV_IDS
	else if(extraction==2)
		tab.file <- FILE_EQUIV_IDS2
	tlog(2,"Loading the table of equivalent ids (",tab.file,")")
	equiv.table <- read.table(
			file=tab.file,				# name of the equivalence file
			header=TRUE, 				# look for a header
			sep="\t", 					# character used to separate columns 
			check.names=FALSE, 			# don't change the column names from the file
			comment.char="", 			# ignore possible comments in the content
			row.names=NULL, 			# don't look for row names in the file
			quote="", 					# don't expect double quotes "..." around text fields
			as.is=TRUE,					# don't convert strings to factors
			colClasses="character"		# all column originally read as characters
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
	)
	# convert to map
	conv.map <- list()
	if(nrow(equiv.table)>0)
	{	for(r in 1:nrow(equiv.table))
		{	main.id <- equiv.table[r,1]
			other.ids <- strsplit(x=equiv.table[r,2], split=",", fixed=TRUE)[[1]]
			conv.map[[length(conv.map)+1]] <- c(main.id,other.ids)
		}
	}
	tlog(4,"Done: ",length(conv.map)," id(s) read")
	
	# identify all unique individuals
	# under the form of a (lastname, firstname, birthdate, sex, territory) tuple
	tlog(2,"Identifying all unique individuals")
	lastnames <- data[,COL_ATT_ELU_NOM]
	firstnames <- data[,COL_ATT_ELU_PRENOM]
	birthdates <- format(data[,COL_ATT_ELU_DDN])
	sexes <- data[,COL_ATT_ELU_SEXE]
	if(hasArg(loc.col) && !is.na(loc.col))
	{	locations <- data[,loc.col]
		indivs <- apply(cbind(lastnames,firstnames,birthdates,sexes,locations),1,function(r) paste(r,collapse=":"))
	}		
	else
		indivs <- apply(cbind(lastnames,firstnames,birthdates,sexes),1,function(r) paste(r,collapse=":"))
	unique.indivs <- sort(unique(indivs))
	tlog(4,"Found ",length(unique.indivs)," of them")
	
	# process each unique individidual
	tlog(2,"Processing each unique individual")
	count <- 0
	for(i in 1:length(unique.indivs))
	{	indiv <- unique.indivs[i]
		tlog(4,"Processing individual ",i,"/",length(unique.indivs),": ",indiv)
		
		# get the corresponding mandates
		idx <- which(indivs[]==indiv)
		tlog(6,"Found ",length(idx)," mandate(s)")
		# check if the individual ID is always the same
		if(length(idx)>1)
		{	ids <- sort(unique(data[idx,COL_ATT_ELU_ID]))
			
			# if there are several distinct ids
			if(length(ids)>1)
			{	tlog(8,"Found ",length(ids)," different ids: ",paste(ids,collapse=", "))
			
				# add to the table of problematic cases
				tab <- rbind(tab, data[idx,], rep(NA,ncol(data)))
				count <- count + 1
				
				# add to the conversion map
				ids <- as.character(ids)
				if(length(conv.map)==0) 
					ridx <- c()
				else
					ridx <- which(sapply(conv.map, function(v) length(intersect(v,ids))>0))	
				if(length(ridx)>1)
					stop("Problem with the map of equivalent ids: several groups of ids would be equivalent (",paste(ids,collapse=","),")")
				else if(length(ridx)==1)
					conv.map[[ridx]] <- as.character(sort(as.integer(unique(c(conv.map[[ridx]],ids)))))
				else if(length(ridx)==0)
					conv.map[[length(conv.map)+1]] <- as.character(sort(as.integer(unique(ids))))
			}
		}
	}
	tlog(2,"Processing of unique individuals over")
	
	# only if some duplicates were found:
	if(nrow(tab)>0)
	{	# record the table of problematic cases
		tab.file <- file.path(out.folder,"elu_id_problems_multiple_names.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, col.names=TRUE)
		
		# update the table of equivalent ids (but in the output folder)
		tab.file <- file.path(out.folder,"equiv_ids_updated.txt")
		main.ids <- sapply(conv.map, function(v) v[1])
		other.ids <- sapply(conv.map, function(v) paste(v[2:length(v)],collapse=","))
		tab <- cbind(main.ids, other.ids)
		colnames(tab) <- c("Main Id","Other ids")
		idx <- order(as.integer(main.ids))
		write.table(x=tab[idx,], file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, col.names=TRUE,
				quote=FALSE, sep="\t"
		)
	}
	
	tlog(4,"Found a total of ",count," individuals associated to several different IDs")
}




#############################################################################################
# Performs a series of tests on IDs and personal information, in order to detect cases where
# the same ID is associated to different persons, or the same person is associated to several
# different IDs, etc.
#
# data: table containing the data.
# loc.col: name of the column containing the location (optional).
# out.folder: folder where to output the results.
#############################################################################################
test.duplicates <- function(data, loc.col=NA, out.folder)
{	# look for IDs associated to several distinct pieces of personal information
#	test.id.reuse(data, out.folder) 
	
	# look for persons seemingly associated to different IDs
	test.multiple.id(data, loc.col, out.folder)
}
