#############################################################################################
# Functions performing various tests on person duplicates.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Detect the rows that are considered as similar in the table. Generally speaking, two rows 
# are similar if they have the same values for certain columns of interest, and compatible 
# values for the rest (i.e. one cell empty in a row whereas it has a proper value in the other). 
#
# data: the data table.
# out.folder: folder where to output the results.
#############################################################################################
test.duplicate.rows <- function(data, out.folder=NA)
{	comp.cols <- c(COL_ATT_ELU_ID, COL_ATT_ELU_NOM, COL_ATT_ELU_PRENOM, COL_ATT_ELU_SEXE, COL_ATT_ELU_DDN)
	tlog(0,"Looking for compatible rows, using compulsory columns \"",paste(comp.cols,collapse="\",\""),"\"")
	plan(multiprocess, workers=CORE.NBR/2)
	rm.col <- which(colnames(data) %in% comp.cols)
	
	# identify redundant rows
	concat <- apply(data[,comp.cols], 1, function(row) paste(row, collapse=":"))
	tt <- table(concat)
	codes <- names(tt)[which(tt>1)]
	tlog(2,"Looking for redundant rows: found ",length(codes))
	
	# identify compatible rows amongst redundant ones
	tlog(2,"Looking for compatible rows among them")
	mats <- lapply(codes, function(code)
			{	res <- matrix(nrow=0,ncol=ncol(data)+1)
				rs <- which(concat==code)
				for(r1 in 1:(length(rs)-1))
				{	row1 <- data[rs[r1], -rm.col]
					for(r2 in (r1+1):length(rs))
					{	row2 <- data[rs[r2], -rm.col]
						if(all(is.na(row1) | is.na(row2) | row1==row2))
						{	tmp <- rbind(data[rs[r1],], data[rs[r2],])
							tmp <- cbind(rs[c(r1,r2)], tmp)
							res <- rbind(res, tmp)
						}
					}
				}
				if(nrow(res)>0)
					res <- rbind(res, rep(NA,ncol(res)))
				return(res)
			})
	
	# merge the list of tables and record them
	tlog(2,"Merging the ",length(mats)," result tables")
	tab <- cbind(rep(NA,nrow(data)),data)[-(1:nrow(data)),]
	colnames(tab)[1]<- "Ligne"
	for(m in 1:length(mats))
		tab <- rbind(tab, mats[[m]])
	if(!is.na(out.folder) && nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"compatible_rows.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab, file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			se="\t"
		)
	}
	
	tlog(2,"Done searching for compatible rows")
}




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
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
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
	tlog(4,"Done: ",length(conv.map)," lines read")
	
	# load table of confirmed homonyms
	if(extraction==1)
		tab.file <- FILE_HOMON_IDS
	else if(extraction==2)
		tab.file <- FILE_HOMON_IDS2
	tlog(2,"Loading the table of confirmed homonyms (",tab.file,")")
	homon.table <- read.table(
			file=tab.file,				# name of the tab file
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
	tlog(4,"Done: ",nrow(homon.table)," rows read")
	
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
				
				# check whether they are confirmed homonyms
# TODO not tested yet
				if(ids[1] %in% homon.table[,1])
				{	rem <- homon.table[which(homon.table[,1]==ids[1]),2]
					ids <- setdiff(ids, rem)
				}
				
				tlog(8,"After filtering out the manually confirmed true homonyms, ",length(ids)," ids remain")
				if(length(ids)>1)
				{	# TODO: check whether these ids have incompatible mandates
					# >> actually even a single id may have incompatible mandates due to the
					# way the data are structured, so we eventually did not perform this test 
					
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
	}
	tlog(2,"Processing of unique individuals over")
	
	# only if some duplicates were found:
	if(nrow(tab)>0)
	{	# record the table of problematic cases
		tab.file <- file.path(out.folder,"elu_name_problems_multiple_ids.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
		
		# update the table of equivalent ids (but in the output folder)
		tab.file <- file.path(out.folder,"equiv_ids_updated.txt")
		main.ids <- sapply(conv.map, function(v) v[1])
		other.ids <- sapply(conv.map, function(v) paste(v[2:length(v)],collapse=","))
		tab <- cbind(main.ids, other.ids)
		colnames(tab) <- c("Main Id","Other ids")
		idx <- order(as.integer(main.ids))
		write.table(x=tab[idx,], file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE, 
				sep="\t"
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
