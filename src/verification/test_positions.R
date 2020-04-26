#############################################################################################
# Functions performing various tests on elective positions.
# 
# 10/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Checks that two departmental councilors do not hold the exact same position at the same time.
#
# data: table containing the departmental data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.cd <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in departmental positions")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique mandate positions: generated unique department code
	tlog(2,"Identifying all unique mandate positions")	# except specific cantons
	unique.pos <- sort(unique(data[data[,COL_ATT_CANT_NOM]!="CANTON FICTIF",COL_ATT_CANT_ID]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog.start.loop(2,length(unique.pos),"Processing each unique mandate position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		tlog.loop(4,p,"Processing mandate position ",unique.pos[p]," (",dpt,") (",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_CANT_ID]==unique.pos[p])
		tlog(6,"Found ",length(idx)," rows")
		
		folder2 <- file.path(folder,dpt)
		dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
		
		# record the sequence of mandates for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder2,paste0(unique.pos[p],"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#				fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#				quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder2,paste0(unique.pos[p],".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#				fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
			
		if(length(idx)>1)
		{	# check if their dates overlap
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared rows
				start1 <- data[idx[i],COL_ATT_MDT_DBT]
				end1 <- data[idx[i],COL_ATT_MDT_FIN]
				sex1 <- data[idx[i],COL_ATT_ELU_SEXE]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared rows
					start2 <- data[idx[j],COL_ATT_MDT_DBT]
					end2 <- data[idx[j],COL_ATT_MDT_FIN]
					sex2 <- data[idx[j],COL_ATT_ELU_SEXE]
					
					# check if the periods intersect
					if(date.intersect(start1, end1, start2, end2))
					{	# problem only if before 2015 or persons of the same sex
						if(get.year(start1)<2015 || get.year(start2)<2015 || sex1==sex2)
						{	if(get.year(start1)<2015)
								tlog(8,"Date before 2015")
							if(sex1==sex2)
								tlog(8,"Persons of the same sex")
							# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# count the problematic cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
			}
			
			# possibly add an empty row to separate cases
			tlog(6,"Found ",ccount," overlapping mandate posotions for this specific position")
			if(ccount>0)
				tab <- rbind(tab, rep(NA,ncol(data)))
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," overlapping mandate positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}

	###### quite the same thing, but with function positions this time
	
	# ignore the following (non-unique) functions
	ign.functs <- c(
		"VICE PRESIDENT DU CONSEIL DEPARTEMENTAL",			# several of them, but not distinguished
		"VICE PRESIDENT DELEGUE DU CONSEIL DEPARTEMENTAL",	# several of them, but not distinguished
		"QUESTEUR",											# several of them, but not distinguished
		"PRESIDENT DE GROUPE",								# several of them, but not distinguished
		"PRESIDENT DE COMMISSION",							# several of them, but not distinguished
		"AUTRE MEMBRE COMMISSION PERMANENTE",				# several of them, but not distinguished
		"AUTRE MEMBRE"										# many
	)
	
	# identify all unique functions: departement code + function name
	tlog(2,"Identifying all unique function positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	functs <- data[,COL_ATT_FCT_NOM]
	pos <- apply(cbind(dpts,functs),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos[!is.na(functs)]))		# ignore certain functions
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	# retrieve the department code and function name
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		funct <- tmp[2]
		tlog.loop(4,p,"Processing function ",p,"/",length(unique.pos)," dpt=",dpt," function=",funct)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_FCT_NOM]==funct)
		tlog(6,"Found ",length(idx)," rows")
		
		folder2 <- file.path(folder,dpt)
		dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder2,paste0(funct,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder2,paste0(funct,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
		
		if(length(idx)>1)
		{	# check if their dates overlap
			if(funct %in% ign.functs)
				tlog(6,"Function not uniquely identified: not checking date overlaps")			
			else
			{	tlog(6,"Checking date overlaps")
				ccount <- 0
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared rows
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared rows
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(date.intersect(start1, end1, start2, end2))
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
				
				# possibly add an empty row to separate cases
				tlog(6,"Found ",ccount," pairs of overlapping rows for this specific function position")
				if(ccount>0)
					tab <- rbind(tab, rep(NA,ncol(data)))
			}
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that two municipal councilors do not hold the exact same function at the same time.
#
# data: table containing the municipal data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.cm <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in municipal functions")
	
	# ignore the following (non-unique) functions
	ign.functs <- c("MAIRE DELEGUE")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique functions: departement code + city code + function name
	tlog(2,"Identifying all unique function positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	coms <- data[,COL_ATT_COM_CODE]
	functs <- data[,COL_ATT_FCT_NOM]
	pos <- apply(cbind(dpts,coms,functs),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos[!is.na(functs)]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	# retrieve the city code and function name
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		com <- tmp[2]
		funct <- tmp[3]
		tlog.loop(4,p,"Processing function ",p,"/",length(unique.pos)," dpt=",dpt," city=",com," function=",funct)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_COM_CODE]==com & data[,COL_ATT_FCT_NOM]==funct)
		tlog(6,"Found ",length(idx)," rows")
		
		folder2 <- file.path(folder,dpt)
		dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder2,paste0(com,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder2,paste0(com,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
			
		if(length(idx)>1)
		{	# check if their dates overlap
			if(funct %in% ign.functs)
				tlog(6,"Function not uniquely identified: not checking date overlaps")			
			else
			{	tlog(6,"Checking date overlaps")
				ccount <- 0
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared rows
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared rows
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(date.intersect(start1, end1, start2, end2))
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
				
				# possibly add an empty row to separate cases
				tlog(6,"Found ",ccount," pairs of overlapping rows for this specific function position")
				if(ccount>0)
					tab <- rbind(tab, rep(NA,ncol(data)))
			}
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that the number of simultaneous regional counsilors for each region is always under 
# the legal limit.
#
# data: table containing the regional data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.cr <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in regional positions")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique regions
	tlog(2,"Identifying all unique regions")
	unique.pos <- sort(unique(data[,COL_ATT_REG_CODE]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique region
	tlog.start.loop(2,length(unique.pos),"Processing each unique region")
	for(p in 1:length(unique.pos))
	{	tlog.loop(4,p,"Processing region ",unique.pos[p], "(",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_REG_CODE]==unique.pos[p])
		tlog(6,"Found ",length(idx)," rows")
		
		#if(length(idx)>1)
		{	# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(unique.pos[p],"_details.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,
				file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(unique.pos[p],".txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,
				file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				sep="\t"
			)
		}
	}
	
	# load the legal limit for the number of regional counsilors in each subdivision
	fn <- FILE_VERIF_NBR_CR
	tlog(0,"Loading verification file \"",fn,"\"")
	verif.table <- read.table(
		file=fn, 					# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses=c("character","character","integer","Date","Date")
	)
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Start date: ",format(start.date))
	tlog(4,"End date: ",format(end.date))
	
	# loop over each day in the period
	tab <- data[FALSE,]
	count <- 0
	cur.day <- start.date
	old.regs <- c()
	nbr <- end.date-start.date + 1
	tlog.start.loop(4,nbr,"Looping over time by 1-day increments")
	it <- 0
	while(cur.day <= end.date)
	{	it <- it + 1
		day <- as.integer(format(cur.day,format="%d"))
		next.day <- cur.day + 1
		found <- FALSE
		
		# get all mandates containing the current day
		day.idx <- which(sapply(1:nrow(data), function(r)
					date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
				))
		if(length(day.idx)>0)
		{	# count the number of mandates by region
			tt <- table(data[day.idx,COL_ATT_REG_NOM])
			# get the verification values (upper bounds) for the current day 
			per.idx <- which(sapply(1:nrow(verif.table), function(r)
						date.intersect(verif.table[r,COL_VERIF_MDT_DBT], verif.table[r,COL_VERIF_MDT_FIN], cur.day, cur.day)
					))
			# match to the region names
			midx <- match(names(tt),verif.table[per.idx, COL_ATT_REG_NOM])
			# compare the mandate counts and upper bounds
			ridx <- which(tt > verif.table[per.idx[midx], COL_VERIF_MDT_NBR])
			# record the problematic departments
			for(r in ridx)
			{	if(!(names(tt)[r] %in% old.regs))
				{	if(!found)
						tlog.loop(6,it,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
					tlog(8,"Problem with ",names(tt)[r],": ",tt[r],"/",verif.table[per.idx[midx[r]],COL_VERIF_MDT_NBR]," mandates found")
					count <- count + (tt[r]-verif.table[per.idx[midx[r]],COL_VERIF_MDT_NBR])
					zidx <- which(data[day.idx,COL_ATT_REG_NOM]==names(tt)[r])
					tab <- rbind(tab, data[day.idx[zidx],], rep(NA,ncol(data)))
					found <- TRUE
				}
			}
			old.regs <- names(tt)[ridx]
		}
		else
			old.regs <- c()
		
		if(day==1 && !found)
			tlog.loop(6,it,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
		
		# update current date
		cur.day <- next.day
		if(found)
			tab <- rbind(tab, rep(NA,ncol(data)))
	}
	tlog.end.loop(4,"Processing over: found a total of ",count," pairs of overlapping mandates for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	###### quite the same thing, but with function positions this time
	
	# ignore the following (non-unique) functions
	ign.functs <- c(
		"PRESIDENT DE COMMISSION",
		"AUTRE MEMBRE COMMISSION PERMANENTE"
	)
	
	# identify all unique functions: departement code + city code + function name
	tlog(2,"Identifying all unique function positions")
	regs <- data[,COL_ATT_REG_CODE]
	functs <- data[,COL_ATT_FCT_NOM]
	pos <- apply(cbind(regs,functs),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos[!is.na(functs)]))	# ignore certain functions
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	# retrieve the region code and function name
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		reg <- tmp[1]
		funct <- tmp[2]
		tlog.loop(4,p,"Processing function ",p,"/",length(unique.pos)," region=",reg," function=",funct)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_REG_CODE]==reg & data[,COL_ATT_FCT_NOM]==funct)
		tlog(6,"Found ",length(idx)," rows")
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder,paste0(reg,"_",funct,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder,paste0(reg,"_",funct,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
			
		if(length(idx)>1)
		{	# check if their dates overlap
			if(funct %in% ign.functs)
				tlog(6,"Function not uniquely identified: not checking date overlaps")			
			else
			{	tlog(6,"Checking date overlaps")
				ccount <- 0
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared rows
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared rows
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(date.intersect(start1, end1, start2, end2))
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
				
				# possibly add an empty row to separate cases
				tlog(6,"Found ",ccount," pairs of overlapping rows for this specific function position")
				if(ccount>0)
					tab <- rbind(tab, rep(NA,ncol(data)))
			}
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that the number of simultaneous members of the European Parliament for each circonscription 
# is always under the legal limit.
#
# data: table containing the regional data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.de <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in European positions")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique european circonscriptions 
	tlog(2,"Identifying all unique european circonscriptions")
	unique.pos <- sort(unique(data[,COL_ATT_CIRCE_NOM]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique circonscriptions
	tlog.start.loop(2,length(unique.pos),"Processing each unique european circonscriptions")
	for(p in 1:length(unique.pos))
	{	tlog.loop(4,p,"Processing european circonscription ",unique.pos[p], "(",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_CIRCE_NOM]==unique.pos[p])
		tlog(6,"Found ",length(idx)," rows")
		
		#if(length(idx)>1)
		{	reg.name <- chartr(old=" ",new="_",	x=unique.pos[p]) # replace spaces by underscores
			# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(reg.name,"_details.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,
				file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(reg.name,".txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,
				file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				sep="\t"
			)
		}
	}
	
	# load the legal limit for the number of members of the European Parliament in each subdivision
	fn <- FILE_VERIF_NBR_DE
	tlog(0,"Loading verification file \"",fn,"\"")
	verif.table <- read.table(
		file=fn, 					# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses=c("character","character","integer","Date","Date")
	)
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Start date: ",format(start.date))
	tlog(4,"End date: ",format(end.date))
	
	# loop over each day in the period
	tab <- data[FALSE,]
	count <- 0
	cur.day <- start.date
	old.circos <- c()
	nbr <- end.date-start.date + 1
	tlog.start.loop(4,nbr,"Looping over time by 1-day increments")
	it <- 0
	while(cur.day <= end.date)
	{	it <- it + 1
		day <- as.integer(format(cur.day,format="%d"))
		next.day <- cur.day + 1
		found <- FALSE
		
		# get all mandates containing the current day
		day.idx <- which(sapply(1:nrow(data), function(r)
					date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
				))
		if(length(day.idx)>0)
		{	# count the number of mandates by circonscription
			# TODO: no euro-circonscription before 2004, we should use a different approach if we get the data for this period
			tt <- table(data[day.idx,COL_ATT_CIRCE_NOM])
			# get the verification values (upper bounds) for the current day 
			per.idx <- which(sapply(1:nrow(verif.table), function(r)
						date.intersect(verif.table[r,COL_VERIF_MDT_DBT], verif.table[r,COL_VERIF_MDT_FIN], cur.day, cur.day)
					))
			# match to the circonscriptions names
			midx <- match(names(tt),verif.table[per.idx, COL_ATT_CIRCE_NOM])
			# compare the mandate counts and upper bounds
			ridx <- which(tt > verif.table[per.idx[midx], COL_VERIF_MDT_NBR])
			# record the problematic circonscriptions
			for(r in ridx)
			{	if(!(names(tt)[r] %in% old.circos))
				{	if(!found)
						tlog.loop(6,it,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
					tlog(8,"Problem with ",names(tt)[r],": ",tt[r],"/",verif.table[per.idx[midx[r]],COL_VERIF_MDT_NBR]," mandates found")
					count <- count + (tt[r]-verif.table[per.idx[midx[r]],COL_VERIF_MDT_NBR])
					zidx <- which(data[day.idx,COL_ATT_CIRCE_NOM]==names(tt)[r])
					tab <- rbind(tab, data[day.idx[zidx],], rep(NA,ncol(data)))
					found <- TRUE
				}
			}
			old.circos <- names(tt)[ridx]
		}
		else
			old.circos <- c()
		
		if(day==1 && !found)
			tlog.loop(6,it,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
		
		# update current date
		cur.day <- next.day
		if(found)
			tab <- rbind(tab, rep(NA,ncol(data)))
	}
	tlog.end.loop(4,"Processing over: found a total of ",count," pairs of overlapping mandates for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that two members of the parliament do not hold the exact same position at the same time.
#
# data: table containing the parliamentary data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.d <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in legislative positions")
	hon.fct <- c("PRESIDENT D AGE DE L ASSEMBLEE NATIONALE", "SECRETAIRE D AGE DE L ASSEMBLEE NATIONALE")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique mandate positions: department code + circonscription code
	tlog(2,"Identifying all unique mandate positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	circos <- data[,COL_ATT_CIRC_CODE]
	pos <- apply(cbind(dpts,circos),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog.start.loop(2,length(unique.pos),"Processing each unique mandate position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	# retrieve the department and circonscription codes
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		circo <- tmp[2]
		tlog.loop(4,p,"Processing mandate position ",p,"/",length(unique.pos)," dpt=",dpt," circo=",circo)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_CIRC_CODE]==circo)
		tlog(6,"Found ",length(idx)," rows")
		
		folder2 <- file.path(folder,dpt)
		dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
		
		# record the sequence of mandates for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder2,paste0(circo,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder2,paste0(circo,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
			
		if(length(idx)>1)
		{	# check if their dates overlap
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared rows
				mdt.start1 <- data[idx[i],COL_ATT_MDT_DBT]
				mdt.end1 <- data[idx[i],COL_ATT_MDT_FIN]
				fct.start1 <- data[idx[i],COL_ATT_FCT_DBT]
				fct.end1 <- data[idx[i],COL_ATT_FCT_FIN]
				fct.lib1 <- data[idx[i],COL_ATT_FCT_NOM]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared rows
					mdt.start2 <- data[idx[j],COL_ATT_MDT_DBT]
					mdt.end2 <- data[idx[j],COL_ATT_MDT_FIN]
					fct.start2 <- data[idx[j],COL_ATT_FCT_DBT]
					fct.end2 <- data[idx[j],COL_ATT_FCT_FIN]
					fct.lib2 <- data[idx[j],COL_ATT_FCT_NOM]
					
					# check if the periods intersect
					if(date.intersect(mdt.start1, mdt.end1, mdt.start2, mdt.end2))			# the mandate periods overlap
					{	# check the function
						if(data[idx[i],COL_ATT_ELU_ID]!=data[idx[j],COL_ATT_ELU_ID]			# if not the same person
							|| is.na(fct.start1) || is.na(fct.start2) 						# or same person with at most one function
							|| (date.intersect(fct.start1, fct.end1, fct.start2, fct.end2)	# or two overlapping functions
								&& (!fct.lib1 %in% hon.fct) && (!fct.lib2 %in% hon.fct)))	# provided they are not honorific ones
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
			}
			
			# possibly add an empty row to separate cases
			tlog(4,"Found ",ccount," pairs of overlapping mandate positions for this specific position")
			if(ccount>0)
				tab <- rbind(tab, rep(NA,ncol(data)))
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping mandate positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
	
	###### quite the same thing, but with function positions this time
	
	# ignore the following (non-unique) functions
	ign.functs <- c(
#		"MEMBRE",										# many of them (actually should be removed at AN integration)
		"QUESTEUR DE L ASSEMBLEE NATIONALE",			# several but not distinguished
		"SECRETAIRE DE L ASSEMBLEE NATIONALE",			# several but not distinguished
		"SECRETAIRE D AGE DE L ASSEMBLEE NATIONALE",	# several but not distinguished
		"VICE PRESIDENT DE L ASSEMBLEE NATIONALE"		# several but not distinguished
	)
	
	# identify all unique functions: function name
	tlog(2,"Identifying all unique function positions")
	functs <- data[,COL_ATT_FCT_NOM]
	unique.pos <- sort(unique(functs[!is.na(functs)]))	# ignore certain functions
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	# retrieve the function name
		funct <- unique.pos[p]
		tlog.loop(4,p,"Processing function ",funct," (",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_FCT_NOM]==funct)
		tlog(6,"Found ",length(idx)," rows")
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder,paste0(funct,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder,paste0(funct,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
			
		if(length(idx)>1)
		{	# check if their dates overlap
			if(funct %in% ign.functs)
				tlog(6,"Function not uniquely identified: not checking date overlaps")			
			else
			{	tlog(6,"Checking date overlaps")
				ccount <- 0
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared rows
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared rows
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(date.intersect(start1, end1, start2, end2))
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
				
				# possibly add an empty row to separate cases
				tlog(6,"Found ",ccount," pairs of overlapping rows for this specific function position")
				if(ccount>0)
					tab <- rbind(tab, rep(NA,ncol(data)))
			}
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that two EPCI councilors do not hold the exact same unique function at the same time.
#
# data: table containing the municipal data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.epci <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in EPCI functions")
	
	# ignore the following (non-unique) functions
	ign.functs <- c("VICE PRESIDENT D EPCI")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique functions: departement code + city code + function name
	tlog(2,"Identifying all unique function positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	siren <- data[,COL_ATT_EPCI_SIREN]
	functs <- data[,COL_ATT_FCT_NOM]
	pos <- apply(cbind(dpts,siren,functs),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos[!is.na(functs)]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	tab <- data[FALSE,]
	count <- 0
	for(p in 1:length(unique.pos))
	{	# retrieve the city code and function name
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		siren <- tmp[2]
		funct <- tmp[3]
		tlog.loop(4,p,"Processing function ",p,"/",length(unique.pos)," dpt=",dpt," siren=",siren," function=",funct)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_EPCI_SIREN]==siren & data[,COL_ATT_FCT_NOM]==funct)
		tlog(6,"Found ",length(idx)," rows")
		
		folder2 <- file.path(folder,dpt)
		dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder2,paste0(siren,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder2,paste0(siren,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
		
		if(length(idx)>1)
		{	# check if their dates overlap
			if(funct %in% ign.functs)
				tlog(6,"Function not uniquely identified: not checking date overlaps")			
			else
			{	tlog(6,"Checking date overlaps")
				ccount <- 0
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared rows
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared rows
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(date.intersect(start1, end1, start2, end2))
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
				
				# possibly add an empty row to separate cases
				tlog(6,"Found ",ccount," pairs of overlapping rows for this specific function position")
				if(ccount>0)
					tab <- rbind(tab, rep(NA,ncol(data)))
			}
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that two mayors do not hold the exact same position at the same time.
#
# data: table containing the mayoral data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.m <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in mayoral positions")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique function positions: department + circonscription
	tlog(2,"Identifying all unique function positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	coms <- data[,COL_ATT_COM_CODE]
	pos <- apply(cbind(dpts,coms),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	count <- 0
	tab <- data[FALSE,]
	for(p in 1:length(unique.pos))
	{	# retrieve the department and circonscription codes
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		com <- tmp[2]
		tlog.loop(4,p,"Processing function position ",p,"/",length(unique.pos)," dpt=",dpt," city=",com)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_COM_CODE]==com)
		tlog(6,"Found ",length(idx)," rows")
		
		tlog(6,"Checking function position overlaps")
		folder2 <- file.path(folder,dpt)
		dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder2,paste0(com,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder2,paste0(com,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
		

		if(length(idx)>1)
		{	# check if the function dates overlap
			tlog(6,"Checking function position overlaps")
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared rows
				start1 <- data[idx[i],COL_ATT_FCT_DBT]
				end1 <- data[idx[i],COL_ATT_FCT_FIN]
				
				if(!(is.na(start1) && is.na(end1)))
				{	for(j in (i+1):length(idx))
					{	# get the dates of the second compared rows
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(!(is.na(start2) && is.na(end2)))
						{	tlog(8, "Comparing ",format(start1),"--",format(end1)," vs. ",format(start2),"--",format(end2))
							if(date.intersect(start1, end1, start2, end2))
							{	# add to the table of problematic cases
								tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
								# add a row of NAs in order to separate pairs of cases
								count <- count + 1
								ccount <- ccount + 1
							}
						}
					}
				}
			}
			
			# possibly add an empty row to separate cases
			tlog(6,"Found ",ccount," pairs of overlapping function positions for this specific position")
			if(ccount>0)
				tab <- rbind(tab, rep(NA,ncol(data)))
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the tables of problematic function cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab, 
			file=tab.file,
			#fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			#quote=TRUE,
			sep="\t"
		)
	}
}




#############################################################################################
# Checks that the number of simultaneous senators for each department is always under the legal 
# limit.
#
# data: table containing the senatorial data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.s <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in senatorial positions")
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique departments
	tlog(2,"Identifying all unique departments")
	unique.pos <- sort(unique(data[,COL_ATT_DPT_CODE]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique department
	tlog.start.loop(2,length(unique.pos),"Processing each unique department")
	for(p in 1:length(unique.pos))
	{	tlog.loop(4,p,"Processing department ",p,"/",length(unique.pos)," dpt=",unique.pos[p])
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==unique.pos[p])
		tlog(6,"Found ",length(idx)," rows")
		
		#if(length(idx)>1)
		{	# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(unique.pos[p],"_details.txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,
				file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(unique.pos[p],".txt"))
			tlog(6,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,
				file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				sep="\t"
			)
		}
	}
	
	# load the legal limit for the number of senators in each department
	fn <- FILE_VERIF_NBR_S
	tlog(0,"Loading verification file \"",fn,"\"")
	verif.table <- read.table(
		file=fn, 					# name of the data file
		header=TRUE, 				# look for a header
		sep="\t", 					# character used to separate columns 
		check.names=FALSE, 			# don't change the column names from the file
		comment.char="", 			# ignore possible comments in the content
		row.names=NULL, 			# don't look for row names in the file
		quote="", 					# don't expect double quotes "..." around text fields
		as.is=TRUE,					# don't convert strings to factors
#		fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
		colClasses=c("character","character","integer","Date","Date")
	)
	# NOTE in theory, should not use department codes, because they changed over time
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Start date: ",format(start.date))
	tlog(4,"End date: ",format(end.date))
	
	# loop over each day in the period
	tab <- data[FALSE,]
	count <- 0
	cur.day <- start.date
	old.dpts <- c()
	nbr <- end.date-start.date + 1
	tlog.start.loop(4,nbr,"Looping over time by 1-day increments")
	it <- 0
	while(cur.day <= end.date)
	{	it <- it + 1
		day <- as.integer(format(cur.day,format="%d"))
		next.day <- cur.day + 1
		found <- FALSE
		
		# get all mandates containing the current day
		day.idx <- which(sapply(1:nrow(data), function(r)
					date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
				))
		if(length(day.idx)>0)
		{	# count the number of mandates by department
			tt <- table(data[day.idx,COL_ATT_DPT_NOM])
			# get the verification values (upper bounds) for the current day 
			per.idx <- which(sapply(1:nrow(verif.table), function(r)
						date.intersect(verif.table[r,COL_VERIF_MDT_DBT], verif.table[r,COL_VERIF_MDT_FIN], cur.day, cur.day)
					))
			# match to the departments names
			midx <- match(names(tt),verif.table[per.idx, COL_ATT_DPT_NOM])
			# compare the mandate counts and upper bounds
			didx <- which(tt > verif.table[per.idx[midx], COL_VERIF_MDT_NBR])
			# record the problematic departments
			for(d in didx)
			{	if(!(names(tt)[d] %in% old.dpts))
				{	if(!found)
						tlog.loop(6,it,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
					tlog(8,"Problem with ",names(tt)[d],": ",tt[d],"/",verif.table[per.idx[midx[d]],COL_VERIF_MDT_NBR]," mandates found")
					count <- count + (tt[d]-verif.table[per.idx[midx[d]],COL_VERIF_MDT_NBR])
					zidx <- which(data[day.idx,COL_ATT_DPT_NOM]==names(tt)[d])
					tab <- rbind(tab, data[day.idx[zidx],], rep(NA,ncol(data)))
					found <- TRUE
				}
			}
			old.dpts <- names(tt)[didx]
		}
		else
			old.dpts <- c()
		
		if(day==1 && !found)
			tlog.loop(6,it,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
		
		# update current date
		cur.day <- next.day
		if(found)
			tab <- rbind(tab, rep(NA,ncol(data)))
	}
	tlog.end.loop(4,"Processing over: found a total of ",count," pairs of overlapping mandate positionss for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}

	###### quite the same thing, but with function positions this time
	
	# ignore the following (non-unique) functions
	ign.functs <- c(
			"QUESTEUR DU SENAT",		# several but not distinguished
			"SECRETAIRE DU SENAT",		# several but not distinguished
			"VICE PRESIDENT DU SENAT"	# several but not distinguished
		)
	
	# identify all unique functions: function name
	tlog(2,"Identifying all unique function positions")
	functs <- data[,COL_ATT_FCT_NOM]
	unique.pos <- sort(unique(functs[!is.na(functs)]))	# ignore certain functions
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tab <- data[FALSE,]
	count <- 0
	tlog.start.loop(2,length(unique.pos),"Processing each unique function position")
	for(p in 1:length(unique.pos))
	{	# retrieve the function name
		funct <- unique.pos[p]
		tlog.loop(4,p,"Processing function ",funct,"(",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_FCT_NOM]==funct)
		tlog(6,"Found ",length(idx)," rows")
		
		# record the sequence of functions for this position
		idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
						data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
		tab2 <- cbind(idx2, data[idx2,])
		colnames(tab2) <- "Ligne"
		tab.file <- file.path(folder,paste0(funct,"_details.txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#				fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#				quote=TRUE,
			sep="\t"
		)
		tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_FCT_DBT,COL_ATT_FCT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
		tab.file <- file.path(folder,paste0(funct,".txt"))
		tlog(6,"Recording in file \"",tab.file,"\"")
		write.table(x=tab2,
			file=tab.file,
#				fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
			quote=FALSE,
			sep="\t"
		)
			
		if(length(idx)>1)
		{	# check if the function dates overlap
			if(funct %in% ign.functs)
				tlog(6,"Function not uniquely identified: not checking date overlaps")			
			else
			{	tlog(6,"Checking date overlaps")
				ccount <- 0
				for(i in 1:(length(idx)-1))
				{	# get the dates of the first compared row
					start1 <- data[idx[i],COL_ATT_FCT_DBT]
					end1 <- data[idx[i],COL_ATT_FCT_FIN]
					
					for(j in (i+1):length(idx))
					{	# get the dates of the second compared row
						start2 <- data[idx[j],COL_ATT_FCT_DBT]
						end2 <- data[idx[j],COL_ATT_FCT_FIN]
						
						# check if the periods intersect
						if(!is.na(start1) && !is.na(start2)
							&& date.intersect(start1, end1, start2, end2))
						{	# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
				
				# possibly add an empty row to separate cases
				tlog(6,"Found ",ccount," pairs of overlapping rows for this specific function position")
				if(ccount>0)
					tab <- rbind(tab, rep(NA,ncol(data)))
			}
		}
	}
	tlog.end.loop(2,"Processing over: found a total of ",count," pairs of overlapping function positions for the whole table")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,
			file=tab.file,
#			fileEncoding="UTF-8",
			row.names=FALSE, 
			col.names=TRUE,
#			quote=TRUE,
			sep="\t"
		)
	}
}
