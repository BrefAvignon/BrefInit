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
	tab <- data[FALSE,]
	count <- 0
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique positions: generated unique code
	tlog(2,"Identifying all unique positions")
	unique.pos <- sort(unique(data[,COL_ATT_CANT_ID]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog(2,"Processing each unique position")
	for(p in 1:length(unique.pos))
	{	tlog(4,"Processing position ",unique.pos[p]," (",p,"/",length(unique.pos),")")
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_CANT_ID]==unique.pos[p])
		tlog(4,"Found ",length(idx)," mandates")
		
		if(length(idx)>1)
		{	# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(unique.pos[p],"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(unique.pos[p],".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
			)
			
			# check if their dates overlap
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared mandate
				start1 <- data[idx[i],COL_ATT_MDT_DBT]
				end1 <- data[idx[i],COL_ATT_MDT_FIN]
				sex1 <- data[idx[i],COL_ATT_ELU_SEXE]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared mandate
					start2 <- data[idx[j],COL_ATT_MDT_DBT]
					end2 <- data[idx[j],COL_ATT_MDT_FIN]
					sex2 <- data[idx[j],COL_ATT_ELU_SEXE]
					
					# check if the periods intersect
					if(date.intersect(start1, end1, start2, end2))
					{	# problem only if before 2015 or persons of the same sex
						if(get.year(start1)<2015  || sex1==sex2)
						{	if(get.year(start1)<2015)
								tlog(6,"Date before 2015")
							if(sex1==sex2)
								tlog(6,"Persons of the same sex")
							# add to the table of problematic cases
							tab <- rbind(tab, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
							# add a row of NAs in order to separate pairs of cases
							count <- count + 1
							ccount <- ccount + 1
						}
					}
				}
			}
			
			# possibly add an empty row to separate cases
			tlog(4,"Found ",ccount," pair(s) of overlapping mandates of this specific position")
			if(ccount>0)
				tab <- rbind(tab, rep(NA,ncol(data)))
		}
	}
	tlog(2,"Processing over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
		tlog(4,"Found a total of ",count," pairs of overlapping mandates for the whole table")
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
	tab <- data[FALSE,]
	count <- 0
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique functions: city + function
	tlog(2,"Identifying all unique functions")
	dpts <- data[,COL_ATT_DPT_CODE]
	coms <- data[,COL_ATT_COM_CODE]
	functs <- data[,COL_ATT_FCT_NOM]
	pos <- apply(cbind(dpts,coms,functs),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos))
	idx <- which(substr(unique.pos,nchar(unique.pos)-2,nchar(unique.pos))==":NA")
	unique.pos <- unique.pos[-idx]
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog(2,"Processing each unique function")
	for(p in 1:length(unique.pos))
	{	# retrieve the city code and function name
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		com <- tmp[2]
		funct <- tmp[3]
		tlog(4,"Processing function ",p,"/",length(unique.pos)," dpt=",dpt," city=",com," function=",funct)
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_COM_CODE]==com & data[,COL_ATT_FCT_NOM]==funct)
		tlog(4,"Found ",length(idx)," function mandates")
		
		if(length(idx)>1)
		{	folder2 <- file.path(folder,dpt)
			dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
			
			# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder2,paste0(com,"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder2,paste0(com,".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
			)
			
			# check if their dates overlap
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared mandate
				start1 <- data[idx[i],COL_ATT_FCT_DBT]
				end1 <- data[idx[i],COL_ATT_FCT_FIN]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared mandate
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
			tlog(4,"Found ",ccount," pairs of overlapping mandates of this specific function")
			if(ccount>0)
				tab <- rbind(tab, rep(NA,ncol(data)))
		}
	}
	tlog(2,"Processing over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
		tlog(4,"Found a total of ",count," pairs of overlapping mandates for the whole table")
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
	tab <- data[FALSE,]
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique regions
	tlog(2,"Identifying all unique regions")
	unique.pos <- sort(unique(data[,COL_ATT_REG_CODE]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique region
	tlog(2,"Processing each unique region")
	for(p in 1:length(unique.pos))
	{	tlog(4,"Processing region ",unique.pos[p], "(",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_REG_CODE]==unique.pos[p])
		tlog(4,"Found ",length(idx)," rows")
		
		if(length(idx)>1)
		{	# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(unique.pos[p],"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(unique.pos[p],".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
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
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
			colClasses=c("character","character","integer","Date","Date")
	)
	# TODO should not use region codes, because they changed over time
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Start date: ",format(start.date))
	tlog(4,"End date: ",format(end.date))
	
	# loop over each day in the period
	cur.day <- start.date
	old.regs <- c()
	tlog(4,"Looping over time by 1-day increments")
	while(cur.day <= end.date)
	{	day <- as.integer(format(cur.day,format="%d"))
		next.day <- cur.day + 1
		found <- FALSE
		
		# get all mandates containing the current day
		day.idx <- which(sapply(1:nrow(data), function(r)
							date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
				))
		if(day==1)
			tlog(6,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
		if(length(day.idx)>0)
		{	# count the number of mandates by region
			tt <- table(data[day.idx,COL_ATT_REG_NOM])
			# get the verification values (upper bounds) for the current day 
			per.idx <- which(sapply(1:nrow(verif.table), function(r)
								date.intersect(verif.table[r,COL_VERIF_MDT_DBT], verif.table[r,COL_VERIF_MDT_FIN], cur.day, cur.day)
					))
			# match to the departments names
			midx <- match(names(tt),verif.table[per.idx, COL_VERIF_REG_NOM])
			# compare the mandate counts and upper bounds
			ridx <- which(tt > verif.table[per.idx[midx], COL_VERIF_MDT_NBR])
			# record the problematic departments
			for(r in ridx)
			{	if(!(names(tt)[r] %in% old.regs))
				{	tlog(8,"Problem with ",names(tt)[r],": ",tt[r],"/",verif.table[per.idx[midx[r]],COL_VERIF_MDT_NBR]," mandates found")
					zidx <- which(data[day.idx,COL_ATT_REG_NOM]==names(tt)[r])
					tab <- rbind(tab, data[day.idx[zidx],], rep(NA,ncol(data)))
					found <- TRUE
				}
			}
			old.regs <- names(tt)
		}
		else
			old.regs <- c()
		
		# update current date
		cur.day <- next.day
		if(found)
			tab <- rbind(tab, rep(NA,ncol(data)))
	}
	tlog(4,"Looping over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
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
	tab <- data[FALSE,]
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique circonscriptions
	tlog(2,"Identifying all unique circonscriptions")
	unique.pos <- sort(unique(data[,COL_ATT_CIRCE_NOM]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique circonscriptions
	tlog(2,"Processing each unique circonscriptions")
	for(p in 1:length(unique.pos))
	{	tlog(4,"Processing circonscriptions ",unique.pos[p], "(",p,"/",length(unique.pos),")")
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_CIRCE_NOM]==unique.pos[p])
		tlog(4,"Found ",length(idx)," rows")
		
		if(length(idx)>1)
		{	reg.name <- chartr(old=" ",new="_",	x=unique.pos[p]) # replace spaces by underscores
			# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(reg.name,"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(reg.name,".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
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
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
			colClasses=c("character","character","integer","Date","Date")
	)
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Start date: ",format(start.date))
	tlog(4,"End date: ",format(end.date))
	
	# loop over each day in the period
	cur.day <- start.date
	old.circo <- c()
	tlog(4,"Looping over time by 1-day increments")
	while(cur.day <= end.date)
	{	day <- as.integer(format(cur.day,format="%d"))
		next.day <- cur.day + 1
		found <- FALSE
		
		# get all mandates containing the current day
		day.idx <- which(sapply(1:nrow(data), function(r)
							date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
				))
		if(day==1)
			tlog(6,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
		if(length(day.idx)>0)
		{	# count the number of mandates by subdivision
			# TODO: no subdivision before 2004, we should use a different approach if we get the data for this period
			tt <- table(data[day.idx,COL_ATT_CIRCE_NOM])
			# get the verification values (upper bounds) for the current day 
			per.idx <- which(sapply(1:nrow(verif.table), function(r)
								date.intersect(verif.table[r,COL_VERIF_MDT_DBT], verif.table[r,COL_VERIF_MDT_FIN], cur.day, cur.day)
					))
			# match to the departments names
			midx <- match(names(tt),verif.table[per.idx, COL_VERIF_CIRCE_NOM])
			# compare the mandate counts and upper bounds
			ridx <- which(tt > verif.table[per.idx[midx], COL_VERIF_MDT_NBR])
			# record the problematic circonscriptions
			for(r in ridx)
			{	if(!(names(tt)[r] %in% old.circo))
				{	tlog(8,"Problem with ",names(tt)[r],": ",tt[r],"/",verif.table[per.idx[midx[r]],COL_VERIF_MDT_NBR]," mandates found")
					zidx <- which(data[day.idx,COL_ATT_CIRCE_NOM]==names(tt)[r])
					tab <- rbind(tab, data[day.idx[zidx],], rep(NA,ncol(data)))
					found <- TRUE
				}
			}
			old.circo <- names(tt)
		}
		else
			old.circo <- c()
		
		# update current date
		cur.day <- next.day
		if(found)
			tab <- rbind(tab, rep(NA,ncol(data)))
	}
	tlog(4,"Looping over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
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
	tab <- data[FALSE,]
	count <- 0
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique positions: department + circonscription
	tlog(2,"Identifying all unique positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	circos <- data[,COL_ATT_CIRC_CODE]
	pos <- apply(cbind(dpts,circos),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog(2,"Processing each unique position")
	for(p in 1:length(unique.pos))
	{	# retrieve the department and circonscription codes
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		circo <- as.integer(tmp[2])
		tlog(4,"Processing position ",p,"/",length(unique.pos)," dpt=",dpt," circo=",circo)
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_CIRC_CODE]==circo)
		tlog(4,"Found ",length(idx)," mandates")
		
		if(length(idx)>1)
		{	# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(unique.pos[p],"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(unique.pos[p],".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
			)
			
			# check if their dates overlap
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared mandate
				start1 <- data[idx[i],COL_ATT_MDT_DBT]
				end1 <- data[idx[i],COL_ATT_MDT_FIN]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared mandate
					start2 <- data[idx[j],COL_ATT_MDT_DBT]
					end2 <- data[idx[j],COL_ATT_MDT_FIN]
					
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
			tlog(4,"Found ",ccount," pairs of overlapping mandates of this specific position")
			if(ccount>0)
				tab <- rbind(tab, rep(NA,ncol(data)))
		}
	}
	tlog(2,"Processing over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
		tlog(4,"Found a total of ",count," pairs of overlapping mandates for the whole table")
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
	count <- 0
	tab.m <- data[FALSE,]
	tab.f <- data[FALSE,]
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique positions: department + circonscription
	tlog(2,"Identifying all unique positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	coms <- data[,COL_ATT_COM_CODE]
	pos <- apply(cbind(dpts,coms),1,function(r) paste(r,collapse="_"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog(2,"Processing each unique position")
	for(p in 1:length(unique.pos))
	{	# retrieve the department and circonscription codes
		tmp <- strsplit(unique.pos[p],"_")[[1]]
		dpt <- tmp[1]
		com <- tmp[2]
		tlog(4,"Processing position ",p,"/",length(unique.pos)," dpt=",dpt," city=",com)
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_COM_CODE]==com)
		tlog(4,"Found ",length(idx)," rows")
		
		tlog(4,"Checking mandate overlaps")
		if(length(idx)>1)
		{	folder2 <- file.path(folder,dpt)
			dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
			
			# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder2,paste0(com,"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder2,paste0(com,".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
			)
			
			# check if the mandate dates overlap
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared mandate
				start1 <- data[idx[i],COL_ATT_MDT_DBT]
				end1 <- data[idx[i],COL_ATT_MDT_FIN]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared mandate
					start2 <- data[idx[j],COL_ATT_MDT_DBT]
					end2 <- data[idx[j],COL_ATT_MDT_FIN]
					
					# check if the periods intersect
					if(date.intersect(start1, end1, start2, end2))
					{	# add to the table of problematic cases
						tab.m <- rbind(tab.m, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
						# add a row of NAs in order to separate pairs of cases
						count <- count + 1
						ccount <- ccount + 1
					}
				}
			}
			
			# possibly add an empty row to separate cases
			tlog(6,"Found ",ccount," pairs of overlapping mandates of this specific position")
			if(ccount>0)
				tab.m <- rbind(tab.m, rep(NA,ncol(data)))
		
			# check if the function dates overlap
			tlog(4,"Checking function overlaps")
			ccount <- 0
			for(i in 1:(length(idx)-1))
			{	# get the dates of the first compared function
				start1 <- data[idx[i],COL_ATT_FCT_DBT]
				end1 <- data[idx[i],COL_ATT_FCT_FIN]
				
				for(j in (i+1):length(idx))
				{	# get the dates of the second compared function
					start2 <- data[idx[j],COL_ATT_FCT_DBT]
					end2 <- data[idx[j],COL_ATT_FCT_FIN]
					
					# check if the periods intersect
					if(date.intersect(start1, end1, start2, end2))
					{	# add to the table of problematic cases
						tab.f <- rbind(tab.f, data[c(idx[i],idx[j]),], rep(NA,ncol(data)))
						# add a row of NAs in order to separate pairs of cases
						count <- count + 1
						ccount <- ccount + 1
					}
				}
			}
			
			# possibly add an empty row to separate cases
			tlog(6,"Found ",ccount," pairs of overlapping functions of this specific position")
			if(ccount>0)
				tab.f <- rbind(tab.f, rep(NA,ncol(data)))
		}
	}
	tlog(2,"Processing over")
	
	# possibly record the tables of problematic mandate cases
	if(nrow(tab.m)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab.m,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE,
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
		tlog(4,"Found a total of ",count," pairs of overlapping mandates for the whole table")
	}
	# possibly record the tables of problematic function cases
	if(nrow(tab.f)>0)
	{	tab.file <- file.path(out.folder,"fonction_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab.f,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
		tlog(4,"Found a total of ",count," pairs of overlapping functions for the whole table")
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
	tab <- data[FALSE,]
	
	# possibly create folder to output detailed position chronology
	folder <- file.path(out.folder,"positions")
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# identify all unique departments
	tlog(2,"Identifying all unique departments")
	unique.pos <- sort(unique(data[,COL_ATT_DPT_CODE]))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique department
	tlog(2,"Processing each unique department")
	for(p in 1:length(unique.pos))
	{	tlog(4,"Processing department ",p,"/",length(unique.pos)," dpt=",unique.pos[p])
		
		# get the corresponding rows
		idx <- which(data[,COL_ATT_DPT_CODE]==unique.pos[p])
		tlog(4,"Found ",length(idx)," rows")
		
		if(length(idx)>1)
		{	# record the sequence of mandates for this position
			idx2 <- idx[order(data[idx,COL_ATT_MDT_DBT], data[idx,COL_ATT_MDT_FIN], 
							data[idx,COL_ATT_FCT_DBT], data[idx,COL_ATT_FCT_DBT])]
			tab2 <- cbind(idx2, data[idx2,])
			colnames(tab2) <- "Ligne"
			tab.file <- file.path(folder,paste0(unique.pos[p],"_details.txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
			)
			tab2 <- data[idx2,c(COL_ATT_MDT_DBT,COL_ATT_MDT_FIN,COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM,COL_ATT_ELU_ID)]
			tab.file <- file.path(folder,paste0(unique.pos[p],".txt"))
			tlog(4,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
				quote=FALSE,
				se="\t"
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
#			fileEncoding="Latin1"		# original tables seem to be encoded in Latin1 (ANSI)
			colClasses=c("character","character","integer","Date","Date")
	)
	# in theory, should not use department codes, because they changed over time
	
	# set up start/end dates
	start.date <- min(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	end.date <- max(c(data[,COL_ATT_MDT_DBT],data[,COL_ATT_MDT_FIN]),na.rm=TRUE)
	tlog(4,"Start date: ",format(start.date))
	tlog(4,"End date: ",format(end.date))
	
	# loop over each day in the period
	cur.day <- start.date
	old.dpts <- c()
	tlog(4,"Looping over time by 1-day increments")
	while(cur.day <= end.date)
	{	day <- as.integer(format(cur.day,format="%d"))
		next.day <- cur.day + 1
		found <- FALSE
		
		# get all mandates containing the current day
		day.idx <- which(sapply(1:nrow(data), function(r)
							date.intersect(data[r,COL_ATT_MDT_DBT], data[r,COL_ATT_MDT_FIN], cur.day, cur.day)
				))
		if(day==1)
			tlog(6,"Processing day ",format(cur.day),": ",length(day.idx)," occurrence(s)")
		if(length(day.idx)>0)
		{	# count the number of mandates by department
			tt <- table(data[day.idx,COL_ATT_DPT_NOM])
			# get the verification values (upper bounds) for the current day 
			per.idx <- which(sapply(1:nrow(verif.table), function(r)
								date.intersect(verif.table[r,COL_VERIF_MDT_DBT], verif.table[r,COL_VERIF_MDT_FIN], cur.day, cur.day)
						))
			# match to the departments names
			midx <- match(names(tt),verif.table[per.idx, COL_VERIF_DPT_NOM])
			# compare the mandate counts and upper bounds
			didx <- which(tt > verif.table[per.idx[midx], COL_VERIF_MDT_NBR])
			# record the problematic departments
			for(d in didx)
			{	if(!(names(tt)[d] %in% old.dpts))
				{	tlog(8,"Problem with ",names(tt)[d],": ",tt[d],"/",verif.table[per.idx[midx[d]],COL_VERIF_MDT_NBR]," mandates found")
					zidx <- which(data[day.idx,COL_ATT_DPT_NOM]==names(tt)[d])
					tab <- rbind(tab, data[day.idx[zidx],], rep(NA,ncol(data)))
					found <- TRUE
				}
			}
			old.dpts <- names(tt)
		}
		else
			old.dpts <- c()
		
		# update current date
		cur.day <- next.day
		if(found)
			tab <- rbind(tab, rep(NA,ncol(data)))
	}
	tlog(4,"Looping over")
	
	# possibly record the table of problematic cases
	if(nrow(tab)>0)
	{	tab.file <- file.path(out.folder,"mandat_problems_overlap.txt")
		tlog(2,"Recording in file \"",tab.file,"\"")
		write.table(x=tab,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				se="\t"
		)
	}
}
