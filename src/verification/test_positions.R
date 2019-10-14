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
	
	# identify all unique positions: department + canton
	tlog(2,"Identifying all unique positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	circos <- data[,COL_ATT_CANT_CODE]
	pos <- apply(cbind(dpts,circos),1,function(r) paste(r,collapse=":"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog(2,"Processing each unique position")
	for(p in 1:length(unique.pos))
	{	# retrieve the department and canton codes
		tmp <- strsplit(unique.pos[p],":")[[1]]
		dpt <- tmp[1]
		cant <- as.integer(tmp[2])
		tlog(4,"Processing position ",p,"/",length(unique.pos)," dpt=",dpt," canton=",cant)
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_CANT_CODE]==cant)
		tlog(4,"Found ",length(idx)," mandates")
		# check if their dates overlap
		if(length(idx)>1)
		{	ccount <- 0
			
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
		write.table(x=tab,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
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
	
	# identify all unique functions: city + function
	tlog(2,"Identifying all unique functions")
	dpts <- data[,COL_ATT_DPT_CODE_M]
	coms <- data[,COL_ATT_COM_CODE]
	functs <- data[,COL_ATT_FCT_NOM]
	pos <- apply(cbind(dpts,coms,functs),1,function(r) paste(r,collapse=":"))
	unique.pos <- sort(unique(pos))
	idx <- which(substr(unique.pos,nchar(unique.pos)-2,nchar(unique.pos))==":NA")
	unique.pos <- unique.pos[-idx]
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique function
	tlog(2,"Processing each unique function")
	for(p in 1:length(unique.pos))
	{	# retrieve the city code and function name
		tmp <- strsplit(unique.pos[p],":")[[1]]
		dpt <- tmp[1]
		com <- tmp[2]
		funct <- tmp[3]
		tlog(4,"Processing function ",p,"/",length(unique.pos)," dpt=",dpt," city=",com," function=",funct)
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_DPT_CODE_M]==dpt & data[,COL_ATT_COM_CODE]==com & data[,COL_ATT_FCT_NOM]==funct)
		tlog(4,"Found ",length(idx)," function mandates")
		# check if their dates overlap
		if(length(idx)>1)
		{	ccount <- 0
			
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
		write.table(x=tab,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		tlog(4,"Found a total of ",count," pairs of overlapping mandates for the whole table")
	}
}




#############################################################################################
# Checks that two members of the parliament do not hold the exact same position at the same time.
#
# data: table containing the parliamentary data.
# out.folder: folder where to output the results.
#############################################################################################
test.position.d <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in parliamentary positions")
	tab <- data[FALSE,]
	count <- 0
	
	# identify all unique positions: department + circonscription
	tlog(2,"Identifying all unique positions")
	dpts <- data[,COL_ATT_DPT_CODE]
	circos <- data[,COL_ATT_CIRC_CODE]
	pos <- apply(cbind(dpts,circos),1,function(r) paste(r,collapse=":"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog(2,"Processing each unique position")
	for(p in 1:length(unique.pos))
	{	# retrieve the department and circonscription codes
		tmp <- strsplit(unique.pos[p],":")[[1]]
		dpt <- tmp[1]
		circo <- as.integer(tmp[2])
		tlog(4,"Processing position ",p,"/",length(unique.pos)," dpt=",dpt," circo=",circo)
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_DPT_CODE]==dpt & data[,COL_ATT_CIRC_CODE]==circo)
		tlog(4,"Found ",length(idx)," mandates")
		# check if their dates overlap
		if(length(idx)>1)
		{	ccount <- 0
			
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
		write.table(x=tab,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
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
{	tlog(0,"Trying to detect problems in parliamentary positions")
	tab <- data[FALSE,]
	count <- 0
	
	# identify all unique positions: department + circonscription
	tlog(2,"Identifying all unique positions")
	dpts <- data[,COL_ATT_DPT_CODE_M]
	coms <- data[,COL_ATT_COM_CODE]
	pos <- apply(cbind(dpts,coms),1,function(r) paste(r,collapse=":"))
	unique.pos <- sort(unique(pos))
	tlog(4,"Found ",length(unique.pos)," of them")
	
	# process each unique position
	tlog(2,"Processing each unique position")
	for(p in 1:length(unique.pos))
	{	# retrieve the department and circonscription codes
		tmp <- strsplit(unique.pos[p],":")[[1]]
		dpt <- tmp[1]
		com <- tmp[2]
		tlog(4,"Processing position ",p,"/",length(unique.pos)," dpt=",dpt," city=",com)
		
		# get the corresponding mandates
		idx <- which(data[,COL_ATT_DPT_CODE_M]==dpt & data[,COL_ATT_COM_CODE]==com)
		tlog(4,"Found ",length(idx)," mandates")
		# check if their dates overlap
		if(length(idx)>1)
		{	ccount <- 0
			
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
		write.table(x=tab,file=tab.file,row.names=FALSE,col.names=TRUE,fileEncoding="UTF8")
		tlog(4,"Found a total of ",count," pairs of overlapping mandates for the whole table")
	}
}
