#############################################################################################
# Functions performing various tests on location names.
# 
# 12/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Checks whether the same location name exists with and without an article (L, LE, LA, LES).
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
#test.locations.articles <- function(data, out.folder)
#{	tlog(0,"Trying to detect problems in location names")
#	
#	# identify location columns in the table
#	cols <- colnames(data)
#	cols <- intersect(cols,COLS_ATT_LOCATION_NOUNS)
#	tlog(2,"Identifying location columns: ",paste(cols,collapse=", "))
#	
#	# process each location column in the table
#	for(col in cols)
#	{	tlog(2,"Processing column \"",col,"\"")
#		count <- 0
#		
#		# retrieve unique values
#		vals <- sort(unique(data[,col]))
#		
#		# look for names starting with an article
#		idx <- which(startsWith(x=vals, prefix="L ")
#						| startsWith(x=vals, prefix="LA ")
#						| startsWith(x=vals, prefix="LE ")
#						| startsWith(x=vals, prefix="LES "))
#		
#		# for each of them, lookup an article-less version
#		if(length(idx)>0)
#		{	# get the article-less versions of the name
#			vals1 <- vals[idx]
#			vals2 <- gsub(x=vals1, pattern="^(L|LA|LE|LES) ",replacement="")
#			
#			# look them up in the name list 
#			idx <- which(!is.na(match(vals2,vals)))
#			
#			if(length(idx)>0)
#			{	# init result table
#				tab <- cbind(vals1[idx],vals2[idx])
#				colnames(tab) <- c("AvecArticle","SansArticle")
#				#idx1 <- match(vals1[idx],data[,"Libelle commune"])
#				#idx2 <- match(vals2[idx],data[,"Libelle commune"])
#				#tab <- cbind(vals1[idx],data[idx1,c("Code departement","Code Insee commune")],vals2[idx],data[idx2,c("Code departement","Code Insee commune")])
#				count <- nrow(tab)
#				
#				# record the table of problematic cases
#				tab.file <- file.path(out.folder,paste0(BASENAMES[col],"_problems_article.txt"))
#				tlog(2,"Recording in file \"",tab.file,"\"")
#				write.table(x=tab,file=tab.file,
##					fileEncoding="UTF-8",
#					row.names=FALSE, col.names=TRUE)
#			}
#		}
#
#		tlog(4,"Found a total of ",count," pairs of problematic location names in column \"",col,"\"")
#	}
#}




#############################################################################################
# Performs a series of test on the names of municipalities.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.municipality <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in municipality names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_COM_CODE %in% cols && COL_ATT_COM_NOM %in% cols))
		tlog(2,"The municipality name and/or code is missing from the table >> cannot check anything")
	else if(!(COL_ATT_DPT_CODE %in% cols))
		tlog(2,"Municipality info present but department info missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_COM_NOM]
		codes <- paste(data[,COL_ATT_DPT_CODE],data[,COL_ATT_COM_CODE],sep=":")
		
		# init result table
		tab <- matrix(NA,ncol=3,nrow=0)
		colnames(tab) <- c(COL_ATT_DPT_CODE, COL_ATT_COM_CODE, "Noms de la commune")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing municipality ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(
					strsplit(x=unique.code, split=":", fixed=TRUE)[[1]],
					paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
			
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_COM_NOM],"_problems_id.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic municipality names")
	}
}




#############################################################################################
# Performs a series of test on the names of cantons.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.canton <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in canton names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_CANT_CODE %in% cols && COL_ATT_CANT_NOM %in% cols))
		tlog(2,"The canton name and/or code is missing from the table >> cannot check anything")
	else if(!(COL_ATT_DPT_CODE %in% cols))
		tlog(2,"Canton info present but department info missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_CANT_NOM]
		codes <- paste(data[,COL_ATT_DPT_CODE],data[,COL_ATT_CANT_CODE],sep=":")
		
		# init result table
		tab <- matrix(NA,ncol=3,nrow=0)
		colnames(tab) <- c(COL_ATT_DPT_CODE, COL_ATT_CANT_CODE, "Noms du canton")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing canton ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(
						strsplit(x=unique.code, split=":", fixed=TRUE)[[1]],
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
		
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_CANT_NOM],"_problems_id.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic canton names")
	}
}




#############################################################################################
# Performs a series of test on the names of legislative circonscriptions.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.legcirco <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in legislative circonscription names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_CIRC_CODE %in% cols && COL_ATT_CIRC_NOM %in% cols))
		tlog(2,"The circonscription name and/or code is missing from the table >> cannot check anything")
	else if(!(COL_ATT_DPT_CODE %in% cols))
		tlog(2,"Circonscription info present but department info missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_CIRC_NOM]
		codes <- paste(data[,COL_ATT_DPT_CODE],data[,COL_ATT_CIRC_CODE],sep=":")
		
		# init result table
		tab <- matrix(NA,ncol=3,nrow=0)
		colnames(tab) <- c(COL_ATT_DPT_CODE, COL_ATT_CIRC_CODE, "Noms de la circonscription")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing circonscription ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(
						strsplit(x=unique.code, split=":", fixed=TRUE)[[1]],
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
		
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_CIRC_NOM],"_problems_id.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic legislative circonscription names")
	}
}




#############################################################################################
# Performs a series of test on the names of European circonscriptions.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.eurocirco <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in European circonscription names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_CIRCE_CODE %in% cols && COL_ATT_CIRCE_NOM %in% cols))
		tlog(2,"The circonscription name and/or code is missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_CIRCE_NOM]
		codes <- data[,COL_ATT_CIRCE_CODE]
		
		# init result table
		tab <- matrix(NA,ncol=2,nrow=0)
		colnames(tab) <- c(COL_ATT_CIRCE_CODE, "Noms de la cirsconscription")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing circonscription ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(unique.code,
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
		
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_CIRCE_NOM],"_problems_id.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic European circonscription names")
	}
}




#############################################################################################
# Performs a series of test on the names of EPCIs.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.epci <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in EPCI names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_EPCI_SIREN %in% cols && COL_ATT_EPCI_NOM %in% cols))
		tlog(2,"The EPCI name and/or code is missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_EPCI_NOM]
		codes <- data[,COL_ATT_EPCI_SIREN]
		
		# init result table
		tab <- matrix(NA,ncol=2,nrow=0)
		colnames(tab) <- c(COL_ATT_EPCI_SIREN, "Noms de l'EPCI")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing EPCI ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(
						unique.code,
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
		
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_EPCI_NOM],"_problems_siren.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic EPCI names")
	}
}




#############################################################################################
# Performs a series of test on the names of EPCIs.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.department <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in department names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_DPT_CODE %in% cols && COL_ATT_DPT_NOM %in% cols))
		tlog(2,"The department name and/or code is missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_DPT_NOM]
		codes <- data[,COL_ATT_DPT_CODE]
		
		# init result table
		tab <- matrix(NA,ncol=2,nrow=0)
		colnames(tab) <- c(COL_ATT_DPT_CODE, "Noms du department")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing department ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(
						unique.code,
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
		
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_DPT_NOM],"_problems_id.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic department names")
	}
}




#############################################################################################
# Performs a series of test on the names of EPCIs.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations.region <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in region names")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_REG_CODE %in% cols && COL_ATT_REG_NOM %in% cols))
		tlog(2,"The region name and/or code is missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_REG_NOM]
		codes <- data[,COL_ATT_REG_CODE]
		
		# init result table
		tab <- matrix(NA,ncol=2,nrow=0)
		colnames(tab) <- c(COL_ATT_REG_CODE, "Noms de la region")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing region ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(
						unique.code,
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab <- rbind(tab, row)
			}
		}
		
		# possibly record the table
		if(nrow(tab)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_REG_NOM],"_problems_id.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab,file=tab.file,
#					fileEncoding="UTF-8",
					row.names=FALSE, col.names=TRUE)
		}
		
		tlog(4,"Found a total of ",nrow(tab)," problematic region names")
	}
}




#############################################################################################
# General function to check location-related data.
#
# data: table containing the senatorial data.
# out.folder: folder where to output the results.
#############################################################################################
test.col.locations <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in location names")
	
	# checks whether the same name appears with and without article
#	test.locations.articles(data, out.folder)
	
	# check municipality names
	test.col.locations.municipality(data, out.folder)
	
	# check canton names
	test.col.locations.canton(data, out.folder)
	
	# check legislative circonscription names
	test.col.locations.legcirco(data, out.folder)
	
	# check european circonscription names
	test.col.locations.eurocirco(data, out.folder)
	
	# check EPCI names
	test.col.locations.epci(data, out.folder)
	
	# check department names
	test.col.locations.department(data, out.folder)
	
	# check region names
	test.col.locations.region(data, out.folder)
}
