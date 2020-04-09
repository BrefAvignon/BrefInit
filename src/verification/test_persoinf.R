#############################################################################################
# Functions performing various tests on personal information (check that the birthdate is
# always the sale, etc.).
# 
# 01/2020 Vincent Labatut
#
# source("src/verification/test_persoinf.R")
#############################################################################################




#############################################################################################
# Checks whether the value associated to a personal id is always the same, taking into account
# the last name, first name, sex, birthday and nationality.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.personal.details <- function(data, out.folder)
{	# columns to check
	cols <- c(
		COL_ATT_ELU_NOM,
		COL_ATT_ELU_PRENOM,
		COL_ATT_ELU_SEXE,
		COL_ATT_ELU_NAIS_DATE,
		COL_ATT_ELU_NAT
	)
	tlog(2,"Trying to detect problems in personal information columns: ",paste(cols,collapse="\",\""))
	
	# get all unique ids
	tlog(4,"Retrieving all unique ids appearing several times in the table")
	tt <- table(data[,COL_ATT_ELU_ID])
	idx <- which(tt>1)
	ids <- names(tt)[idx]
	tlog(6,"Done: found ",length(ids)," ids")
	
	# compare the associated values for the specified columns
	tlog(4,"Comparing the values for each specified column")
	issues <- matrix(NA,nrow=length(ids),ncol=length(cols))
	colnames(issues) <- cols
	issues <- t(future_sapply(ids, function(id)
			{	idx <- which(data[,COL_ATT_ELU_ID]==id)
				res <- sapply(cols, function(col)
					{	vals <- data[idx,col]
						res <- !all(vals[1]==vals)
						return(res)
					})
				return(res)
			}))
	
	tlog(4,"Recording the detected issues")
	for(col in cols)
	{	col.ids <- ids[which(issues[,col])]
		tlog(6,"Processing column \"",col,"\": detected ",length(col.ids)," issues")
		
		if(length(col.ids)>0)
		{	col.basename <- BASENAMES[col]
			idx <- which(!is.na(match(data[,COL_ATT_ELU_ID], col.ids)))
			tmp <- cbind(idx, data[idx,])
			
			# record in a specific text file
			tab.file <- file.path(out.folder,paste0(col.basename,"_problems_same_id_diff_val.txt"))
			tlog(8,"Recording in file \"",tab.file,"\"")
			write.table(x=tmp, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
	}
	
	tlog(4,"Done with the personal info")
}




#############################################################################################
# Performs a series of tests on the names and codes of jobs/occupations.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.occupation.col <- function(data, out.folder)
{	tlog(0,"Trying to detect problems in occupation names and codes")
	
	# check if the necessary columns are present in the table
	cols <- colnames(data)
	if(!(COL_ATT_PRO_CODE %in% cols && COL_ATT_PRO_NOM %in% cols))
		tlog(2,"The occupation name and/or code is missing from the table >> cannot check anything")
	
	else
	{	# get unique names and codes
		names <- data[,COL_ATT_PRO_NOM]
		codes <- data[,COL_ATT_PRO_CODE]
		
		# init result table
		tab1 <- matrix(NA,ncol=2,nrow=0)
		colnames(tab1) <- c(COL_ATT_PRO_CODE, "Noms de la profession")
		
		# check that each code is associated to a unique name
		unique.codes <- sort(unique(codes[!is.na(names)]))
		for(i in 1:length(unique.codes))
		{	unique.code <- unique.codes[i]
			tlog(2,"Processing occupation ",unique.code," (",i,"/",length(unique.codes),")")
			
			idx <- which(codes==unique.code)
			if(length(idx)>0)
			{	ns <- names[idx]
				if(any(is.na(ns) | ns!=ns[1]))
				{	row <- c(unique.code,
							paste(sort(unique(ns)),collapse=",")
					)
#					print(row)
					tab1 <- rbind(tab1, row)
				}
			}
		}
		
		# found codes associated to multiple names
		if(nrow(tab1)>0)
		{	tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_PRO_CODE],"_problems_multiple_names.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab1, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		tlog(4,"Found a total of ",nrow(tab1)," problematic occupation names")
		
		# check if some codes have no name
		idx <- which(!is.na(data[,COL_ATT_PRO_CODE]) & is.na(data[,COL_ATT_PRO_NOM]))
		if(length(idx)==0)
			tlog(2,"Did not find an occupation code without an associated name")
		else
		{	tlog(2,"Found ",length(idx)," occupation codes without an associated name")
			tab <- cbind(idx, data[idx,])
			colnames(tab)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_PRO_CODE],"_problems_nameless.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab1, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		
		# init second result table
		tab2 <- matrix(NA,ncol=2,nrow=0)
		colnames(tab2) <- c(COL_ATT_PRO_NOM, "Codes de la profession")
		
		# check that each name is associated to a unique code
		unique.names <- sort(unique(names[!is.na(codes)]))
		for(i in 1:length(unique.names))
		{	unique.name <- unique.names[i]
			tlog(2,"Processing occupation ",unique.name," (",i,"/",length(unique.names),")")
			
			idx <- which(names==unique.name)
			if(length(idx)>0)
			{	cs <- codes[idx]
				if(any(is.na(cs) | cs!=cs[1]))
				{	row <- c(
						unique.name,
						paste(sort(unique(cs)),collapse=",")
					)
#					print(row)
					tab2 <- rbind(tab2, row)
				}
			}
		}
		
		# found names associated to multiple codes
		if(nrow(tab2)>0)
		{	# record the table listing them
			tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_PRO_NOM],"_problems_multiple_codes.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab2,file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
		tlog(4,"Found a total of ",nrow(tab2)," names with several distinct codes")
		
		# check if some names have no code
		idx <- which(is.na(data[,COL_ATT_PRO_CODE]) & !is.na(data[,COL_ATT_PRO_NOM]))
		if(length(idx)==0)
			tlog(2,"Did not find an occupation name without an associated code")
		else
		{	tlog(2,"Found ",length(idx)," occupation names without an associated code")
			tab <- cbind(idx, data[idx,])
			colnames(tab)[1] <- "Ligne"
			tab.file <- file.path(out.folder,paste0(BASENAMES[COL_ATT_PRO_CODE],"_problems_codeless.txt"))
			tlog(2,"Recording in file \"",tab.file,"\"")
			write.table(x=tab1, file=tab.file,
#				fileEncoding="UTF-8",
				row.names=FALSE, 
				col.names=TRUE,
#				quote=TRUE,
				sep="\t"
			)
		}
	}
}




#############################################################################################
# Compares approximately the names of the persons in the table, and list the closest ones
# to allow a manual verification (whether the names should be the same person or not). Exact
# matches are not listed, they are already dealt with by another function.
# 
# data: data table.
#############################################################################################
compare.person.names <- function(data)
{	VERBOSE <- FALSE
	tlog(0,"Looking for very similar last/first name, in order to detect typos in names")
	
	# concatenate last and first names
	tlog(2,"Retrieving all unique last/first name pairs")
	fullnames <- apply(as.matrix(data[,c(COL_ATT_ELU_NOM,COL_ATT_ELU_PRENOM)]), 1, function(r) paste(r,collapse=" "))
	tlog(4,"Found ",length(fullnames)," of them")
	# keep only one occurrence of each
	unique.names <- unique(fullnames)
	tlog(4,"Keep ",length(unique.names)," unique first/last names")
	
	tlog.start.loop(2,(length(unique.names)-1),"Processing each unique name separately")
	for(i in 1:(length(unique.names)-1))
	{	if(VERBOSE) tlog.loop(4,i,"Processing name \"",unique.names[i],"\" (",i,"/",(length(unique.names)-1),")")
		
		# compute string distance
		if(VERBOSE) tlog(6,"Computing distance to all remaining names")
		dd <- stringdist(a=unique.names[i],b=unique.names[(i+1):length(unique.names)],method="osa") # osa lv lcs
		mm <- min(dd)
		if(mm<=2)
		{	idx <- which(dd==mm) + i
			dpt.lst <- list()
			nme.lst <- list()
			
			# possibly filter using the department
			if(COL_ATT_DPT_CODE %in% colnames(data))
			{	# get the department for the current name
				rows1 <- which(fullnames==unique.names[i])
				dpts1 <- unique(data[rows1,COL_ATT_DPT_CODE])
				# init lists
				dpt.lst0 <- list()
				nme.lst0 <- list()
				for(dpt in dpts1)
				{	dpt.lst0[[as.character(dpt)]] <- c(dpt.lst0[[as.character(dpt)]], rows1[which(data[rows1,COL_ATT_DPT_CODE]==dpt)])
					nme.lst0[[as.character(dpt)]] <- union(nme.lst0[[as.character(dpt)]], unique.names[i])
				}
				# complete with rows containing similar name and same dpt
				for(j in idx)
				{	rows2 <- which(fullnames==unique.names[j])
					dpts2 <- unique(data[rows2,COL_ATT_DPT_CODE])
					com.dpts <- intersect(dpts1, dpts2)
					# add to the list
					for(dpt in com.dpts)
					{	dpt.lst[[as.character(dpt)]] <- c(dpt.lst0[[as.character(dpt)]], rows2[which(data[rows2,COL_ATT_DPT_CODE]==dpt)])
						nme.lst[[as.character(dpt)]] <- union(nme.lst0[[as.character(dpt)]], unique.names[j])
					}
				}
			}
			# no department in the table
			else if(length(idx)>0)
			{	dpt.lst[[1]] <- rows1
				nme.lst[[1]] <- union(unique.names[i], unique.names[idx])
			}
			
			# log results
			if(length(dpt.lst)>0)
			{	if(!VERBOSE) tlog.loop(4,i,"Processing name \"",unique.names[i],"\" (",i,"/",(length(unique.names)-1),")")
				for(k in 1:length(dpt.lst))
				{	if(length(dpt.lst)>1)
						tlog(6,"Group of similar names ",k,"/",length(dpt.lst))
					# display the names
					tlog(8,"Found ",(length(nme.lst[[k]])-1)," close names:")
					for(name in nme.lst[[k]])
						tlog(10,name)
					# display the rows
					tlog(8,"Corresponding rows:")
					for(r in dpt.lst[[k]])
						tlog(10, format.row(data[r,]))
				}
			}
			# no department match
			else
			{	if(VERBOSE) tlog(6,"Found some close names, but different department")
			}
		}
		# no match
		else
		{	if(VERBOSE) tlog(6,"No other name is close enough")
		}
		
	}
}




#############################################################################################
# Checks the columns related to personal information: first name, last name, birthdate, nationality,
# sex, occupation.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.personal.info <- function(data, out.folder)
{	# check personal info
	test.personal.details(data, out.folder)
	
	# check the occupation column
	test.occupation.col(data, out.folder)
	
	# approximately compare names
	compare.person.names(data)
}
