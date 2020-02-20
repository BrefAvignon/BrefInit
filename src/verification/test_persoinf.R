#############################################################################################
# Functions performing various tests on personal information (check that the birthdate is
# always the sale, etc.).
# 
# 01/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Checks whether the value associated to a personal id is always the same, taking into account
# the last name, first name, sex, birthday and nationality.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.personal.details <- function(data, out.folder)
{	plan(multiprocess, workers=CORE.NBR/2)
	
	# columns to check
	cols <- c(
		COL_ATT_ELU_NOM,
		COL_ATT_ELU_PRENOM,
		COL_ATT_ELU_SEXE,
		COL_ATT_ELU_DDN,
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
#					fileEncoding="UTF-8",
					row.names=FALSE, 
					col.names=TRUE,
#					quote=TRUE,
					se="\t"
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
			ns <- names[idx]
			if(any(ns!=ns[1]))
			{	row <- c(unique.code,
						paste(sort(unique(ns)),collapse=",")
				)
#				print(row)
				tab1 <- rbind(tab1, row)
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
				se="\t"
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
				se="\t"
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
			cs <- codes[idx]
			if(any(cs!=cs[1]))
			{	row <- c(
					unique.name,
					paste(sort(unique(cs)),collapse=",")
				)
#				print(row)
				tab2 <- rbind(tab2, row)
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
				se="\t"
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
				se="\t"
			)
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
}
