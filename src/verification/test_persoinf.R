#############################################################################################
# Functions performing various tests on personal information (check that the birthdate is
# always the sale, etc.).
# 
# 01/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Checks whether the value associated to a personal id is always the same, for the specified
# columns.
#
# data: table containing the data.
# out.folder: folder where to output the results.
#############################################################################################
test.personal.info <- function(data, out.folder)
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
					row.names=FALSE, col.names=TRUE)
		}
	}
	
	tlog(4,"Done with the personal info")
}
