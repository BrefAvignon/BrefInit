#############################################################################################
# Compares the mayoral and municipal tables, trying to check whether the former is included
# in the latter, as we suppose (spoiler: we were right). 
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
# source("src/merge/merge_municipal.R")
#############################################################################################




#############################################################################################
# Compare two tables containing similar information. Compares the person id and the 
# mandate dates with a 1 week tolerance. If ovelap is true, the function just checks if
# the mandates and functions overlap, instead.
#
# data1: first table to compare.
# data2: municipal table (CM).
# log.details: whether or not to log the details of the processing.
# overlap: if TRUE, the test constraints are more relaxed.
#
# returns: matches of the first table in the second one.
#############################################################################################
match.similar.tables <- function(data1, data2, log.details=FALSE, overlap=FALSE)
{	tlog(0,"Matching compatible rows")
	
	# identify compatible rows against redundant ones
	tlog(2,"Matching ",nrow(data1)," rows vs. ",nrow(data2)," rows")
	result <- future_sapply(1:nrow(data1), function(r1)
	{	res <- NA
		if(log.details) tlog(4,"Processing row ",r1,"/",nrow(data1))
		if(log.details) tlog(4, format.row(data1[r1,]))
		
		rs <- which(data2[,COL_ATT_ELU_ID]==data1[r1,COL_ATT_ELU_ID])
		if(length(rs)>0)
		{	j <- 1
			while(is.na(res) && j<=length(rs))
			{	r2 <- rs[j]
				if(log.details) tlog(6,"Comparing to row ",r2,"(",j,"/",length(rs),")")
				if(log.details) tlog(6, format.row(data2[r2,]))
				
				if((!overlap 
					&& abs(data1[r1,COL_ATT_MDT_DBT]-data2[r2,COL_ATT_MDT_DBT])<=7
					&& (is.na(data1[r1,COL_ATT_MDT_FIN]) && is.na(data2[r2,COL_ATT_MDT_FIN]) 
						|| (!is.na(data1[r1,COL_ATT_MDT_FIN]) && !is.na(data2[r2,COL_ATT_MDT_FIN])
							&& abs(data1[r1,COL_ATT_MDT_FIN]-data2[r2,COL_ATT_MDT_FIN])<=7)))
				|| (overlap 
					&& date.intersect(start1=data1[r1,COL_ATT_MDT_DBT], end1=data1[r1,COL_ATT_MDT_FIN], 
							start2=data2[r2,COL_ATT_MDT_DBT], end2=data2[r2,COL_ATT_MDT_FIN])
					&& date.intersect(start1=data1[r1,COL_ATT_FCT_DBT], end1=data1[r1,COL_ATT_FCT_FIN], 
							start2=data2[r2,COL_ATT_FCT_DBT], end2=data2[r2,COL_ATT_FCT_FIN])
					)
				)
				{	if(log.details) tlog(6, "Found a match:")
					if(log.details) tlog(6, format.row(data1[r1,]))
					if(log.details) tlog(6, format.row(data2[r2,]))
					res <- r2
				}
				else
					j <- j + 1
			}
		}
		return(res)
	})
	
	return(result)
}




#############################################################################################
# Check if the rows taken pairwise in both tables are compatible (i.e. first row of table1 
# compatible with first row of table2, and so on). Compatibility here means that they have either
# the same value or at least one is NA, for each field excluding mandate and function dates,
# which must overlap.
#
# table1: first table to compare.
# table2: second table. They must have the same dimensions.
#
# returns: a vector indicating whether each pair of rows is compatible.
#############################################################################################
check.compatibility <- function(table1, table2)
{	atts <- setdiff(colnames(table1), c(COL_ATT_MDT_DBT, COL_ATT_MDT_FIN, COL_ATT_FCT_DBT, COL_ATT_FCT_FIN, COL_ATT_CORREC_DATE, COL_ATT_CORREC_INFO))
	
	result <- future_sapply(1:nrow(table1), function(r)
	{	# debug
		#if(!all(is.na(table1[r,atts]) | is.na(table2[r,atts]) | table1[r,atts]==table2[r,atts]))
		#{	idx <- which(!(is.na(table1[r,atts]) | is.na(table2[r,atts]) | table1[r,atts]==table2[r,atts]))
		#	print(colnames(table1)[idx])
		#}
		#
		(all(is.na(table1[r,atts]) | is.na(table2[r,atts]) | table1[r,atts]==table2[r,atts])
			&& date.intersect(start1=table1[r,COL_ATT_MDT_DBT], end1=table1[r,COL_ATT_MDT_FIN], 
					start2=table2[r,COL_ATT_MDT_DBT], end2=table2[r,COL_ATT_MDT_FIN])
			&& date.intersect(start1=table1[r,COL_ATT_FCT_DBT], end1=table1[r,COL_ATT_FCT_FIN], 
					start2=table2[r,COL_ATT_FCT_DBT], end2=table2[r,COL_ATT_FCT_FIN])
		)
	})
	return(result)
}




#############################################################################################
# Gets the table containing the mayoral data and the one containing the municipal data. Extracts
# the mayoral rows from the latter, and merge them with the ones from the former. Puts them
# back in the municipal table. Returns the resulting table.
#
# m.data: mayoral table.
# cm.data: municipal table.
#
# returns: revised municipal table.
#############################################################################################
merge.municipal <- function(m.data, cm.data)
{	tlog(0,"Merging M and CM tables")
	
	# keep only the mayors from the municipal table
	cm.idx <- which(cm.data[,COL_ATT_FCT_NOM]=="MAIRE")
	tlog(2,"Keeping only the mayors, i.e. ", length(cm.idx)," rows")
	result <- cm.data[-cm.idx,]
	cm.data <- cm.data[cm.idx,]
	tlog(2,"Comparing ",nrow(m.data)," (M) vs. ",nrow(cm.data), " (CM)")
	
	# compare rows exactly
	tlog(2,"Performing exact comparison")
	sel.cols <- c(
		COL_ATT_ELU_ID,
#		COL_ATT_ELU_NOM,
#		COL_ATT_ELU_PRENOM,
#		COL_ATT_ELU_NAIS_DATE,
#		COL_ATT_ELU_SEXE,
		COL_ATT_MDT_DBT,
		COL_ATT_MDT_FIN
	)
	cm.codes <- future_apply(cm.data[,sel.cols],1,function(r) paste(r,collapse=":"))
	m.codes <- future_apply(m.data[,sel.cols],1,function(r) paste(r,collapse=":"))
	# look for all mayors in the municipal data
	m.map <- match(m.codes,cm.codes)
	m.unmatched <- which(is.na(m.map))
	tlog(4,"Number of CM rows appearing several times: ",which(table(m.unmatched)>1))
	# and the opposite
	cm.map <- match(cm.codes,m.codes)
	cm.unmatched <- which(is.na(cm.map))
	tlog(4,"Number of M rows appearing several times: ",which(table(cm.unmatched)>1))
	# display result
	tlog(4,"Found ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
	#print(m.codes[m.unmatched])
	
	# compare remaining rows in a more flexible way (dates)
	tlog(2,"Comparing the remaining rows in a more flexible way (tolerance on date)")
	m.map[m.unmatched] <- match.similar.tables(m.data[m.unmatched,], cm.data)
	m.newmatches <- m.unmatched[which(!is.na(m.map[m.unmatched]))]
	m.unmatched <- which(is.na(m.map))
	cm.map[cm.unmatched] <- match.similar.tables(cm.data[cm.unmatched,], m.data)
	cm.newmatches <- cm.unmatched[which(!is.na(cm.map[cm.unmatched]))]
	cm.unmatched <- which(is.na(cm.map))
	tlog(4,"Now ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
	# check if matched rows are compatible
	#m.incompat <- m.newmatches[which(!check.compatibility(m.data[m.newmatches,], cm.data[m.map[m.newmatches],]))]
	#cm.incompat <- cm.newmatches[which(!check.compatibility(cm.data[cm.newmatches,], m.data[cm.map[cm.newmatches],]))]
	#tlog(2,"Found ",length(m.incompat),"/",length(m.newmatches)," incompatible rows among the matched ones for M and ",length(cm.incompat),"/",length(cm.newmatches)," for CM")
	#print(rbind(m.data[m.incompat[1],], cm.data[m.map[m.incompat[1]],]))
	
	# compare remaining rows in a more flexible way (dates)
	tlog(2,"Comparing the remaining rows in an even more flexible way (period interesection)")
	m.map[m.unmatched] <- match.similar.tables(m.data[m.unmatched,], cm.data, overlap=TRUE)
	m.newmatches <- m.unmatched[which(!is.na(m.map[m.unmatched]))]
	m.unmatched <- which(is.na(m.map))
	cm.map[cm.unmatched] <- match.similar.tables(cm.data[cm.unmatched,], m.data, overlap=TRUE)
	cm.newmatches <- cm.unmatched[which(!is.na(cm.map[cm.unmatched]))]
	cm.unmatched <- which(is.na(cm.map))
	tlog(4,"Now ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
	# display problems
	#i=3;print(rbind(m.data[m.unmatched[i],], cm.data[which(cm.data[,COL_ATT_ELU_ID]==m.data[m.unmatched[i],COL_ATT_ELU_ID]),]))
	#i=1;print(rbind(cm.data[cm.unmatched[i],], m.data[which(m.data[,COL_ATT_ELU_ID]==cm.data[cm.unmatched[i],COL_ATT_ELU_ID]),]))
	
	## check multiple matches (is the same row mapped several times?)
	#m.tt <- table(m.map)
	#m.rs <- as.integer(names(m.tt[which(m.tt>1)]))
	##r=1;print(rbind(m.data[which(m.map==m.rs[r]),],cm.data[m.rs[r],]))
	#
	#cm.tt <- table(cm.map)
	#cm.rs <- as.integer(names(cm.tt[which(cm.tt>1)]))
	##r=1;print(rbind(cm.data[which(cm.map==cm.rs[r]),],m.data[cm.rs[r],]))

	# matching over, merging the rows
	tlog.start.loop(2, nrow(m.data), "Merging the rows of both tables based on the matches")
	res <- m.data[NULL,]
	for(r in 1:nrow(m.data))
	{	tlog.loop(4, r, "Merging rows ",r,"/",nrow(m.data))
		tab <- rbind(m.data[r,], cm.data[m.map[r],])
		tab <- merge.overlapping.mandates(data=tab, type="M", strict=FALSE, log=FALSE)
		res <- rbind(res,tab)
	}
	tlog.end.loop(4, "Process over")
	
	# set up the result table
	result <- rbind(result, res)
	return(result)
}
