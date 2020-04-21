#############################################################################################
# Functions used to inject data from some manually constitued table.
# 
# 04/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Add supplementary data to the DE table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.de <- function(data)
{	tlog(0,"Integrating the manually constituted table")
	
	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames=FILE_SUPPL_DE, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
		
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="DE")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add data source column
	tlog(2,"Adding the source column in the supplementary table")
	src.col <- data.frame(rep("WP",nrow(supp.data)), stringsAsFactors=FALSE)
	supp.data <- cbind(supp.data, src.col)
	colnames(supp.data)[ncol(supp.data)] <- COL_ATT_SOURCES
	
	# complete missing occupation based on RNE
	tlog(2,"Completing missing occupation info in supplementary table")
	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID_RNE]) & is.na(supp.data[,COL_ATT_PRO_CODE]))
	ids <- unique(supp.data[idx,COL_ATT_ELU_ID_RNE])
	idx0 <- match(ids, data[,COL_ATT_ELU_ID_RNE])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	occ.codes <- data[idx0,COL_ATT_PRO_CODE]
	occ.names <- data[idx0,COL_ATT_PRO_NOM]
	for(i in 1:length(ids))
	{	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID_RNE]) & supp.data[,COL_ATT_ELU_ID_RNE]==ids[i] & is.na(supp.data[,COL_ATT_PRO_CODE]))
		supp.data[idx,COL_ATT_PRO_CODE] <- occ.codes[i]
		supp.data[idx,COL_ATT_PRO_NOM] <- occ.names[i]
	}
	
	# complete missing EP ID to data table
	tlog(2,"Adding missing EP IDs in main table")
	idx <- which(is.na(data[,COL_ATT_ELU_ID_EURO]))
	ids <- unique(data[idx,COL_ATT_ELU_ID_RNE])
	idx0 <- match(ids, supp.data[,COL_ATT_ELU_ID_RNE])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	euro.id <- supp.data[idx0,COL_ATT_ELU_ID_EURO]
	for(i in 1:length(ids))
	{	idx <- which(data[,COL_ATT_ELU_ID_RNE]==ids[i] & is.na(data[,COL_ATT_ELU_ID_EURO]))
		data[idx,COL_ATT_ELU_ID_EURO] <- euro.id[i]
	}
	
	# normalize columns order
	tlog(2,"Normalizing column order in both tables")
	supp.data <- normalize.col.order(supp.data)
	data <- normalize.col.order(data)
	
	# merge both tables
	tlog(2,"Merging both tables")
	added.nbr <- nrow(supp.data)
	nbr.before <- nrow(data)
	data <- rbind(data, supp.data)
	rownames(data) <- NULL
	
	# order the resulting table
	tlog(2,"Ordering merged table")
	idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
		data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN])
	data <- data[idx,]
	
	tlog(2,"CHECKPOINT x: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	return(data)
}




#############################################################################################
# Add supplementary data to the D table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.d <- function(data)
{	# TODO
	
	return(data)
}
