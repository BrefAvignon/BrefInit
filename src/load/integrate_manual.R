#############################################################################################
# Functions used to inject data from some manually constitued table.
# 
# 04/2020 Vincent Labatut
#
# source("src/load/integrate_manual.R")
#############################################################################################




#############################################################################################
# Add supplementary data to the CD table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.cd <- function(data)
{	tlog(0,"Integrating the manually constituted CD table")
	
	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames=FILE_SUPPL_CD, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
	
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="CD")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add missing columns
	supp.data <- add.missing.columns(supp.data)
	
	# add data source column
	supp.data[,COL_ATT_SOURCES] <- rep("WIKIPEDIA", nrow(supp.data))
	
	# complete missing occupation based on RNE
	tlog(2,"Completing missing occupation info in supplementary table")
	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & is.na(supp.data[,COL_ATT_PRO_CODE]))
	ids <- unique(supp.data[idx,COL_ATT_ELU_ID])
	idx0 <- match(ids, data[,COL_ATT_ELU_ID])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	occ.codes <- data[idx0,COL_ATT_PRO_CODE]
	occ.names <- data[idx0,COL_ATT_PRO_NOM]
	for(i in 1:length(ids))
	{	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & supp.data[,COL_ATT_ELU_ID]==ids[i] & is.na(supp.data[,COL_ATT_PRO_CODE]))
		supp.data[idx,COL_ATT_PRO_CODE] <- occ.codes[i]
		supp.data[idx,COL_ATT_PRO_NOM] <- occ.names[i]
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
#	tlog(2,"Ordering merged table")
#	idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
#			data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN])
#	data <- data[idx,]
	
	tlog(2,"CHECKPOINT 16: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=added.nbr, size=nbr.before)
	return(data)
}




#############################################################################################
# Add supplementary data to the CM table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.cm <- function(data)
{	tlog(0,"Integrating the manually constituted CM table")
	
	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames=FILE_SUPPL_CM, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
	
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="CM")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add missing columns
	supp.data <- add.missing.columns(supp.data)
	
	# add data source column
	supp.data[,COL_ATT_SOURCES] <- rep("WIKIPEDIA", nrow(supp.data))
	
	# add mandate name column
	supp.data <- cbind(supp.data, rep("CONSEILLER MUNICIPAL", nrow(supp.data)))
	colnames(supp.data)[ncol(supp.data)] <- COL_ATT_MDT_NOM
	
	# complete missing occupation based on RNE
	tlog(2,"Completing missing occupation info in supplementary table")
	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & is.na(supp.data[,COL_ATT_PRO_CODE]))
	ids <- unique(supp.data[idx,COL_ATT_ELU_ID])
	idx0 <- match(ids, data[,COL_ATT_ELU_ID])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	occ.codes <- data[idx0,COL_ATT_PRO_CODE]
	occ.names <- data[idx0,COL_ATT_PRO_NOM]
	for(i in 1:length(ids))
	{	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & supp.data[,COL_ATT_ELU_ID]==ids[i] & is.na(supp.data[,COL_ATT_PRO_CODE]))
		supp.data[idx,COL_ATT_PRO_CODE] <- occ.codes[i]
		supp.data[idx,COL_ATT_PRO_NOM] <- occ.names[i]
	}
	
	# normalize columns order
	tlog(2,"Normalizing column order in both tables")
	supp.data <- normalize.col.order(supp.data)
	data <- normalize.col.order(data)
print(colnames(supp.data))
print(colnames(data))
	
	# merge both tables
	tlog(2,"Merging both tables")
	added.nbr <- nrow(supp.data)
	nbr.before <- nrow(data)
	data <- rbind(data, supp.data)
	rownames(data) <- NULL
	
	# order the resulting table
#	tlog(2,"Ordering merged table")
#	idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
#			data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN])
#	data <- data[idx,]
	
	tlog(2,"CHECKPOINT 16: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=added.nbr, size=nbr.before)
	return(data)
}




#############################################################################################
# Add supplementary data to the CR table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.cr <- function(data)
{	tlog(0,"Integrating the manually constituted CR table")
	
	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames=FILE_SUPPL_CR, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
	
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="CR")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add missing columns
	supp.data <- add.missing.columns(supp.data)
	
	# add data source column
	supp.data[,COL_ATT_SOURCES] <- rep("WIKIPEDIA", nrow(supp.data))
	
	# complete missing occupation based on RNE
	tlog(2,"Completing missing occupation info in supplementary table")
	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & is.na(supp.data[,COL_ATT_PRO_CODE]))
	ids <- unique(supp.data[idx,COL_ATT_ELU_ID])
	idx0 <- match(ids, data[,COL_ATT_ELU_ID])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	occ.codes <- data[idx0,COL_ATT_PRO_CODE]
	occ.names <- data[idx0,COL_ATT_PRO_NOM]
	for(i in 1:length(ids))
	{	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & supp.data[,COL_ATT_ELU_ID]==ids[i] & is.na(supp.data[,COL_ATT_PRO_CODE]))
		supp.data[idx,COL_ATT_PRO_CODE] <- occ.codes[i]
		supp.data[idx,COL_ATT_PRO_NOM] <- occ.names[i]
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
#	tlog(2,"Ordering merged table")
#	idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
#			data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN])
#	data <- data[idx,]
	
	tlog(2,"CHECKPOINT 16: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=added.nbr, size=nbr.before)
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
{	tlog(0,"Integrating the manually constituted D table")
	
	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames=FILE_SUPPL_D, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
	
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="D")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add missing columns
	supp.data <- add.missing.columns(supp.data)
	
	# set data source column
	supp.data[,COL_ATT_SOURCES] <- rep("WIKIPEDIA", nrow(supp.data))
	
	# complete missing occupation based on RNE
	tlog(2,"Completing missing occupation info in supplementary table")
	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & is.na(supp.data[,COL_ATT_PRO_CODE]))
	ids <- unique(supp.data[idx,COL_ATT_ELU_ID])
	idx0 <- match(ids, data[,COL_ATT_ELU_ID])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	occ.codes <- data[idx0,COL_ATT_PRO_CODE]
	occ.names <- data[idx0,COL_ATT_PRO_NOM]
	for(i in 1:length(ids))
	{	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & supp.data[,COL_ATT_ELU_ID]==ids[i] & is.na(supp.data[,COL_ATT_PRO_CODE]))
		supp.data[idx,COL_ATT_PRO_CODE] <- occ.codes[i]
		supp.data[idx,COL_ATT_PRO_NOM] <- occ.names[i]
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
	
	tlog(2,"CHECKPOINT 16: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=added.nbr, size=nbr.before)
	return(data)
}




#############################################################################################
# Add supplementary data to the DE table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.de <- function(data)
{	tlog(0,"Integrating the manually constituted DE table")
	
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
	src.col <- data.frame(rep("EUROPARL",nrow(supp.data)), stringsAsFactors=FALSE)
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
	
	tlog(2,"CHECKPOINT 16: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=added.nbr, size=nbr.before)
	return(data)
}




#############################################################################################
# Add supplementary data to the M table. No verification of transformation is made here,
# as any error can be directly edited in the table manually constituted by ourselves.
#
# data: original table.
#
# returns: same table, with additional rows.
#############################################################################################
manual.integrate.data.m <- function(data)
{	tlog(0,"Integrating the manually constituted M table")
	
	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames=FILE_SUPPL_M, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
	
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="M")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add missing columns
	supp.data <- add.missing.columns(supp.data)
	
	# add data source column
	supp.data[,COL_ATT_SOURCES] <- rep("WIKIPEDIA", nrow(supp.data))
	
	# complete missing occupation based on RNE
	tlog(2,"Completing missing occupation info in supplementary table")
	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & is.na(supp.data[,COL_ATT_PRO_CODE]))
	ids <- unique(supp.data[idx,COL_ATT_ELU_ID])
	idx0 <- match(ids, data[,COL_ATT_ELU_ID])
	ids <- ids[!is.na(idx0)]
	idx0 <- idx0[!is.na(idx0)]
	occ.codes <- data[idx0,COL_ATT_PRO_CODE]
	occ.names <- data[idx0,COL_ATT_PRO_NOM]
	for(i in 1:length(ids))
	{	idx <- which(!is.na(supp.data[,COL_ATT_ELU_ID]) & supp.data[,COL_ATT_ELU_ID]==ids[i] & is.na(supp.data[,COL_ATT_PRO_CODE]))
		supp.data[idx,COL_ATT_PRO_CODE] <- occ.codes[i]
		supp.data[idx,COL_ATT_PRO_NOM] <- occ.names[i]
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
#	tlog(2,"Ordering merged table")
#	idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
#			data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN])
#	data <- data[idx,]
	
	tlog(2,"CHECKPOINT 16: added ",added.nbr," rows (",(100*added.nbr/nbr.before),"%)")
	tlog(2,"Now ",nrow(data)," rows and ",ncol(data)," columns in table")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=added.nbr, size=nbr.before)
	return(data)
}




#############################################################################################
# Loads the presidential data.
#
# returns: presidential table.
#############################################################################################
manual.integrate.data.prf <- function()
{	tlog(0,"Integrating the manually constituted PRF table")
	
	# load the data
	data <- retrieve.normalize.data(filenames=FILE_SUPPL_PRF, correct.data=FALSE)
	
	# apply systematic corrections
	data <- apply.systematic.corrections(data, type="PRF")
	
	# convert date and numeric columns
	data <- convert.col.types(data)
	
	# add data source column
	tlog(2,"Adding the source column to the table")
	src.col <- data.frame(rep("WIKIPEDIA",nrow(data)), stringsAsFactors=FALSE)
	data <- cbind(data, src.col)
	colnames(data)[ncol(data)] <- COL_ATT_SOURCES
	
	# normalize columns order
	tlog(2,"Normalizing column order")
	data <- normalize.col.order(data)
	
	tlog(2,"CHECKPOINT 16: loaded ",nrow(data)," rows and ",ncol(data)," columns")
	update.stat.table(s.nbr=16, s.name="Integrate secondary source", del.nbr=0, mod.nbr=0, add.nbr=nrow(data), size=0)
	return(data)
}
