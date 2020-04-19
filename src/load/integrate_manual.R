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
{	# load the supplementary data
	supp.data <- retrieve.normalize.data(filenames, correct.data=FALSE)
	
	# fix id duplicates and other id-related issues
	supp.data <- fix.id.problems(supp.data)
		
	# apply systematic corrections
	supp.data <- apply.systematic.corrections(supp.data, type="DE")
	
	# convert date and numeric columns
	supp.data <- convert.col.types(supp.data)
	
	# add data source column
	src.col <- data.frame(rep("WP",nrow(supp.data)), stringsAsFactors=FALSE)
	supp.data <- cbind(supp.data, src.col)
	colnames(supp.data)[ncol(supp.data)] <- COL_ATT_SOURCES
	
	# add EP ID to data table
	data <- cbind(data, rep(NA,nrow(data)))
	colnames(data)[ncol(data)] <- COL_ATT_ELU_ID_EURO
	
	# normalize columns order
	supp.data <- normalize.col.order(supp.data)
	data <- normalize.col.order(data)
	
	# merge both tables
	data <- rbind(data, supp.data)
	
	# order the resulting table
	idx <- order(data[,COL_ATT_ELU_NOM], data[,COL_ATT_ELU_PRENOM], data[,COL_ATT_ELU_ID],
		data[,COL_ATT_MDT_DBT], data[,COL_ATT_MDT_FIN], data[,COL_ATT_FCT_DBT], data[,COL_ATT_FCT_FIN])
	data <- data[idx,]
	
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
