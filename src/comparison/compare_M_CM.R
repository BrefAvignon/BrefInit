#############################################################################################
# Compares the mayoral and municipal tables, trying to check whether the former is included
# in the latter, as we suppose (spoiler: we were right). 
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
# source("src/comparison/compare_M_CM.R")
#############################################################################################
source("src/common/include.R")
source("src/comparison/compare_tables.R")
source("src/verification/sumup_col.R")
source("src/verification/sumup_col.R")




#############################################################################################
# start logging
start.rec.log(text="M_vs_CM")
tlog(0,"Comparing the mayoral and municipal data")
correct.data <- TRUE
complete.data <- TRUE



#############################################################################################
# load the mayoral data
tlog(0,"Load mayoral data")
m.data <- load.m.data(out.folder=FOLDER_OUT_CM, correct.data, complete.data)




#############################################################################################
# load the municipal data
tlog(0,"Load municipal data")
cm.data <- load.cm.data(out.folder=FOLDER_OUT_M, correct.data, complete.data)
# keep only the mayors
cm.idx <- which(cm.data[,COL_ATT_FCT_NOM]=="MAIRE")
tlog(2,"Keeping only the mayors, i.e. ", length(cm.idx)," rows")
cm.data <- cm.data[cm.idx,]
tlog(0,"Comparing ",nrow(m.data)," (M) vs. ",nrow(cm.data), " (CM)")




#############################################################################################
# compare rows exactly
tlog(0,"Performing exact comparison")
sel.cols <- c(
	COL_ATT_ELU_ID,
#	COL_ATT_ELU_NOM,
#	COL_ATT_ELU_PRENOM,
#	COL_ATT_ELU_NAIS_DATE,
#	COL_ATT_ELU_SEXE,
	COL_ATT_MDT_DBT,
	COL_ATT_MDT_FIN
)
cm.codes <- future_apply(cm.data[,sel.cols],1,function(r) paste(r,collapse=":"))
m.codes <- future_apply(m.data[,sel.cols],1,function(r) paste(r,collapse=":"))
# look for all mayors in the municipal data
m.map <- match(m.codes,cm.codes)
m.unmatched <- which(is.na(m.map))
tlog(2,"Number of CM rows appearing several times: ",which(table(m.unmatched)>1))
# and the opposite
cm.map <- match(cm.codes,m.codes)
cm.unmatched <- which(is.na(cm.map))
tlog(2,"Number of M rows appearing several times: ",which(table(cm.unmatched)>1))
# remove matched rows from tables
tlog(2,"Found ",length(m.unmatched)," unmatched rows in M and ",length(cm.unmatched)," in CM")
m.data <- m.data[m.unmatched,]
cm.data <- cm.data[cm.unmatched,]
#print(m.codes[m.unmatched])




#############################################################################################
# compare remaining rows in a more flexible way (dates)
m.map <- match.similar.tables(m.data, cm.data)
m.matched <- which(!is.na(m.map))
m.unmatched <- which(is.na(m.map))
cm.map <- match.similar.tables(cm.data, m.data)
cm.matched <- which(!is.na(cm.map))
cm.unmatched <- which(is.na(cm.map))
tlog(2,"Found ",length(m.matched),"/",nrow(m.data)," matched rows in M and ",length(cm.matched),"/",nrow(cm.data)," in CM")
# check if matched rows are compatible
m.incompat <- which(!check.compatibility(m.data[m.matched,], cm.data[m.map[m.matched],]))
cm.incompat <- which(!check.compatibility(cm.data[cm.matched,], m.data[cm.map[cm.matched],]))
tlog(2,"Found ",length(m.incompat),"/",length(m.matched)," incompatible rows among the matched ones for M and ",length(cm.incompat),"/",length(cm.matched)," for CM")
print(rbind(m.data[m.matched[m.incompat[1]],], cm.data[m.map[m.matched[m.incompat[1]]],]))

# remove matched rows from tables
tlog(2,"Found ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
m.data <- m.data[m.unmatched,]
cm.data <- cm.data[cm.unmatched,]



#############################################################################################
# look for unmatched persons
x <- future_sapply(1:nrow(m.data), function(r1)
{	idx <- which(cm.data[,COL_ATT_ELU_ID]==m.data[r1,COL_ATT_ELU_ID])
	if(length(idx)>0)
	{	tlog(2, "Processing unmatched mayor ",m.data[r1,COL_ATT_ELU_ID])
		tlog(4, format.row(m.data[r1,]))
		tlog(4, "Municipal councilors with the same id:")
		for(r2 in idx)
			tlog(4, format.row(cm.data[r2,]))
	}
})




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
