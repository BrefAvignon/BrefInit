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
source("src/verification/sumup_col.R")




#############################################################################################
# start logging
start.rec.log(text="M_vs_CM")
tlog(0,"Comparing the mayoral and municipal data")
correct.data <- TRUE



#############################################################################################
# load the mayoral data
tlog(0,"Load mayoral data")
m.data <- load.m.data(correct.data)




#############################################################################################
# load the municipal data
tlog(0,"Load municipal data")
cm.data <- load.cm.data(correct.data)




#############################################################################################
# compare rows
sel.cols <- c(
	COL_ATT_ELU_ID,
	COL_ATT_ELU_NOM,
	COL_ATT_ELU_PRENOM,
	COL_ATT_ELU_NAIS_DATE,
	COL_ATT_ELU_SEXE,
	COL_ATT_MDT_DBT,
	COL_ATT_MDT_FIN
)
cm.codes <- apply(cm.data[,sel.cols],1,function(r) paste(r,collapse=":"))
m.codes <- apply(m.data[,sel.cols],1,function(r) paste(r,collapse=":"))
# look for all mayors in the municipal data
idx <- match(m.codes,cm.codes)
m.unmatched <- which(is.na(idx))
tlog(2,"Unmatched rows of M:")
print(m.codes[m.unmatched])




#############################################################################################
# look for unmatched persons
for(um in unmatched)
{	tlog(2, "Processing unmatched mayor")
	print(m.data[um,sel.cols])
	tlog(4, "Municipal councilors with the same name:")
	idx <- which(cm.data[,COL_ATT_ELU_NOM]==m.data[um,COL_ATT_ELU_NOM])
	print(cm.data[idx,sel.cols])
}




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
