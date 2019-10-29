#############################################################################################
# Compares the mayoral and municipal tables. 
# 
# 10/2019 Vincent Labatut
#
# source("src/comparison/compare_M_CM.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/sumup_col.R")




#############################################################################################
# start logging
start.rec.log(text="M_vs_CM")
tlog(0,"Comparing the mayoral and municipal data")




#############################################################################################
# load the mayoral data
tlog(0,"Load mayoral data")
tmp <- load.m.data()
m.data <- tmp$data
m.cols <- tmp$cols




#############################################################################################
# load the municipal data
tlog(0,"Load municipal data")
tmp <- load.cm.data()
cm.data <- tmp$data
cm.cols <- tmp$cols




#############################################################################################
# look for all mayors in the municipal data
sel.cols <- c(
	"N° Identification d'un élu",
	"Nom de l'élu",
	"Prénom de l'élu",
	"Date de naissance",
	"Code sexe",
	"Date de début du mandat",
	"Date de fin du mandat"
)
cm.codes <- apply(cm.data[,sel.cols],1,function(r) paste(r,collapse=":"))
m.codes <- apply(m.data[,sel.cols],1,function(r) paste(r,collapse=":"))
idx <- match(m.codes,cm.codes)
unmatched <- which(is.na(idx))
tlog(2,"Unmatched row:")
print(m.codes[unmatched])




#############################################################################################
# look for unmatched persons
for(um in unmatched)
{	tlog(2, "Processing unmatched mayor")
	print(m.data[um,sel.cols])
	tlog(4, "Municipal councilors with the same name:")
	idx <- which(cm.data[,"Nom de l'élu"]==m.data[um,"Nom de l'élu"])
	print(cm.data[idx,sel.cols])
}




#############################################################################################
# close the log file
tlog(0,"Done")
end.rec.log()
