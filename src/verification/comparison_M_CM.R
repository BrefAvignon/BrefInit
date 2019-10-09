#############################################################################################
# Compares the mayoral and municipal tables. 
# 
# 10/2019 Vincent Labatut
#
# source("src/verification/comparison_M_CM.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/sumup_col.R")

# start logging
start.rec.log(text="M_vs_CM")

# load the mayoral data
tmp <- load.m.data()
m.data <- tmp$data
m.cols <- tmp$cols

# load the municipal data
tmp <- load.cm.data()
cm.data <- tmp$data
cm.cols <- tmp$cols

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
cat("  Unmatched row:\n")
print(m.codes[unmatched])

# look for unmatched persons
for(um in unmatched)
{	cat("  Processing unmatched mayor\n")
	print(m.data[um,sel.cols])
	cat("  Municipal councilors with the same name:\n")
	idx <- which(cm.data[,"Nom de l'élu"]==m.data[um,"Nom de l'élu"])
	print(cm.data[idx,sel.cols])
}

# close the log file
tlog(0,"Done")
end.rec.log()
