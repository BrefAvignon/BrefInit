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
# display result
tlog(2,"Found ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
#print(m.codes[m.unmatched])




#############################################################################################
# compare remaining rows in a more flexible way (dates)
m.map[m.unmatched] <- match.similar.tables(m.data[m.unmatched,], cm.data)
m.newmatches <- m.unmatched[which(!is.na(m.map[m.unmatched]))]
m.unmatched <- which(is.na(m.map))
cm.map[cm.unmatched] <- match.similar.tables(cm.data[cm.unmatched,], m.data)
cm.newmatches <- cm.unmatched[which(!is.na(cm.map[cm.unmatched]))]
cm.unmatched <- which(is.na(cm.map))
tlog(2,"Now ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
# check if matched rows are compatible
#m.incompat <- m.newmatches[which(!check.compatibility(m.data[m.newmatches,], cm.data[m.map[m.newmatches],]))]
#cm.incompat <- cm.newmatches[which(!check.compatibility(cm.data[cm.newmatches,], m.data[cm.map[cm.newmatches],]))]
#tlog(2,"Found ",length(m.incompat),"/",length(m.newmatches)," incompatible rows among the matched ones for M and ",length(cm.incompat),"/",length(cm.newmatches)," for CM")
#print(rbind(m.data[m.incompat[1],], cm.data[m.map[m.incompat[1]],]))




#############################################################################################
# compare remaining rows in a more flexible way (dates)
m.map[m.unmatched] <- match.similar.tables(m.data[m.unmatched,], cm.data, overlap=TRUE)
m.newmatches <- m.unmatched[which(!is.na(m.map[m.unmatched]))]
m.unmatched <- which(is.na(m.map))
cm.map[cm.unmatched] <- match.similar.tables(cm.data[cm.unmatched,], m.data, overlap=TRUE)
cm.newmatches <- cm.unmatched[which(!is.na(cm.map[cm.unmatched]))]
cm.unmatched <- which(is.na(cm.map))
tlog(2,"Now ",length(m.unmatched),"/",nrow(m.data)," unmatched rows in M and ",length(cm.unmatched),"/",nrow(cm.data)," in CM")
# display problems
#i=3;print(rbind(m.data[m.unmatched[i],], cm.data[which(cm.data[,COL_ATT_ELU_ID]==m.data[m.unmatched[i],COL_ATT_ELU_ID]),]))
#i=1;print(rbind(cm.data[cm.unmatched[i],], m.data[which(m.data[,COL_ATT_ELU_ID]==cm.data[cm.unmatched[i],COL_ATT_ELU_ID]),]))




#############################################################################################
## check multiple matches
#m.tt <- table(m.map)
#m.rs <- as.integer(names(m.tt[which(m.tt>1)]))
##r=1;print(rbind(m.data[which(m.map==m.rs[r]),],cm.data[m.rs[r],]))
#
#cm.tt <- table(cm.map)
#cm.rs <- as.integer(names(cm.tt[which(cm.tt>1)]))
##r=1;print(rbind(cm.data[which(cm.map==cm.rs[r]),],m.data[cm.rs[r],]))




#############################################################################################
tlog(0, "Merging the rows of both tables based on the matches")
res <- m.data[NULL,]
for(r in 1:nrow(m.data))
{	tlog(2, "Merging rows ",r,"/",nrow(m.data))
	tab <- rbind(m.data[r,], cm.data[m.map[r],])
	tab <- merge.overlapping.mandates(data=tab, type="M", strict=FALSE, log=FALSE)
	res <- rbind(res,tab)
}

# TODO passer le script merge sous forme de fonction?
# ou en tout cas y intégrer sous forme de fonction le script de ce fichier-ci
# et restaurer ce fichier à son ancienne forme (comparaison de 2 fichiers texte bruts)
# voir aussi s'il n'y a pas des choses à restaurer dans le fichier compare_table, du coup





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
