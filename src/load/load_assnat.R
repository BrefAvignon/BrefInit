#############################################################################################
# Functions used to inject data from the National Assembly database into the RNE.
# 
# 02/2020 Vincent Labatut
#############################################################################################




#############################################################################################
# Reads the set of XML files constituting the Assembly database, and extract the information
# we need. It is recorded as a separate table used in the rest of the processing.
#############################################################################################
# retrieve the list of all XML files
tlog(2, "Listing all XML files:")
xml.files <- list.files(path=FOLDER_ASSEM_RAW, pattern=".\\.xml", all.files=TRUE,
		full.names=TRUE, recursive=FALSE,
		ignore.case=TRUE, include.dirs=FALSE, no..=TRUE)
tlog(4, "Found ",length(xml.files)," files")

# init personal table
perso.tab <- data.frame(
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	as.character(rep(NA,length(xml.files))),
	#
	check.names=FALSE,
	stringsAsFactors=FALSE
)
colnames(perso.tab) <- c(
	COL_ATT_ELU_ID_ASSEMB,
	COL_ASSEMB_ELU_CIV,
	COL_ATT_ELU_PRENOM,
	COL_ATT_ELU_NOM,
	COL_ATT_ELU_DDN,
	COL_ASSEMB_ELU_CODN,
	COL_ASSEMB_ELU_DEDN,
	COL_ASSEMB_ELU_PADN,
	COL_ASSEMB_PRO_NOM,
	COL_ASSEMB_PRO_CAT,
	COL_ASSEMB_PRO_FAM
)	

# init mandate table
mandate.tab <- data.frame(
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	#
	check.names=FALSE,
	stringsAsFactors=FALSE
)
colnames(mandate.tab) <- c(
	COL_ATT_ELU_ID_ASSEMB,
	COL_ATT_REG_NOM,
	COL_ATT_DPT_NOM,
	COL_ATT_DPT_CODE,
	COL_ATT_CIRC_CODE,
	COL_ATT_MDT_DBT,
	COL_ATT_MDT_FIN,
	COL_ATT_MDT_MOTIF,
	COL_ATT_FCT_NOM
)
mandate.tab <- mandate.tab[-(1:nrow(mandate.tab)),]

# process each XML file one after the oter
tlog(2, "Processing each file separately")
for(i in 1:length(xml.files))
{	tlog(4, "Processing file ",i,"/",length(xml.files)," (",xml.files[i],")")
	
	doc <- xmlParse(file=xml.files[i])
#	root <- xmlRoot(doc)
	
	# retrieve personal info
	perso.tab[i,COL_ATT_ELU_ID_ASSEMB] <- xmlValue(getNodeSet(doc,"/d:acteur/d:uid", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	tlog(6, "ID: ",perso.tab[i,COL_ATT_ELU_ID_ASSEMB])
	perso.tab[i,COL_ASSEMB_ELU_CIV] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:civ", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	perso.tab[i,COL_ATT_ELU_PRENOM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:prenom", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	perso.tab[i,COL_ATT_ELU_NOM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:nom", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	tlog(6, "Name: ",perso.tab[i,COL_ATT_ELU_PRENOM]," ",perso.tab[i,COL_ATT_ELU_NOM])
tlog(6, "trigramme: \"",getNodeSet(doc,"/d:acteur/d:etatCivil/d:ident/d:trigramme", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]][1]$text,"\"")

	# retrieve bith-related info
	perso.tab[i,COL_ATT_ELU_DDN] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:dateNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	tlog(6, "Birthdate: ",perso.tab[i,COL_ATT_ELU_DDN])
	perso.tab[i,COL_ASSEMB_ELU_CODN] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:villeNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	perso.tab[i,COL_ASSEMB_ELU_DEDN] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:depNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	perso.tab[i,COL_ASSEMB_ELU_PADN] <- xmlValue(getNodeSet(doc,"/d:acteur/d:etatCivil/d:infoNaissance/d:paysNais", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	
	# retrieve death-related info
tlog(6,"dateDeces: \"",getNodeSet(doc,"/d:acteur/d:etatCivil/d:dateDeces", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]][1]$text,"\"")
	
	# retrieve occupation-related info
	perso.tab[i,COL_ASSEMB_PRO_NOM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:profession/d:libelleCourant", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	tlog(6, "Occupation: ",perso.tab[i,COL_ASSEMB_PRO_NOM])
	perso.tab[i,COL_ASSEMB_PRO_CAT] <- xmlValue(getNodeSet(doc,"/d:acteur/d:profession/d:socProcINSEE/d:catSocPro", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	perso.tab[i,COL_ASSEMB_PRO_FAM] <- xmlValue(getNodeSet(doc,"/d:acteur/d:profession/d:socProcINSEE/d:famSocPro", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
	
	# retrieve list of mandates
	mandate.nodes <- getNodeSet(doc,"/d:acteur/d:mandats/d:mandat[@xsi:type='MandatParlementaire_type']", c(d="http://schemas.assemblee-nationale.fr/referentiel", xsi="http://www.w3.org/2001/XMLSchema-instance"))
#	mandate.nodes <- getNodeSet(doc,"/d:acteur/d:mandats/d:mandat[@xsi:type='MandatParlementaire_type' and ./d:typeOrgane/text()='ASSEMBLEE']", c(d="http://schemas.assemblee-nationale.fr/referentiel", xsi="http://www.w3.org/2001/XMLSchema-instance"))
	tlog(6, "Computing ",length(mandate.nodes)," mandates")
	for(j in 1:length(mandate.nodes))
	{	tlog(8, "Processing mandate ",j,"/",length(mandate.nodes))
		
		org.type <- xmlValue(getNodeSet(mandate.nodes[[j]],"d:typeOrgane", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]])
		if(org.type=="ASSEMBLEE")
		{	row <- data.frame(
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:acteurRef", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:region", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:departement", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:numDepartement", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:election/d:lieu/d:numCirco", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:dateDebut", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:dateFin", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:mandature/d:causeFin", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				xmlValue(getNodeSet(mandate.nodes[[j]],"d:infosQualite/d:codeQualite", c(d="http://schemas.assemblee-nationale.fr/referentiel"))[[1]]),
				#
				check.names=FALSE,
				stringsAsFactors=FALSE
			)
			colnames(row) <- c(
				COL_ATT_ELU_ID_ASSEMB,
				COL_ATT_REG_NOM,
				COL_ATT_DPT_NOM,
				COL_ATT_DPT_CODE,
				COL_ATT_CIRC_CODE,
				COL_ATT_MDT_DBT,
				COL_ATT_MDT_FIN,
				COL_ATT_MDT_MOTIF,
				COL_ATT_FCT_NOM
			)
			mandate.tab <- rbind(mandate.tab, row)
		}
	}
}
