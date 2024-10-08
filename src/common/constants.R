#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#
# source("src/common/constants.R")
#############################################################################################




#############################################################################################
CORE.NBR <- detectCores(all.tests=TRUE)




#############################################################################################
PLOT_FORMATS <- c("pdf","png")
#PLOT_FORMATS <- "pdf"
#PLOT_FORMATS <- "png"




#############################################################################################
## column names
# stat names
COL_STATS_UNQ <- "Nbr unique"
COL_STATS_NA <- "Nbr NAs"
COL_STATS_MOD <- "Mode"
COL_STATS_NEG <- "Nbr Negative"
COL_STATS_ZER <- "Nbr Zeroes"
COL_STATS_POS <- "Nbr Positive"
COL_STATS_11X <- "Nbr 1st Jan"
COL_STATS_MIN <- "Minimum"
COL_STATS_Q1 <- "1st Quartile"
COL_STATS_MED <- "Median"
COL_STATS_Q3 <- "3rd Quartile"
COL_STATS_MAX <- "Maximum"
COL_STATS_AVG <- "Mean"
COL_STATS_STD <- "Standard deviation"
# all col names for stats
COL_STATS_NAMES <- c(
	COL_STATS_UNQ, COL_STATS_NA, COL_STATS_MOD, COL_STATS_NEG, 
	COL_STATS_ZER, COL_STATS_POS, COL_STATS_11X, COL_STATS_MIN, 
	COL_STATS_Q1, COL_STATS_MED, COL_STATS_Q3, COL_STATS_MAX, 
	COL_STATS_AVG, COL_STATS_STD
)

# attribute names
COL_ATT_CANT_CODE <- "Code canton"
COL_ATT_CANT_ID <- "ID canton"
COL_ATT_CANT_NOM <- "Libelle canton"
COL_ATT_CIRC_CODE <- "Code circo legislative"
COL_ATT_CIRC_NOM <- "Libelle circo legislative"
COL_ATT_CIRCE_CODE <- "Code circo euro"
COL_ATT_CIRCE_NOM <- "Libelle circo euro"
COL_ATT_COM_CODE <- "Code Insee commune"
COL_ATT_COM_NOM <- "Libelle commune"
COL_ATT_COM_POP <- "Population commune"
COL_ATT_CORREC_DATE <- "Correction dates"
COL_ATT_CORREC_INFO <- "Correction autres"
COL_ATT_DPT_CODE <- "Code departement"
COL_ATT_DPT_ID <- "ID departement"
COL_ATT_DPT_NOM <- "Libelle departement"
COL_ATT_ELU_NAIS_COM <- "Commune de naissance"
COL_ATT_ELU_NAIS_DATE <- "Date naissance"
COL_ATT_ELU_NAIS_DPT <- "Departement de naissance"
COL_ATT_ELU_NAIS_PAYS <- "Pays de naissance"
COL_ATT_ELU_DDD <- "Date deces"
COL_ATT_ELU_ID <- "ID universel"
COL_ATT_ELU_ID_RNE <- "ID RNE"
COL_ATT_ELU_ID_ASSEMB <- "ID Assemblee"
COL_ATT_ELU_ID_SENAT <- "ID Senat"
COL_ATT_ELU_ID_EURO <- "ID Europe"
COL_ATT_ELU_NAT <- "Nationalite elu"
COL_ATT_ELU_NOM <- "Nom elu"
COL_ATT_ELU_PRENOM <- "Prenom elu"
COL_ATT_ELU_SEXE <- "Code sexe"
COL_ATT_ELU_NUANCE <- "Nuance politique"
COL_ATT_EPCI_NOM <- "Libelle EPCI"
COL_ATT_EPCI_SIREN <- "Numero SIREN"
COL_ATT_EPCI_DPT_CODE <- "Code departement EPCI"
COL_ATT_EPCI_DPT_ID <- "ID departement EPCI"
COL_ATT_FCT_DBT <- "Date debut fonction"
COL_ATT_FCT_FIN <- "Date fin fonction"
COL_ATT_FCT_MOTIF <- "Motif fin fonction"
COL_ATT_FCT_CODE <- "Code fonction"
COL_ATT_FCT_NOM <- "Libelle fonction"
COL_ATT_MDT_DBT <- "Date debut mandat"
COL_ATT_MDT_FIN <- "Date fin mandat"
COL_ATT_MDT_MOTIF <- "Motif fin mandat"
COL_ATT_MDT_NOM <- "Libelle mandat"
COL_ATT_PRO_CODE <- "Code profession"
COL_ATT_PRO_NOM <- "Libelle profession"
COL_ATT_REG_CODE <- "Code region"
COL_ATT_REG_NOM <- "Libelle region"
COL_ATT_SOURCES <- "Sources"

# normalized col order
COLS_ATT_NORMALIZED <- c(
	COL_ATT_CIRCE_CODE,
	COL_ATT_CIRCE_NOM,
	COL_ATT_REG_CODE,
	COL_ATT_REG_NOM,
	COL_ATT_DPT_ID,
	COL_ATT_DPT_CODE,
	COL_ATT_DPT_NOM,
	COL_ATT_CIRC_CODE,
	COL_ATT_CIRC_NOM,
	COL_ATT_CANT_ID,
	COL_ATT_CANT_CODE,
	COL_ATT_CANT_NOM,
	COL_ATT_COM_CODE,
	COL_ATT_COM_NOM,
	COL_ATT_COM_POP,
	COL_ATT_EPCI_SIREN,
	COL_ATT_EPCI_NOM,
	COL_ATT_EPCI_DPT_ID,
	COL_ATT_EPCI_DPT_CODE,
	COL_ATT_ELU_ID,
	COL_ATT_ELU_ID_RNE,
	COL_ATT_ELU_ID_ASSEMB,
	COL_ATT_ELU_ID_SENAT,
	COL_ATT_ELU_ID_EURO,
	COL_ATT_ELU_NAIS_DATE,
	COL_ATT_ELU_NAIS_COM,
	COL_ATT_ELU_NAIS_DPT,
	COL_ATT_ELU_NAIS_PAYS,
	COL_ATT_ELU_DDD,
	COL_ATT_ELU_NOM,
	COL_ATT_ELU_PRENOM,
	COL_ATT_ELU_SEXE,
	COL_ATT_ELU_NAT,
	COL_ATT_ELU_NUANCE,
	COL_ATT_PRO_CODE,
	COL_ATT_PRO_NOM,
	COL_ATT_MDT_NOM,
	COL_ATT_MDT_DBT,
	COL_ATT_MDT_FIN,
	COL_ATT_MDT_MOTIF,
	COL_ATT_FCT_NOM,
	COL_ATT_FCT_CODE,
	COL_ATT_FCT_DBT,
	COL_ATT_FCT_FIN,
	COL_ATT_FCT_MOTIF,
	COL_ATT_SOURCES,
	COL_ATT_CORREC_DATE,
	COL_ATT_CORREC_INFO
)

# location names
COLS_ATT_LOCATION_NOUNS <- c(
	COL_ATT_CANT_NOM,
	COL_ATT_CIRC_NOM,
	COL_ATT_CIRCE_NOM,
	COL_ATT_COM_NOM,
	COL_ATT_DPT_NOM,
	COL_ATT_EPCI_NOM,
	COL_ATT_REG_NOM
)

# proper nouns
COLS_ATT_PROPER_NOUNS <- c(
	COLS_ATT_LOCATION_NOUNS,
	COL_ATT_ELU_NOM,
	COL_ATT_ELU_PRENOM,
	COL_ATT_FCT_NOM,
	COL_ATT_MDT_NOM,
	COL_ATT_PRO_NOM
)

# column long names
LONGNAMES <- c()
LONGNAMES[COL_ATT_CANT_CODE] <- "Code du canton"
LONGNAMES[COL_ATT_CANT_ID] <- "Id unique du canton"
LONGNAMES[COL_ATT_CANT_NOM] <- "Libellé du canton"
LONGNAMES[COL_ATT_CIRC_CODE] <- "Code de la circonscription législative"
LONGNAMES[COL_ATT_CIRC_NOM] <- "Libellé de la circonscription législative"
LONGNAMES[COL_ATT_CIRCE_CODE] <- "Code de la circonscription européenne"
LONGNAMES[COL_ATT_CIRCE_NOM] <- "Libellé de la circonscription européenne"
LONGNAMES[COL_ATT_COM_CODE] <- "Code de la commune"
LONGNAMES[COL_ATT_COM_NOM] <- "Libellé de la commune"
LONGNAMES[COL_ATT_COM_POP] <- "Population de la commune"
LONGNAMES[COL_ATT_CORREC_DATE] <- "Modification de date(s)"
LONGNAMES[COL_ATT_CORREC_INFO] <- "Autres modifications"
LONGNAMES[COL_ATT_DPT_ID] <- "Id unique du département"
LONGNAMES[COL_ATT_DPT_CODE] <- "Code du département"
LONGNAMES[COL_ATT_DPT_NOM] <- "Libellé du département"
LONGNAMES[COL_ATT_ELU_NAIS_COM] <- "Commune de naissance de l'élu"
LONGNAMES[COL_ATT_ELU_NAIS_DATE] <- "Date de naissance de l'élu"
LONGNAMES[COL_ATT_ELU_NAIS_DPT] <- "Département de naissance de l'élu"
LONGNAMES[COL_ATT_ELU_NAIS_PAYS] <- "Pays de naissance de l'élu"
LONGNAMES[COL_ATT_ELU_DDD] <- "Date de décès de l'élu"
LONGNAMES[COL_ATT_ELU_ID] <- "Id universel de l'élu"
LONGNAMES[COL_ATT_ELU_ID_RNE] <- "Id RNE de l'élu"
LONGNAMES[COL_ATT_ELU_ID_ASSEMB] <- "Id parlementaire de l'élu"
LONGNAMES[COL_ATT_ELU_ID_SENAT] <- "Id sénatorial de l'élu"
LONGNAMES[COL_ATT_ELU_ID_EURO] <- "Id européen de l'élu"
LONGNAMES[COL_ATT_ELU_NAT] <- "Nationalité de l'élu"
LONGNAMES[COL_ATT_ELU_NOM] <- "Nom de l'élu"
LONGNAMES[COL_ATT_ELU_PRENOM] <- "Prénom de l'élu"
LONGNAMES[COL_ATT_ELU_SEXE] <- "Code sexe de l'élu"
LONGNAMES[COL_ATT_ELU_NUANCE] <- "Nuance politique de l'élu"
LONGNAMES[COL_ATT_EPCI_NOM] <- "Libellé de l'EPCI"
LONGNAMES[COL_ATT_EPCI_SIREN] <- "N° SIREN"
LONGNAMES[COL_ATT_EPCI_DPT_CODE] <- "Code du département de l'EPCI"
LONGNAMES[COL_ATT_EPCI_DPT_ID] <- "Id du département de l'EPCI"
LONGNAMES[COL_ATT_FCT_DBT] <- "Date de début de la fonction"
LONGNAMES[COL_ATT_FCT_FIN] <- "Date de fin de la fonction"
LONGNAMES[COL_ATT_FCT_MOTIF] <- "Motif de fin de fonction"
LONGNAMES[COL_ATT_FCT_CODE] <- "Code de la fonction"
LONGNAMES[COL_ATT_FCT_NOM] <- "Libellé de la fonction"
LONGNAMES[COL_ATT_MDT_DBT] <- "Date de début du mandat"
LONGNAMES[COL_ATT_MDT_FIN] <- "Date de fin du mandat"
LONGNAMES[COL_ATT_MDT_MOTIF] <- "Motif de fin de mandat"
LONGNAMES[COL_ATT_MDT_NOM] <- "Libellé du mandat"
LONGNAMES[COL_ATT_PRO_CODE] <- "Code de la profession"
LONGNAMES[COL_ATT_PRO_NOM] <- "Libellé de la profession"
LONGNAMES[COL_ATT_REG_CODE] <- "Code de la région"
LONGNAMES[COL_ATT_REG_NOM] <- "Libellé de la région"
LONGNAMES[COL_ATT_SOURCES] <- "Sources de la ligne"

# column short names
BASENAMES <- c()
BASENAMES[COL_ATT_CANT_CODE] <- "canton_code"
BASENAMES[COL_ATT_CANT_ID] <- "canton_id"
BASENAMES[COL_ATT_CANT_NOM] <- "canton_lib"
BASENAMES[COL_ATT_CIRC_CODE] <- "circo_code"
BASENAMES[COL_ATT_CIRC_NOM] <- "circo_lib"
BASENAMES[COL_ATT_CIRCE_CODE] <- "circe_code"
BASENAMES[COL_ATT_CIRCE_NOM] <- "circe_lib"
BASENAMES[COL_ATT_COM_CODE] <- "commune_code"
BASENAMES[COL_ATT_COM_NOM] <- "commune_lib"
BASENAMES[COL_ATT_COM_POP] <- "commune_pop"
BASENAMES[COL_ATT_CORREC_DATE] <- "correc_date"
BASENAMES[COL_ATT_CORREC_INFO] <- "correc_info"
BASENAMES[COL_ATT_DPT_CODE] <- "dept_code"
BASENAMES[COL_ATT_DPT_ID] <- "dept_id"
BASENAMES[COL_ATT_DPT_NOM] <- "dept_lib"
BASENAMES[COL_ATT_ELU_NAIS_COM] <- "elu_nais_com"
BASENAMES[COL_ATT_ELU_NAIS_DATE] <- "elu_nais_date"
BASENAMES[COL_ATT_ELU_NAIS_DPT] <- "elu_nais_dpt"
BASENAMES[COL_ATT_ELU_NAIS_PAYS] <- "elu_nais_pays"
BASENAMES[COL_ATT_ELU_DDD] <- "elu_ddd"
BASENAMES[COL_ATT_ELU_ID] <- "elu_id_univ"
BASENAMES[COL_ATT_ELU_ID_RNE] <- "elu_id_rne"
BASENAMES[COL_ATT_ELU_ID_ASSEMB] <- "elu_id_assemb"
BASENAMES[COL_ATT_ELU_ID_SENAT] <- "elu_id_senat"
BASENAMES[COL_ATT_ELU_ID_EURO] <- "elu_id_euro"
BASENAMES[COL_ATT_ELU_NAT] <- "elu_nation"
BASENAMES[COL_ATT_ELU_NOM] <- "elu_nom"
BASENAMES[COL_ATT_ELU_NUANCE] <- "elu_nuance"
BASENAMES[COL_ATT_ELU_PRENOM] <- "elu_prenom"
BASENAMES[COL_ATT_ELU_SEXE] <- "elu_sexe"
BASENAMES[COL_ATT_EPCI_DPT_CODE] <- "epci_dept_code"
BASENAMES[COL_ATT_EPCI_DPT_ID] <- "epci_dept_id"
BASENAMES[COL_ATT_EPCI_NOM] <- "epci_lib"
BASENAMES[COL_ATT_EPCI_SIREN] <- "epci_siren"
BASENAMES[COL_ATT_FCT_DBT] <- "fonction_debut"
BASENAMES[COL_ATT_FCT_FIN] <- "fonction_fin"
BASENAMES[COL_ATT_FCT_MOTIF] <- "fonction_motif"
BASENAMES[COL_ATT_FCT_CODE] <- "fonction_code"
BASENAMES[COL_ATT_FCT_NOM] <- "fonction_lib"
BASENAMES[COL_ATT_MDT_DBT] <- "mandat_debut"
BASENAMES[COL_ATT_MDT_FIN] <- "mandat_fin"
BASENAMES[COL_ATT_MDT_MOTIF] <- "mandat_motif"
BASENAMES[COL_ATT_MDT_NOM] <- "mandat_lib"
BASENAMES[COL_ATT_PRO_CODE] <- "elu_pro_code"
BASENAMES[COL_ATT_PRO_NOM] <- "elu_pro_lib"
BASENAMES[COL_ATT_REG_CODE] <- "region_code"
BASENAMES[COL_ATT_REG_NOM] <- "region_lib"
BASENAMES[COL_ATT_SOURCES] <- "sources"

# column data types
COL_TYPES <- c()
COL_TYPES[COL_ATT_CANT_CODE] <- "cat"
COL_TYPES[COL_ATT_CANT_ID] <- "cat"
COL_TYPES[COL_ATT_CANT_NOM] <- "nom"
COL_TYPES[COL_ATT_CIRC_CODE] <- "cat"
COL_TYPES[COL_ATT_CIRC_NOM] <- "nom"
COL_TYPES[COL_ATT_CIRCE_CODE] <- "cat"
COL_TYPES[COL_ATT_CIRCE_NOM] <- "nom"
COL_TYPES[COL_ATT_COM_CODE] <- "cat"
COL_TYPES[COL_ATT_COM_NOM] <- "nom"
COL_TYPES[COL_ATT_COM_POP] <- "num"
COL_TYPES[COL_ATT_CORREC_DATE] <- "cat"
COL_TYPES[COL_ATT_CORREC_INFO] <- "cat"
COL_TYPES[COL_ATT_DPT_ID] <- "nom"
COL_TYPES[COL_ATT_DPT_CODE] <- "cat"
COL_TYPES[COL_ATT_DPT_NOM] <- "nom"
COL_TYPES[COL_ATT_ELU_NAIS_COM] <- "nom"
COL_TYPES[COL_ATT_ELU_NAIS_DATE] <- "dat"
COL_TYPES[COL_ATT_ELU_NAIS_DPT] <- "nom"
COL_TYPES[COL_ATT_ELU_NAIS_PAYS] <- "nom"
COL_TYPES[COL_ATT_ELU_DDD] <- "dat"
COL_TYPES[COL_ATT_ELU_ID] <- "nom"
COL_TYPES[COL_ATT_ELU_ID_RNE] <- "cat"
COL_TYPES[COL_ATT_ELU_ID_ASSEMB] <- "nom"
COL_TYPES[COL_ATT_ELU_ID_SENAT] <- "nom"
COL_TYPES[COL_ATT_ELU_ID_EURO] <- "nom"
COL_TYPES[COL_ATT_ELU_NAT] <- "cat"
COL_TYPES[COL_ATT_ELU_NOM] <- "nom"
COL_TYPES[COL_ATT_ELU_NUANCE] <- "cat"
COL_TYPES[COL_ATT_ELU_PRENOM] <- "nom"
COL_TYPES[COL_ATT_ELU_SEXE] <- "cat"
COL_TYPES[COL_ATT_EPCI_SIREN] <- "cat"
COL_TYPES[COL_ATT_EPCI_NOM] <- "nom"
COL_TYPES[COL_ATT_EPCI_DPT_CODE] <- "cat"
COL_TYPES[COL_ATT_EPCI_DPT_ID] <- "nom"
COL_TYPES[COL_ATT_FCT_CODE] <- "cat"
COL_TYPES[COL_ATT_FCT_NOM] <- "nom"
COL_TYPES[COL_ATT_FCT_DBT] <- "dat"
COL_TYPES[COL_ATT_FCT_FIN] <- "dat"
COL_TYPES[COL_ATT_FCT_MOTIF] <- "cat"
COL_TYPES[COL_ATT_MDT_NOM] <- "nom"
COL_TYPES[COL_ATT_MDT_DBT] <- "dat"
COL_TYPES[COL_ATT_MDT_FIN] <- "dat"
COL_TYPES[COL_ATT_MDT_MOTIF] <- "cat"
COL_TYPES[COL_ATT_PRO_CODE] <- "cat"
COL_TYPES[COL_ATT_PRO_NOM] <- "nom"
COL_TYPES[COL_ATT_REG_CODE] <- "cat"
COL_TYPES[COL_ATT_REG_NOM] <- "nom"
COL_TYPES[COL_ATT_SOURCES] <- "cat"

# colunms for correction files
COL_CORREC_ROW <- "Row"
COL_CORREC_ID <- "Id RNE"
COL_CORREC_NOM <- "Nom"
COL_CORREC_PRENOM <- "Prenom"
COL_CORREC_ATTR <- "Attribut"
COL_CORREC_VALAVT <- "ValAvt"
COL_CORREC_VALAPR <- "ValApr"
COL_CORREC_COMM <- "Commentaire"

# colunms for verification files
COL_VERIF_MDT_NBR <- "Nombre"
COL_VERIF_MDT_DBT <- "Debut"
COL_VERIF_MDT_FIN <- "Fin"
COL_VERIF_DATE_TOUR1 <- "Tour1"
COL_VERIF_DATE_TOUR2 <- "Tour2"
COL_VERIF_SERIE <- "Serie"
COL_VERIF_SERIES <- "Series"

# columns for senate files
COL_SENAT_CANT_NOM <- "Circonscription"
COL_SENAT_COM_NOM <- "Commune"
COL_SENAT_DPT_NOM <- "Circonscription"
COL_SENAT_DPT_NOM2 <- "Departement ou collectivite"
COL_SENAT_ELU_DDD <- "Date de deces"
COL_SENAT_ELU_DDN <- "Date naissance"
COL_SENAT_ELU_MATRI <- "Matricule"
COL_SENAT_ELU_NOM <- "Nom usuel"
COL_SENAT_ELU_PRENOM <- "Prenom usuel"
COL_SENAT_ELU_NUANCE <- "Groupe politique"
COL_SENAT_ELU_QUALI <- "Qualite"
COL_SENAT_FCT_ADBT <- "Annee de debut de fonction"
COL_SENAT_FCT_AFIN <- "Annee de fin de fonction"
COL_SENAT_FCT_DBT <- "Date de debut de fonction"
COL_SENAT_FCT_FIN <- "Date de fin de fonction"
COL_SENAT_FCT_BUR <- "Fonction au Bureau du Senat"
COL_SENAT_FCT_NOM <- "Fonction"
COL_SENAT_MDT_ADBT <- "Annee de debut de mandat"
COL_SENAT_MDT_AFIN <- "Annee de fin de mandat"
COL_SENAT_MDT_DBT <- "Date de debut de mandat"
COL_SENAT_MDT_FIN <- "Date de fin de mandat"
COL_SENAT_MDT_MOTIF <- "Motif fin de mandat"
COL_SENAT_PRO_NOM <- "PCS INSEE"
COL_SENAT_REG_NOM <- "Region"

# columns for assemblee files
COL_ASSEMB_PRO_NOM <- "Nom profession"
COL_ASSEMB_PRO_CAT <- "Categorie profession"
COL_ASSEMB_PRO_FAM <- "Famille profession"




#############################################################################################
## folder and file constants
# input folders
FOLDER_IN <- "in"
	FOLDER_CONV <- file.path(FOLDER_IN,"conversions")
	FOLDER_EXTRACT1 <- file.path(FOLDER_IN,"extraction1")
		FOLDER_TABLES <- file.path(FOLDER_EXTRACT1,"tables")
		FOLDER_CORRECS <- file.path(FOLDER_EXTRACT1,"corrections")
	FOLDER_EXTRACT2 <- file.path(FOLDER_IN,"extraction2")
		FOLDER_TABLES2 <- file.path(FOLDER_EXTRACT2,"tables")
		FOLDER_CORRECS2 <- file.path(FOLDER_EXTRACT2,"corrections")
	FOLDER_VERIFS <- file.path(FOLDER_IN,"verifications")
	FOLDER_SECOND <- file.path(FOLDER_IN,"secondary")
		FOLDER_ASSEMBLEE <- file.path(FOLDER_SECOND,"assemblee")
		FOLDER_SENAT <- file.path(FOLDER_SECOND,"senat")

# table files
	# first extraction
	FILES_TAB_CD <- file.path(FOLDER_TABLES, c("F Tous CD.txt"))
	FILES_TAB_CM <- file.path(FOLDER_TABLES, c("A Tous CM 01 30.txt", "B Tous CM 31 60.txt", "C Tous CM 61 95.txt", "D Tous CM OM.txt"))
	FILES_TAB_CR <- file.path(FOLDER_TABLES, c("G Tous CR.txt"))
	FILES_TAB_D <- file.path(FOLDER_TABLES, c("H Tous Deputes.txt"))
	FILES_TAB_DE <- file.path(FOLDER_TABLES, c("J Tous RPE.txt"))
	FILES_TAB_EPCI <- file.path(FOLDER_TABLES, c("E Tous Membres EPCI.txt"))
	FILES_TAB_M <- file.path(FOLDER_TABLES, c("K Tous Maires.txt"))
	FILES_TAB_S <- file.path(FOLDER_TABLES, c("I Tous Senateurs.txt"))
	# second extraction
	FILES_TAB_CD2 <- file.path(FOLDER_TABLES2, c("RNE_export_CD_20191009.txt"))
	FILES_TAB_CM2 <- file.path(FOLDER_TABLES2, c("RNE_export_CM_20191023.txt"))
	FILES_TAB_CR2 <- file.path(FOLDER_TABLES2, c("RNE_export_CR_20191025.txt"))
	FILES_TAB_D2 <- file.path(FOLDER_TABLES2, c("RNE_export_D_20200211.txt"))
	FILES_TAB_DE2 <- file.path(FOLDER_TABLES2, c("RNE_export_DE_20200211.txt"))
	FILES_TAB_EPCI2 <- file.path(FOLDER_TABLES2, c("RNE_export_EPCI_20200211.txt"))
	FILES_TAB_S2 <- file.path(FOLDER_TABLES2, c("RNE_export_S_20200211.txt"))
# correction files
	# first extraction
		# correction files
		FILE_CORREC_CD <- file.path(FOLDER_CORRECS, "correc_CD.txt")
		FILE_CORREC_CM <- file.path(FOLDER_CORRECS, "correc_CM.txt")
		FILE_CORREC_CR <- file.path(FOLDER_CORRECS, "correc_CR.txt")
		FILE_CORREC_D <- file.path(FOLDER_CORRECS, "correc_D.txt")
		FILE_CORREC_DE <- file.path(FOLDER_CORRECS, "correc_DE.txt")
		FILE_CORREC_EPCI <- file.path(FOLDER_CORRECS, "correc_EPCI.txt")
		FILE_CORREC_M <- file.path(FOLDER_CORRECS, "correc_M.txt")
		FILE_CORREC_S <- file.path(FOLDER_CORRECS, "correc_S.txt")
		# supplementary data
		FILE_SUPPL_CD <- file.path(FOLDER_CORRECS, "supp_CD.txt")
		FILE_SUPPL_CM <- file.path(FOLDER_CORRECS, "supp_CM.txt")
		FILE_SUPPL_CR <- file.path(FOLDER_CORRECS, "supp_CR.txt")
		FILE_SUPPL_D <- file.path(FOLDER_CORRECS, "supp_D.txt")
		FILE_SUPPL_DE <- file.path(FOLDER_CORRECS, "supp_DE.txt")
		FILE_SUPPL_M <- file.path(FOLDER_CORRECS, "supp_M.txt")
		FILE_SUPPL_PRF <- file.path(FOLDER_CORRECS, "supp_PRF.txt")
	# second extraction
		# correction files
		FILE_CORREC_CD2 <- file.path(FOLDER_CORRECS2, "correc_CD.txt")
		FILE_CORREC_CM2 <- file.path(FOLDER_CORRECS2, "correc_CM.txt")
		FILE_CORREC_CR2 <- file.path(FOLDER_CORRECS2, "correc_CR.txt")
# verification files
	# position counts
	FILE_VERIF_NBR_CR <- file.path(FOLDER_VERIFS, "decomptes_CR.txt")
	FILE_VERIF_NBR_DE <- file.path(FOLDER_VERIFS, "decomptes_DE.txt")
	FILE_VERIF_NBR_S <- file.path(FOLDER_VERIFS, "decomptes_S.txt")
	# election dates
	FILE_VERIF_DATES_CD <- file.path(FOLDER_VERIFS, "elections_CD.txt")
	FILE_VERIF_DATES_CM <- file.path(FOLDER_VERIFS, "elections_CM.txt")
	FILE_VERIF_DATES_CR <- file.path(FOLDER_VERIFS, "elections_CR.txt")
	FILE_VERIF_DATES_D <- file.path(FOLDER_VERIFS, "elections_D.txt")
	FILE_VERIF_DATES_DE <- file.path(FOLDER_VERIFS, "elections_DE.txt")
	FILE_VERIF_DATES_S <- file.path(FOLDER_VERIFS, "elections_S.txt")
	# series lists
	FILE_VERIF_SERIES_CD <- file.path(FOLDER_VERIFS, "series_CD.txt")
	FILE_VERIF_SERIES_S <- file.path(FOLDER_VERIFS, "series_S.txt")
# secondary data
	# national assembly files
	FOLDER_ASSEMB_RAW <- file.path(FOLDER_ASSEMBLEE, "raw")
	FILE_ASSEMB_CONV_DPTS <- file.path(FOLDER_ASSEMBLEE, "conv_dpts.txt")
	FILE_ASSEMB_CONV_IDS <- file.path(FOLDER_ASSEMBLEE, "elu_equiv_ids.txt")
	FILE_ASSEMB_CONV_NOMSFAM <- file.path(FOLDER_ASSEMBLEE, "conv_noms.txt")
	FILE_ASSEMB_CONV_PAYS <- file.path(FOLDER_ASSEMBLEE, "conv_pays.txt")
	FILE_ASSEMB_CONV_PRENOMS <- file.path(FOLDER_ASSEMBLEE, "conv_prenoms.txt")
	FILE_ASSEMB_CORRECS <- file.path(FOLDER_ASSEMBLEE,"corrections.txt")
	FILE_ASSEMB_GENERAL <- file.path(FOLDER_ASSEMBLEE, "general.txt")
	FILE_ASSEMB_GENERAL_CACHE <- file.path(FOLDER_ASSEMBLEE, "_general_cache.txt")
	FILE_ASSEMB_MANDATS <- file.path(FOLDER_ASSEMBLEE, "depute.txt")
	# senate files
	FILE_SENAT_CONV_DPTS <- file.path(FOLDER_SENAT, "conv_dpts.txt")
	FILE_SENAT_CONV_NOMSFAM <- file.path(FOLDER_SENAT, "conv_noms.txt")
	FILE_SENAT_CONV_NUANCES <- file.path(FOLDER_SENAT, "conv_nuances.txt")
	FILE_SENAT_CONV_PRENOMS <- file.path(FOLDER_SENAT, "conv_prenoms.txt")
	FILE_SENAT_CONV_PRO <- file.path(FOLDER_SENAT, "conv_professions.txt")
	FILE_SENAT_CONV_REGIONS <- file.path(FOLDER_SENAT, "conv_regions.txt")
	FILE_SENAT_CORRECS <- file.path(FOLDER_SENAT,"corrections.txt")
	FILE_SENAT_ELEC_CD <- file.path(FOLDER_SENAT, "departemental.txt")
	FILE_SENAT_ELEC_D <- file.path(FOLDER_SENAT, "depute.txt")
	FILE_SENAT_ELEC_DE <- file.path(FOLDER_SENAT, "europeen.txt")
#	FILE_SENAT_ELEC_METRO <- file.path(FOLDER_SENAT, "metropolitain.txt")	# not sure what this is exactly
	FILE_SENAT_ELEC_CM <- file.path(FOLDER_SENAT, "municipal.txt")
	FILE_SENAT_ELEC_CR <- file.path(FOLDER_SENAT, "regional.txt")
	FILE_SENAT_ELEC_S <- file.path(FOLDER_SENAT, "senatorial.txt")
#	FILE_SENAT_ELEC_EPCI <- file.path(FOLDER_SENAT, "territorial.txt")		# not sure what this is exactly
	FILE_SENAT_GENERAL <- file.path(FOLDER_SENAT, "general.txt")
	FILE_SENAT_GENERAL_CACHE <- file.path(FOLDER_SENAT, "_general_cache.txt")
	FILE_SENAT_GROUPE <- file.path(FOLDER_SENAT, "groupes.txt")
# conversion files
	FILE_CONV_IDS <- file.path(FOLDER_CONV, "elu_equiv_ids.txt")
	FILE_CONV_HOMONYMS <- file.path(FOLDER_CONV, "elu_homonyms.txt")
	FILE_CONV_CANTONS <- file.path(FOLDER_CONV, "canton_unique_ids.txt")
	FILE_CONV_DPT <- file.path(FOLDER_CONV, "departements.txt")
	FILE_CONV_PRO <- file.path(FOLDER_CONV, "professions.txt")
	FILE_CONV_REGIONS <- file.path(FOLDER_CONV, "regions.txt")
	FILE_CONV_NUANCES <- file.path(FOLDER_CONV, "nuances.txt")


# output folders
# folder to store logs
FOLDER_LOG <- "log"
# folder to store results
FOLDER_OUT <- "out"
	# first extraction folders
	FOLDER_EXTRACT1 <- file.path(FOLDER_OUT,"extraction1")
		# table-specific output folders
		FOLDER_OUT_CD <- file.path(FOLDER_EXTRACT1, "CD")
		FOLDER_OUT_CM <- file.path(FOLDER_EXTRACT1, "CM")
		FOLDER_OUT_CR <- file.path(FOLDER_EXTRACT1, "CR")
		FOLDER_OUT_D <- file.path(FOLDER_EXTRACT1, "D")
		FOLDER_OUT_DE <- file.path(FOLDER_EXTRACT1, "DE")
		FOLDER_OUT_EPCI <- file.path(FOLDER_EXTRACT1, "EPCI")
		FOLDER_OUT_M <- file.path(FOLDER_EXTRACT1, "M")
		FOLDER_OUT_S <- file.path(FOLDER_EXTRACT1, "S")
		FOLDER_OUT_ALL <- file.path(FOLDER_EXTRACT1, "_All")
		# first extraction cache files	
		FILE_CACHE_CD <- file.path(FOLDER_OUT_CD, "_data.txt")
		FILE_CACHE_CM <- file.path(FOLDER_OUT_CM, "_data.txt")
		FILE_CACHE_CR <- file.path(FOLDER_OUT_CR, "_data.txt")
		FILE_CACHE_D <- file.path(FOLDER_OUT_D, "_data.txt")
		FILE_CACHE_DE <- file.path(FOLDER_OUT_DE, "_data.txt")
		FILE_CACHE_EPCI <- file.path(FOLDER_OUT_EPCI, "_data.txt")
		FILE_CACHE_M <- file.path(FOLDER_OUT_M, "_data.txt")
		FILE_CACHE_S <- file.path(FOLDER_OUT_S, "_data.txt")
		FILE_CACHE_ALL <- file.path(FOLDER_OUT_ALL, "_data.txt")
	# second extraction folders
	FOLDER_EXTRACT2 <- file.path(FOLDER_OUT,"extraction2")
		# table-specific output folders
		FOLDER_OUT_CD2 <- file.path(FOLDER_EXTRACT2, "CD")
		FOLDER_OUT_CM2 <- file.path(FOLDER_EXTRACT2, "CM")
		FOLDER_OUT_CR2 <- file.path(FOLDER_EXTRACT2, "CR")
		FOLDER_OUT_D2 <- file.path(FOLDER_EXTRACT2, "D")
		FOLDER_OUT_DE2 <- file.path(FOLDER_EXTRACT2, "DE")
		FOLDER_OUT_EPCI2 <- file.path(FOLDER_EXTRACT2, "EPCI")
		FOLDER_OUT_M2 <- file.path(FOLDER_EXTRACT2, "M")
		FOLDER_OUT_S2 <- file.path(FOLDER_EXTRACT2, "S")
		FOLDER_OUT_ALL2 <- file.path(FOLDER_EXTRACT2, "_All")
		# second extraction cache files	
		FILE_CACHE_CD2 <- file.path(FOLDER_OUT_CD2, "_data.txt")
		FILE_CACHE_CM2 <- file.path(FOLDER_OUT_CM2, "_data.txt")
		FILE_CACHE_CR2 <- file.path(FOLDER_OUT_CR2, "_data.txt")
		FILE_CACHE_D2 <- file.path(FOLDER_OUT_D2, "_data.txt")
		FILE_CACHE_DE2 <- file.path(FOLDER_OUT_DE2, "_data.txt")
		FILE_CACHE_EPCI2 <- file.path(FOLDER_OUT_EPCI2, "_data.txt")
		FILE_CACHE_M2 <- file.path(FOLDER_OUT_M2, "_data.txt")
		FILE_CACHE_S2 <- file.path(FOLDER_OUT_S2, "_data.txt")
		FILE_CACHE_ALL2 <- file.path(FOLDER_OUT_ALL2, "_data.txt")
	# comparison folders
	FOLDER_COMPARISON <- file.path(FOLDER_OUT,"comparison")
		# same table in 2 version
		FOLDER_COMPAR_VERSIONS <- file.path(FOLDER_COMPARISON,"versions")
			# table-specific output folders
			FOLDER_COMP_VERS_CD <- file.path(FOLDER_COMPAR_VERSIONS, "CD")
			FOLDER_COMP_VERS_CM <- file.path(FOLDER_COMPAR_VERSIONS, "CM")
			FOLDER_COMP_VERS_CR <- file.path(FOLDER_COMPAR_VERSIONS, "CR")
			FOLDER_COMP_VERS_D <- file.path(FOLDER_COMPAR_VERSIONS, "D")
			FOLDER_COMP_VERS_DE <- file.path(FOLDER_COMPAR_VERSIONS, "DE")
			FOLDER_COMP_VERS_EPCI <- file.path(FOLDER_COMPAR_VERSIONS, "EPCI")
			FOLDER_COMP_VERS_M <- file.path(FOLDER_COMPAR_VERSIONS, "M")
			FOLDER_COMP_VERS_S <- file.path(FOLDER_COMPAR_VERSIONS, "S")
			FOLDER_COMP_VERS_ALL <- file.path(FOLDER_COMPAR_VERSIONS, "_All")
		# tables from different sources
		FOLDER_COMPAR_SOURCES <- file.path(FOLDER_COMPARISON,"sources")
			# source-specific output folders
			FOLDER_COMP_SRC_ASSEMB <- file.path(FOLDER_COMPAR_SOURCES, "Assemblee")
			FOLDER_COMP_SRC_SEN <- file.path(FOLDER_COMPAR_SOURCES, "Senat")
