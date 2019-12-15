#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#############################################################################################




#############################################################################################
CORE.NBR <- detectCores(all.tests=TRUE)




#############################################################################################
#PLOT_FORMAT <- "pdf"
PLOT_FORMAT <- "png"




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
COL_ATT_CANT_NOM <- "Libellé canton"
COL_ATT_CIRC_CODE <- "Code circo législative"
COL_ATT_CIRC_NOM <- "Libellé circo législative"
COL_ATT_CIRCE_CODE <- "Code circo euro"
COL_ATT_CIRCE_NOM <- "Libellé circo euro"
COL_ATT_COM_CODE <- "Code Insee commune"
COL_ATT_COM_NOM <- "Libellé commune"
COL_ATT_COM_POP <- "Population commune"
COL_ATT_DPT_CODE <- "Code département"
COL_ATT_DPT_NOM <- "Libellé département"
COL_ATT_ELU_DDN <- "Date naissance"
COL_ATT_ELU_ID <- "ID élu"
COL_ATT_ELU_NAT <- "Nationalité élu"
COL_ATT_ELU_NOM <- "Nom élu"
COL_ATT_ELU_PRENOM <- "Prénom élu"
COL_ATT_ELU_SEXE <- "Code sexe"
COL_ATT_ELU_NUANCE <- "Nuance politique"
COL_ATT_EPCI_NOM <- "Libellé EPCI"
COL_ATT_EPCI_SIREN <- "Numéro SIREN"
COL_ATT_EPCI_DPT <- "Code département EPCI"
COL_ATT_FCT_DBT <- "Date début fonction"
COL_ATT_FCT_FIN <- "Date fin fonction"
COL_ATT_FCT_MOTIF <- "Motif fin fonction"
COL_ATT_FCT_CODE <- "Code fonction"
COL_ATT_FCT_NOM <- "Libellé fonction"
COL_ATT_MDT_DBT <- "Date début mandat"
COL_ATT_MDT_FIN <- "Date fin mandat"
COL_ATT_MDT_MOTIF <- "Motif fin mandat"
COL_ATT_MDT_NOM <- "Libellé mandat"
COL_ATT_PRO_CODE <- "Code profession"
COL_ATT_PRO_NOM <- "Libellé profession"
COL_ATT_REG_CODE <- "Code région"
COL_ATT_REG_NOM <- "Libellé région"

# normalized col order
COLS_ATT_NORMALIZED <- c(
	COL_ATT_CIRCE_CODE,
	COL_ATT_CIRCE_NOM,
	COL_ATT_REG_CODE,
	COL_ATT_REG_NOM,
	COL_ATT_DPT_CODE,
	COL_ATT_DPT_NOM,
	COL_ATT_CIRC_CODE,
	COL_ATT_CIRC_NOM,
	COL_ATT_CANT_CODE,
	COL_ATT_CANT_NOM,
	COL_ATT_COM_CODE,
	COL_ATT_COM_NOM,
	COL_ATT_COM_POP,
	COL_ATT_EPCI_SIREN,
	COL_ATT_EPCI_NOM,
	COL_ATT_EPCI_DPT,
	COL_ATT_ELU_ID,
	COL_ATT_ELU_NOM,
	COL_ATT_ELU_PRENOM,
	COL_ATT_ELU_DDN,
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
	COL_ATT_FCT_MOTIF
)

# column long names
LONGNAMES <- c()
LONGNAMES[COL_ATT_CANT_CODE] <- "Code du canton"
LONGNAMES[COL_ATT_CANT_NOM] <- "Libellé du canton"
LONGNAMES[COL_ATT_CIRC_CODE] <- "Code de la circonscription législative"
LONGNAMES[COL_ATT_CIRC_NOM] <- "Libellé de la circonscription législative"
LONGNAMES[COL_ATT_CIRCE_CODE] <- "Code de la circonscription européenne"
LONGNAMES[COL_ATT_CIRCE_NOM] <- "Libellé de la circonscription européenne"
LONGNAMES[COL_ATT_COM_CODE] <- "Code de la commune"
LONGNAMES[COL_ATT_COM_NOM] <- "Libellé de la commune"
LONGNAMES[COL_ATT_COM_POP] <- "Population de la commune"
LONGNAMES[COL_ATT_DPT_CODE] <- "Code du département"
LONGNAMES[COL_ATT_DPT_NOM] <- "Libellé du département"
LONGNAMES[COL_ATT_ELU_DDN] <- "Date de naissance de l'élu"
LONGNAMES[COL_ATT_ELU_ID] <- "Numéro d'identification de l'élu"
LONGNAMES[COL_ATT_ELU_NAT] <- "Nationalité de l'élu"
LONGNAMES[COL_ATT_ELU_NOM] <- "Nom de l'élu"
LONGNAMES[COL_ATT_ELU_PRENOM] <- "Prénom de l'élu"
LONGNAMES[COL_ATT_ELU_SEXE] <- "Code sexe de l'élu"
LONGNAMES[COL_ATT_ELU_NUANCE] <- "Nuance politique de l'élu"
LONGNAMES[COL_ATT_EPCI_NOM] <- "Libellé de l'EPCI"
LONGNAMES[COL_ATT_EPCI_SIREN] <- "N° SIREN"
LONGNAMES[COL_ATT_EPCI_DPT] <- "Code du département de l'EPCI"
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

# column short names
BASENAMES <- c()
BASENAMES[COL_ATT_CANT_CODE] <- "canton_code"
BASENAMES[COL_ATT_CANT_NOM] <- "canton_lib"
BASENAMES[COL_ATT_CIRC_CODE] <- "circo_code"
BASENAMES[COL_ATT_CIRC_NOM] <- "circo_lib"
BASENAMES[COL_ATT_CIRCE_CODE] <- "circe_code"
BASENAMES[COL_ATT_CIRCE_NOM] <- "circe_lib"
BASENAMES[COL_ATT_COM_CODE] <- "commune_code"
BASENAMES[COL_ATT_COM_NOM] <- "commune_lib"
BASENAMES[COL_ATT_COM_POP] <- "commune_pop"
BASENAMES[COL_ATT_DPT_CODE] <- "dept_code"
BASENAMES[COL_ATT_DPT_NOM] <- "dept_lib"
BASENAMES[COL_ATT_ELU_DDN] <- "elu_ddn"
BASENAMES[COL_ATT_ELU_ID] <- "elu_id"
BASENAMES[COL_ATT_ELU_NAT] <- "elu_nation"
BASENAMES[COL_ATT_ELU_NOM] <- "elu_nom"
BASENAMES[COL_ATT_ELU_NUANCE] <- "elu_nuance"
BASENAMES[COL_ATT_ELU_PRENOM] <- "elu_prenom"
BASENAMES[COL_ATT_ELU_SEXE] <- "elu_sexe"
BASENAMES[COL_ATT_EPCI_DPT] <- "epci_dept"
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

# column data types
COL_TYPES <- c()
COL_TYPES[COL_ATT_CANT_CODE] <- "cat"
COL_TYPES[COL_ATT_CANT_NOM] <- "nom"
COL_TYPES[COL_ATT_COM_CODE] <- "cat"
COL_TYPES[COL_ATT_COM_NOM] <- "nom"
COL_TYPES[COL_ATT_COM_POP] <- "num"
COL_TYPES[COL_ATT_CIRC_CODE] <- "cat"
COL_TYPES[COL_ATT_CIRC_NOM] <- "nom"
COL_TYPES[COL_ATT_CIRCE_CODE] <- "cat"
COL_TYPES[COL_ATT_CIRCE_NOM] <- "nom"
COL_TYPES[COL_ATT_DPT_CODE] <- "cat"
COL_TYPES[COL_ATT_DPT_NOM] <- "nom"
COL_TYPES[COL_ATT_ELU_DDN] <- "dat"
COL_TYPES[COL_ATT_ELU_ID] <- "cat"
COL_TYPES[COL_ATT_ELU_NAT] <- "cat"
COL_TYPES[COL_ATT_ELU_NOM] <- "nom"
COL_TYPES[COL_ATT_ELU_NUANCE] <- "cat"
COL_TYPES[COL_ATT_ELU_PRENOM] <- "nom"
COL_TYPES[COL_ATT_ELU_SEXE] <- "cat"
COL_TYPES[COL_ATT_EPCI_SIREN] <- "cat"
COL_TYPES[COL_ATT_EPCI_NOM] <- "nom"
COL_TYPES[COL_ATT_EPCI_DPT] <- "cat"
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

# colunms for correction files
COL_CORREC_ID <- "Id"
COL_CORREC_NOM <- "Nom"
COL_CORREC_PRENOM <- "Prénom"
COL_CORREC_ATTR <- "Attribut"
COL_CORREC_VALAVT <- "ValAvt"
COL_CORREC_VALAPR <- "ValApr"
COL_CORREC_COMM <- "Commentaire"

# colunms for verification files
COL_VERIF_CIRCE_CODE <- "CodeCirco"
COL_VERIF_CIRCE_NOM <- "Nom"
COL_VERIF_DPT_CODE <- "CodeDpt"
COL_VERIF_DPT_NOM <- "Nom"
COL_VERIF_MDT_NBR <- "Nombre"
COL_VERIF_MDT_DBT <- "Début"
COL_VERIF_MDT_FIN <- "Fin"
COL_VERIF_REG_CODE <- "CodeRégion"
COL_VERIF_REG_NOM <- "Nom"
COL_VERIF_DATE_TOUR1 <- "Tour1"
COL_VERIF_DATE_TOUR2 <- "Tour2"




#############################################################################################
## folder and file constants
# input folders
FOLDER_IN <- "in"
	FOLDER_EXTRACT1 <- file.path(FOLDER_IN,"extraction1")
		FOLDER_TABLES <- file.path(FOLDER_EXTRACT1,"tables")
		FOLDER_CORRECS <- file.path(FOLDER_EXTRACT1,"corrections")
	FOLDER_EXTRACT2 <- file.path(FOLDER_IN,"extraction2")
		FOLDER_TABLES2 <- file.path(FOLDER_EXTRACT2,"tables")
		FOLDER_CORRECS2 <- file.path(FOLDER_EXTRACT2,"corrections")
	FOLDER_VERIFS <- file.path(FOLDER_IN,"verifications")
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
# correction files
	# first extraction
	FILE_CORREC_CD <- file.path(FOLDER_CORRECS, "correc_CD.txt")
	FILE_CORREC_CM <- file.path(FOLDER_CORRECS, "correc_CM.txt")
	FILE_CORREC_CR <- file.path(FOLDER_CORRECS, "correc_CR.txt")
	FILE_CORREC_D <- file.path(FOLDER_CORRECS, "correc_D.txt")
	FILE_CORREC_DE <- file.path(FOLDER_CORRECS, "correc_DE.txt")
	FILE_CORREC_EPCI <- file.path(FOLDER_CORRECS, "correc_EPCI.txt")
	FILE_CORREC_M <- file.path(FOLDER_CORRECS, "correc_M.txt")
	FILE_CORREC_S <- file.path(FOLDER_CORRECS, "correc_S.txt")
	# second extraction
	FILE_CORREC_CD2 <- file.path(FOLDER_CORRECS2, "correc_CD.txt")
	FILE_CORREC_CM2 <- file.path(FOLDER_CORRECS2, "correc_CM.txt")
	FILE_CORREC_CR2 <- file.path(FOLDER_CORRECS2, "correc_CR.txt")
# verification files
FILE_VERIF_NBR_CR <- file.path(FOLDER_VERIFS, "decomptes_CR.txt")
FILE_VERIF_NBR_DE <- file.path(FOLDER_VERIFS, "decomptes_DE.txt")
FILE_VERIF_NBR_S <- file.path(FOLDER_VERIFS, "decomptes_S.txt")
FILE_VERIF_DATES_CD <- file.path(FOLDER_VERIFS, "elections_CD.txt")
FILE_VERIF_DATES_CM <- file.path(FOLDER_VERIFS, "elections_CM.txt")
FILE_VERIF_DATES_CR <- file.path(FOLDER_VERIFS, "elections_CR.txt")
FILE_VERIF_DATES_D <- file.path(FOLDER_VERIFS, "elections_D.txt")
FILE_VERIF_DATES_DE <- file.path(FOLDER_VERIFS, "elections_DE.txt")
FILE_VERIF_DATES_S <- file.path(FOLDER_VERIFS, "elections_S.txt")

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
	# comparison folders
	FOLDER_COMPARISON <- file.path(FOLDER_OUT,"compare_1vs2")
		# table-specific output folders
		FOLDER_COMP_CD <- file.path(FOLDER_COMPARISON, "CD")
		FOLDER_COMP_CM <- file.path(FOLDER_COMPARISON, "CM")
		FOLDER_COMP_CR <- file.path(FOLDER_COMPARISON, "CR")
		FOLDER_COMP_D <- file.path(FOLDER_COMPARISON, "D")
		FOLDER_COMP_DE <- file.path(FOLDER_COMPARISON, "DE")
		FOLDER_COMP_EPCI <- file.path(FOLDER_COMPARISON, "EPCI")
		FOLDER_COMP_M <- file.path(FOLDER_COMPARISON, "M")
		FOLDER_COMP_S <- file.path(FOLDER_COMPARISON, "S")
		FOLDER_COMP_ALL <- file.path(FOLDER_COMPARISON, "_All")
