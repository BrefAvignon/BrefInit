#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#############################################################################################




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
# verification files
FILE_VERIF_CR <- file.path(FOLDER_VERIFS, "verif_CR.txt")
FILE_VERIF_S <- file.path(FOLDER_VERIFS, "verif_S.txt")

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
		FOLDER_OUT_ALL <- file.path(FOLDER_EXTRACT1, "All")
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
		FOLDER_OUT_ALL2 <- file.path(FOLDER_EXTRACT2, "All")
	# comparison folders
	FOLDER_COMPARISON <- file.path(FOLDER_OUT,"compare_1vs2")
		# table-specific output folders
		FOLDER_OUT_CD <- file.path(FOLDER_COMPARISON, "CD")
		FOLDER_OUT_CM <- file.path(FOLDER_COMPARISON, "CM")
		FOLDER_OUT_CR <- file.path(FOLDER_COMPARISON, "CR")
		FOLDER_OUT_D <- file.path(FOLDER_COMPARISON, "D")
		FOLDER_OUT_DE <- file.path(FOLDER_COMPARISON, "DE")
		FOLDER_OUT_EPCI <- file.path(FOLDER_COMPARISON, "EPCI")
		FOLDER_OUT_M <- file.path(FOLDER_COMPARISON, "M")
		FOLDER_OUT_S <- file.path(FOLDER_COMPARISON, "S")
		FOLDER_OUT_ALL <- file.path(FOLDER_COMPARISON, "All")




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
COL_ATT_CANT_CODE <- "Code du canton"
COL_ATT_CANT_NOM <- "Libellé du canton"
COL_ATT_CIRC_CODE <- "Code de la cir.législative"
COL_ATT_CIRC_NOM <- "Libellé de la cir.législative"
COL_ATT_CIRCE_CODE <- "CodeCirER"
COL_ATT_CIRCE_NOM <- "LibelléCirER"
COL_ATT_COM_CODE <- "Code Insee de la commune"
COL_ATT_COM_CODE_EPCI <- "Code de la commune"
COL_ATT_COM_NOM <- "Libellé de la commune"
COL_ATT_COM_NOM_EPCI <- "Libellé commune rattachée"
COL_ATT_COM_POP <- "Population de la commune"
COL_ATT_DPT_CODE <- "Code du département"
COL_ATT_DPT_CODE_COM <- "Code département commune rattachée"
COL_ATT_DPT_CODE_EPCI <- "Code département EPCI"
COL_ATT_DPT_CODE_M <- "Code du département (Maire)"
COL_ATT_DPT_NOM <- "Libellé du département"
COL_ATT_DPT_NOM_CR <- "Libellé de département"
COL_ATT_DPT_NOM_M <- "Libellé de département (Maires)"
COL_ATT_ELU_DDN <- "Date de naissance"
COL_ATT_ELU_ID <- "N° Identification d'un élu"
COL_ATT_ELU_NAT <- "Nationalité de l'élu"
COL_ATT_ELU_NOM <- "Nom de l'élu"
COL_ATT_ELU_PRENOM <- "Prénom de l'élu"
COL_ATT_ELU_SEXE <- "Code sexe"
COL_ATT_ELU_NUANCE <- "Nuance politique"
COL_ATT_ELU_NUANCE_CD <- "Nuance politique (C. Gén.)"
COL_ATT_ELU_NUANCE_CM <- "Nuance politique (C. Mun.)"
COL_ATT_ELU_NUANCE_CR <- "Nuance mandat"
COL_ATT_ELU_NUANCE_D <- "Nuance politique (Député)"
COL_ATT_ELU_NUANCE_DE <- "Nuance politique (Rep. P.E.)"
COL_ATT_ELU_NUANCE_S <- "Nuance politique (Sénateur)"
COL_ATT_EPCI_NOM <- "Libellé de l'EPCI"
COL_ATT_EPCI_SIREN <- "N° SIREN"
COL_ATT_FCT_DBT <- "Date de début de la fonction"
COL_ATT_FCT_FIN <- "Date de fin de la fonction"
COL_ATT_FCT_MOTIF <- "Motif de fin de fonction"
COL_ATT_FCT_NOM <- "Libellé de fonction"
COL_ATT_MDT_DBT <- "Date de début du mandat"
COL_ATT_MDT_FIN <- "Date de fin du mandat"
COL_ATT_MDT_MOTIF <- "Motif de fin de mandat"
COL_ATT_MDT_NOM <- "Libellé de mandat"
COL_ATT_PRO_CODE <- "Code profession"
COL_ATT_PRO_NOM <- "Libellé de la profession"
COL_ATT_REG_CODE <- "Code région"
COL_ATT_REG_NOM <- "Libellé de la région"

# correction files
COL_CORREC_ID <- "Id"
COL_CORREC_NOM <- "Nom"
COL_CORREC_PRENOM <- "Prénom"
COL_CORREC_ATTR <- "Attribut"
COL_CORREC_VALAVT <- "ValAvt"
COL_CORREC_VALAPR <- "ValApr"
COL_CORREC_COMM <- "Commentaire"

# verification files
COL_VERIF_DPT_CODE <- "CodeDpt"
COL_VERIF_DPT_NOM <- "Nom"
COL_VERIF_MDT_NBR <- "Nombre"
COL_VERIF_MDT_DBT <- "Début"
COL_VERIF_MDT_FIN <- "Fin"
COL_VERIF_REG_CODE <- "CodeRégion"
COL_VERIF_REG_NOM <- "Nom"
