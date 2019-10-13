#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#############################################################################################




#############################################################################################
## folder and file constants
# input folders
FOLDER_IN <- "in"
FOLDER_TABLES <- file.path(FOLDER_IN,"tables")
FOLDER_CORRECS <- file.path(FOLDER_IN,"corrections")
# table files
FILES_TAB_CD <- c("F Tous CD.txt")
FILES_TAB_CM <- c("A Tous CM 01 30.txt", "B Tous CM 31 60.txt", "C Tous CM 61 95.txt", "D Tous CM OM.txt")
FILES_TAB_CR <- c("G Tous CR.txt")
FILES_TAB_D <- c("H Tous Deputes.txt")
FILES_TAB_DE <- c("J Tous RPE.txt")
FILES_TAB_EPCI <- c("E Tous Membres EPCI.txt")
FILES_TAB_M <- c("K Tous Maires.txt")
FILES_TAB_S <- c("I Tous Senateurs.txt")
# correction files
FILE_CORREC_CM <- "correc_CM.txt"
FILE_CORREC_CR <- "correc_CR.txt"
FILE_CORREC_D <- "correc_D.txt"
FILE_CORREC_DE <- "correc_DE.txt"
FILE_CORREC_EPCI <- "correc_EPCI.txt"
FILE_CORREC_M <- "correc_M.txt"
FILE_CORREC_S <- "correc_S.txt"
# additional files, for testing
FILES_TAB_CD2 <- c("2019-10-09 extraction historique CD.txt")
FILE_CORREC_CD2 <- "correc_CD2.txt"

# output folder
FOLDER_OUT <- "out"
# folder to store logs
FOLDER_LOG <- "log"




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
	COL_STATS_ZER, COL_STATS_POS, COL_STATS_MIN, COL_STATS_Q1,
	COL_STATS_MED, COL_STATS_Q3, COL_STATS_MAX, COL_STATS_AVG,
	COL_STATS_STD
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
