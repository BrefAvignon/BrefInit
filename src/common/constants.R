#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#############################################################################################




#############################################################################################
## folder and file constants
# input folder
FOLDER_IN <- "in"
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
