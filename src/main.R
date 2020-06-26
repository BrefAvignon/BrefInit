#############################################################################################
# Main script to process the RNE (historical version from 2018/07/17).
#
# NOTE: it is necessary to convert the original data files to UTF8 in order for these scripts 
# to work correctly on both Windows and Linux.
# 
# 07/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/BrefInit")
# setwd("eclipse/workspaces/Extraction/BrefInit")
#############################################################################################
source("src/common/include.R")




############################################
# extract and test all mandate-specific tables
source("src/verification/check_CD.R")
source("src/verification/check_CR.R")
source("src/verification/check_D.R")
source("src/verification/check_DE.R")
source("src/verification/check_M.R")
source("src/verification/check_S.R")
source("src/verification/check_CM.R")
source("src/verification/check_EPCI.R")




############################################
# merge mandate-specific tables to get a single table
source("src/merge/merge.R")
source("src/verification/check_All.R")
