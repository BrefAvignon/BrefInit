#############################################################################################
# Main script to process the RNE (historical version from 2018/07/17).
# 
# 07/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")




# install stringdist


# TODO
# - Plots the evolution of each type of mandate over time
#	>> done, but now do it for each day (in addition to months)
# - Implement Noemie's consistancy tests
#
# - Date problems:
#   - Some years are just wrong (patterns /00 for 0010 and /020 for 0201)
#	- Birthdate set at 1/1/1900 probably means "unknown"
