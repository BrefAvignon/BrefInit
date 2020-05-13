#############################################################################################
# Compares two versions of the regional councilor table. 
# 
# 11/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
# source("src/comparison/compare_CR.R")
#############################################################################################
source("src/common/include.R")




# start logging
start.rec.log(text="CompareCR")

# create output folder
out.folder <- FOLDER_COMP_VERS_CR
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# perform the comparison
tlog(0,"Comparing both extractions of the CR table")
compare.tables(files0=FILES_TAB_CR, files1=FILES_TAB_CR2, out.folder, skip=1)

# close the log file
tlog(0,"Comparison done")
end.rec.log()
