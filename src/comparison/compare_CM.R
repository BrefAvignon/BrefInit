#############################################################################################
# Compares two versions of the departmental councilor table. 
# 
# 10/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/BrefInit")
# source("src/comparison/compare_CM.R")
# nohup R --vanilla < src/comparison/compare_CM.R > termout.comparison.txt &
#############################################################################################
source("src/common/include.R")




# start logging
start.rec.log(text="CompareCM")

# create output folder
out.folder <- FOLDER_COMP_VERS_CM
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# perform the comparison
tlog(0,"Comparing both extractions of the CM table")
compare.tables(files0=FILES_TAB_CM, files1=FILES_TAB_CM2, out.folder, skip=1)

# close the log file
tlog(0,"Comparison done")
end.rec.log()
