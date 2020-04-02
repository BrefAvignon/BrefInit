#############################################################################################
# Compares two versions of the departmental councilor table. 
# 
# 10/2019 Vincent Labatut
#
# source("src/comparison/compare_CD.R")
#############################################################################################
source("src/common/include.R")




# start logging
start.rec.log(text="CompareCD")

# create output folder
out.folder <- FOLDER_COMP_VERS_CD
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# perform the comparison
tlog(0,"Comparing both extractions of the CD table")
compare.tables(files0=FILES_TAB_CD, files1=FILES_TAB_CD2, out.folder, skip=1)

# close the log file
tlog(0,"Comparison done")
end.rec.log()
