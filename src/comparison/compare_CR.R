#############################################################################################
# Compares two versions of the regional councilor table. 
# 
# 11/2019 Vincent Labatut
#
# source("src/comparison/compare_CR.R")
#############################################################################################
source("src/comparison/compare_tables.R")




# start logging
start.rec.log(text="CompareCR")

# create output folder
out.folder <- FOLDER_COMP_CR
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# perform the comparison
tlog(0,"Comparing both extractions of the CR table")
compare.tables(files0=FILES_TAB_CR, files1=FILES_TAB_CR2, out.folder)

# close the log file
tlog(0,"Comparison done")
end.rec.log()
