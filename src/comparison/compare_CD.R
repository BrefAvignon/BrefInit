#############################################################################################
# Compares two versions of the departmental councilor table. 
# 
# 10/2019 Vincent Labatut
#
# source("src/comparison/compare_CD.R")
#############################################################################################
source("src/comparison/compare_tables.R")




# start logging
start.rec.log(text="CompareCD")

# create output folder
out.folder <- FOLDER_COMP_CD
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# set the files to be compared
tlog(0,"Comparing both extractions of the CD table")
file0 <- FILES_TAB_CD
file1 <- FILES_TAB_CD2

# perform the comparison
compare.tables(files0=file0, files1=file1, out.folder)

# close the log file
tlog(0,"Comparison done")
end.rec.log()
