#############################################################################################
# Checks the departmental councilor table. 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_CD.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_positions.R")
source("src/verification/test_duplicates.R")




# start logging
start.rec.log(text="CD")

# create output folder
out.folder <- FOLDER_OUT_CD
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- load.cd.data()
#tmp <- load.cd2.data()	#TODO check the second version of the table
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
#sumup.cols(data=data, cols=cols, out.folder=out.folder)

# plots the number of persons over time
#plot.pers.time(data, out.folder, daily=TRUE)

# check dates
#test.col.dates.cd(data, cols, out.folder)

# check overlapping mandates for the same position
#test.position.cd(data, out.folder)

# look for duplicates
# not really necessary to do that here, better after the merge
test.duplicates(data, out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
