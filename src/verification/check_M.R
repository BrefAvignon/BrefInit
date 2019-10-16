#############################################################################################
# Checks the mayor tables. 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_M.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_positions.R")




# start logging
start.rec.log(text="M")

# create output folder
out.folder <- file.path(FOLDER_OUT, "M")
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- load.m.data()
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
sumup.cols(data=data, cols=cols, out.folder=out.folder)

# plots the number of persons over time time
#plot.pers.time(data, out.folder, daily=TRUE)

# check dates
#test.col.dates.cm(data, cols, out.folder)

# check overlapping mandates for the same position
#test.position.m(data, out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
