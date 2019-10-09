#############################################################################################
# Checks the senator table. 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_R.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")

# start logging
start.rec.log(text="S")

# create output folder
out.folder <- file.path(FOLDER_OUT, "S")
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- load.s.data()
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
check.cols(data=data, cols=cols, out.folder=out.folder)

# plots the number of persons over time time
plot.pers.time(data, out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
