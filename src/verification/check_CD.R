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




# start logging
start.rec.log(text="CD")

# create output folder
out.folder <- file.path(FOLDER_OUT, "CD")
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- load.cd2.data()
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
#sumup.cols(data=data, cols=cols, out.folder=out.folder)

# plots the number of persons over time time
plot.pers.time(data, out.folder, daily=TRUE)

# check dates
#test.col.dates.cd(data, cols, out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
