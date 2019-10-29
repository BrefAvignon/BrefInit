#############################################################################################
# Checks the municipal councilor tables. 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_CM.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_positions.R")
source("src/verification/test_duplicates.R")




# set up the extraction
extraction <- 2 # 1 or 2

# start logging
start.rec.log(text=paste0("CM",extraction))

# create output folder
out.folder <- if(extraction==1) FOLDER_OUT_CM else FOLDER_OUT_CM2
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- if(extraction==1) load.cm.data() else load.cm2.data() 
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
sumup.cols(data=data, cols=cols, out.folder=out.folder)

# plots the number of persons over time
#plot.pers.time(data, out.folder, daily=TRUE)

# check dates
test.col.dates.cm(data, cols, out.folder)

# check overlapping mandates for the same position
test.position.cm(data, out.folder)

# look for duplicates (not really necessary to do that here, better after the merge)
test.duplicates(data, out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
