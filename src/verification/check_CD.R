#############################################################################################
# Checks the departmental councilor table. 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_CD.R")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_locations.R")
source("src/verification/test_persoinf.R")
source("src/verification/test_positions.R")
source("src/verification/test_duplicates.R")




# set up the extraction
extraction <- 1 # 1 or 2
correct.data <- TRUE
complete.data <- FALSE
type <- "CD"

# start logging
start.rec.log(text=paste0("CD",extraction))

# create output folder
out.folder <- if(extraction==1) FOLDER_OUT_CD else FOLDER_OUT_CD2
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
data <- if(extraction==1) load.cd.data(correct.data, complete.data) else load.cd2.data(correct.data, complete.data)

# summarizes each column separately
tlog(0,"Examining each column separately")
sumup.cols(data=data, out.folder=out.folder)

# look for duplicate rows
tlog(0,"Looking for duplicate rows")
test.compatible.rows(data=data, out.folder=out.folder)

# check personal information
tlog(0,"Checking personal information consistency")
test.personal.info(data=data, out.folder=out.folder)

# check dates
tlog(0,"Checking dates")
test.col.dates.cd(data=data, out.folder=out.folder)

# check locations
tlog(0,"Checking locations")
test.col.locations(data=data, out.folder=out.folder, merged=FALSE)

# check overlapping mandates for the same position
tlog(0,"Checking overlapping mandates for the same position")
test.position.cd(data=data, out.folder=out.folder)

# look for duplicates (not really necessary to do that here, better after the merge)
tlog(0,"Looking for duplicates")
test.duplicates(data=data, loc.col=COL_ATT_DPT_CODE, out.folder=out.folder)

# plots the number of persons over time
tlog(0,"Ploting the number of simultaneously hold positions over time")
plot.pers.time(data=data, out.folder=out.folder, type=type)

# close the log file
tlog(0,"Done")
end.rec.log()
