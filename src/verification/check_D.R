#############################################################################################
# Checks the members of the parliament table. 
# 
# 07/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/BrefInit")
# source("src/verification/check_D.R")
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
complete.data <- TRUE
type <- "D"

# start logging
start.rec.log(text="D")

# create output folder
out.folder <- FOLDER_OUT_D
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
data <- load.d.data(out.folder, correct.data, complete.data)

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
test.col.dates.d(data=data, out.folder=out.folder)

# check locations
tlog(0,"Checking locations")
test.col.locations(data=data, out.folder=out.folder, merged=FALSE)

# check overlapping mandates for the same position
tlog(0,"Checking overlapping mandates for the same position")
test.position.d(data=data, out.folder=out.folder)

# look for duplicates (not really necessary to do that here, better after the merge)
tlog(0,"Looking for duplicates")
test.duplicates(data=data, loc.col=COL_ATT_DPT_CODE, out.folder=out.folder)

# plots the number of persons over time
tlog(0,"Ploting the number of simultaneously hold positions over time")
plot.pers.time(data=data, out.folder=out.folder, type=type)

# close the log file
tlog(0,"Done")
end.rec.log()
