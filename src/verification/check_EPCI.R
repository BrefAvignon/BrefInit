#############################################################################################
# Checks the table related to the councilors for Public Establishment for Intercommunal Co-operation (EPCI). 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_EPCI.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/evolution_plot.R")
source("src/verification/sumup_col.R")
source("src/verification/test_dates.R")
source("src/verification/test_locations.R")
source("src/verification/test_persoinf.R")
source("src/verification/test_duplicates.R")




# set up the extraction
extraction <- 1 # 1 or 2
correct.data <- TRUE

# start logging
start.rec.log(text="EPCI")

# create output folder
out.folder <- FOLDER_OUT_EPCI
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
data <- load.epci.data(correct.data)

# summarizes each column separately
tlog(0,"Examining each column separately")
#sumup.cols(data=data, out.folder=out.folder)

# look for duplicate rows
tlog(0,"Looking for duplicate rows")
#test.compatible.rows(data=data, out.folder=out.folder)

# check personal information
tlog(0,"Checking personal information consistency")
#test.personal.info(data=data, out.folder=out.folder)

# check dates
tlog(0,"Checking dates")
#test.col.dates.epci(data=data, out.folder=out.folder)

# check locations
tlog(0,"Checking locations")
#test.col.locations(data=data, out.folder=out.folder, merged=FALSE)

# look for duplicates (not really necessary to do that here, better after the merge)
tlog(0,"Looking for duplicates")
#test.duplicates(data=data, loc.col=COL_ATT_DPT_CODE, out.folder=out.folder)

# plots the number of persons over time
tlog(0,"Ploting the number of simultaneously hold positions over time")
#plot.pers.time(data=data, out.folder=out.folder, daily=TRUE)
#plot.pers.time2(data=data, out.folder=out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
