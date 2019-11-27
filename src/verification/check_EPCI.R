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
source("src/verification/test_duplicates.R")




# start logging
start.rec.log(text="EPCI")

# create output folder
out.folder <- FOLDER_OUT_EPCI
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- load.epci.data()
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
sumup.cols(data=data, cols=cols, out.folder=out.folder)

# plots the number of persons over time
tlog(0,"Ploting the number of simultaneously hold positions over time")
#plot.pers.time(data, out.folder, daily=TRUE)
plot.pers.time2(data, out.folder)

# check dates
tlog(0,"Checking dates")
test.col.dates.generic(data, cols, out.folder)

# look for duplicates (not really necessary to do that here, better after the merge)
tlog(0,"Looking for duplicates")
test.duplicates(data, out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
