#############################################################################################
# Checks the table related to the councilors for Public Establishment for Intercommunal Co-operation (EPCI). 
# 
# 07/2019 Vincent Labatut
#
# source("src/verification/check_EPCI.R")
#############################################################################################
source("src/common/include.R")
source("src/verification/sumup_col.R")

# start logging
start.rec.log(text="EPCI")

# create output folder
out.folder <- file.path(FOLDER_OUT, "EPCI")
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# load the data
tmp <- load.epci.data()
data <- tmp$data
cols <- tmp$cols

# summarizes each column separately
tlog(0,"Examining each column separately")
check.cols(data=data, cols=cols, out.folder=out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
