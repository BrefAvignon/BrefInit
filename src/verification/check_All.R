#############################################################################################
# Checks the merged table. 
# 
# 03/2020 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/BrefInit")
# source("src/verification/check_All.R")
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

# start logging
start.rec.log(text=paste0("ALL",extraction))

# create output folder
out.folder <- FOLDER_OUT_ALL
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)

# init stats table
init.stat.table(out.folder)

# load the data
data <- read.cached.table(cache.file=FILE_CACHE_ALL)
#print(sapply(1:ncol(data),function(col) class(data[,col])))
#idx <- which(data[,COL_ATT_ELU_ID_RNE] %in% c(37661,1169957));print(data[idx,])	# debug

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
test.col.dates.all(data=data, out.folder=out.folder)

# check locations
tlog(0,"Checking locations")
test.col.locations(data=data, out.folder=out.folder, merged=TRUE)

# look for duplicates
tlog(0,"Looking for duplicates")
test.duplicates(data=data, loc.col=COL_ATT_DPT_CODE, out.folder=out.folder)

# close the log file
tlog(0,"Done")
end.rec.log()
