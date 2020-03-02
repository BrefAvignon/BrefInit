#############################################################################################
# Loads all the script of this folder, in the appropriate order.
# 
# 07/2019 Vincent Labatut
#
# source("src/common/include.R")
#############################################################################################




#############################################################################################
# handling of warnings
#options(warn=1)			# as they happen
options(warn=2)				# as errors
#options(error=recover)		# debug




#############################################################################################
# packages

# load data
#library("readr")						# not necessary, in the end

# string manipulation
library("stringdist")
#library("stringi")						# not used, in the end

# parallel computing
library("parallel")
library("future.apply")
plan(multiprocess, workers=CORE.NBR/2)	# set the number of processor cores used

# XML files								# used to import the parliament DB
library("XML")
library("methods")




#############################################################################################
# source code
source("src/common/constants.R")
source("src/common/dates.R")
source("src/common/stats.R")
source("src/common/strings.R")
source("src/common/logging.R")
source("src/common/stats.R")

source("src/load/integrate_assnat.R")
source("src/load/integrate_senat.R")
source("src/load/load_rne.R")
