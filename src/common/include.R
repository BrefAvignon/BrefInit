#############################################################################################
# Loads all the scripts of this project, in the appropriate order.
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

source("src/comparison/compare_tables.R")

source("src/load/apply_corrections.R")
source("src/load/integrate_assnat.R")
source("src/load/integrate_manual.R")
source("src/load/integrate_senat.R")
source("src/load/load_rne.R")




#############################################################################################
# global options

# whether or not to cache cleaned data tables
CACHE_DATA <- TRUE

# parallel computing options
options(future.globals.maxSize=800*1024^2)	# max limit for future global env is 800 MB
plan(multiprocess, workers=CORE.NBR/2)		# set the number of processor cores used
