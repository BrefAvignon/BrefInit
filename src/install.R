#############################################################################################
# Install all the packages necessary to these scripts.
# 
# 09/2019 Vincent Labatut
# source("src/install.R")
#############################################################################################




# comparison of character strings
install.packages("stringdist")

# parallel computation
install.packages("future.apply")

# solves certain encoding problems when loading tables
# https://dss.iq.harvard.edu/blog/escaping-character-encoding-hell-r-windows
#install.packages("readr") # not used, in the end...
