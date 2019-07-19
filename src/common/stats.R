#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 07/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Computes the statistical mode(s) of a discrete variable.
# Code taken from https://stackoverflow.com/a/8189441/1254730
#
# vals: values of the variable.
#
# returns: a vector containing the mode(s) of the variable.
#############################################################################################
stat.mode <- function(vals)
{	uq <- unique(vals)
	tab <- tabulate(match(vals, uq))
	result <- uq[tab == max(tab)]
	result <- sort(result)
	return(result)
}
