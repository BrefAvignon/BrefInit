#############################################################
# Functions used to generate plots.
# 
# 09/2024 Vincent Labatut
#############################################################




#############################################################
# Receives a solid color and makes it partially transparent by
# adding an alpha channel.
#
# color: original color.
# transparency: alpha level (percent).
#
# returns: partially transparent color.
#############################################################
make.color.transparent <- function(color, transparency=50)
{	# convert to RGB triplet
	rgb.val <- col2rgb(color)
	
	# create new color using specified transparency
	res <- rgb(
			rgb.val[1], rgb.val[2], rgb.val[3],
			max=255,
			alpha=(100-transparency)*255 / 100
	)
	
	return(res)
}
