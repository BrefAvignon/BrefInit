#############################################################################################
# Functions used to handle strings.
# 
# 12/2019 Vincent Labatut
#############################################################################################




#############################################################################################
# Takes a string and removes the accents and other French diacritics. Could not find a better
# way of doing this without falling in R's encoding hell.
#
# strings: vector of original strings.
#
# returns: same strings, but without the diacritics.
#############################################################################################
remove.diacritics<- function(strings)
{	result <- strings
	
	map <- c()
	# A
	map["à"] <- "a"
	map["á"] <- "a"
	map["ä"] <- "a"
	map["â"] <- "a"
	map["ã"] <- "a"
	map["å"] <- "a"
	map["ǎ"] <- "a"
	map["ă"] <- "a"
	map["a̋"] <- "a"
	map["ȁ"] <- "a"
	map["À"] <- "A"
	map["Á"] <- "A"
	map["Ä"] <- "A"
	map["Â"] <- "A"
	map["Ã"] <- "A"
	map["Å"] <- "A"
	map["Ǎ"] <- "A"
	map["Ă"] <- "A"
	map["A̋"] <- "A"
	map["Ȁ"] <- "A"
	# AE
	map["æ"] <- "ae"
	map["ǽ"] <- "ae"
	map["æ̀"] <- "ae"
	map["Æ"] <- "ae"
	map["Ǽ"] <- "ae"
	map["Æ̀"] <- "ae"
	# C
	map["ç"] <- "c"
	map["Ç"] <- "C"
	# D
	map["ḑ"] <- "d"
	map["Ḑ"] <- "D"
	# E
	map["è"] <- "e"
	map["é"] <- "e"
	map["ë"] <- "e"
	map["ê"] <- "e"
	map["ẽ"] <- "e"
	map["ȩ"] <- "e"
	map["ě"] <- "e"
	map["ĕ"] <- "e"
	map["e̋"] <- "e"
	map["ȅ"] <- "e"
	map["È"] <- "E"
	map["É"] <- "E"
	map["Ë"] <- "E"
	map["Ê"] <- "E"
	map["Ẽ"] <- "E"
	map["Ȩ"] <- "E"
	map["Ě"] <- "E"
	map["Ĕ"] <- "E"
	map["E̋"] <- "E"
	map["Ȅ"] <- "E"
	# G
	map["ģ"] <- "g"
	map["ğ"] <- "g"
	map["ǧ"] <- "g"
	map["Ģ"] <- "G"
	map["Ğ"] <- "G"
	map["Ǧ"] <- "G"
	# H
	map["ḩ"] <- "h"
	map["Ḩ"] <- "H"
	# I
	map["ì"] <- "i"
	map["í"] <- "i"
	map["ï"] <- "i"
	map["î"] <- "i"
	map["ĩ"] <- "i"
	map["ǐ"] <- "i"
	map["ĭ"] <- "i"
	map["i̋"] <- "i"
	map["ȉ"] <- "i"
	map["Ì"] <- "I"
	map["Í"] <- "I"
	map["Ï"] <- "I"
	map["Î"] <- "I"
	map["Ĩ"] <- "I"
	map["Ǐ"] <- "I"
	map["Ĭ"] <- "I"
	map["I̋"] <- "I"
	map["Ȉ"] <- "I"
	# K
	map["ķ"] <- "k"
	map["Ķ"] <- "K"
	# L
	map["ļ"] <- "l"
	map["Ļ"] <- "L"
	# M
	map["m̋"] <- "m"
	map["M̋"] <- "M"
	# N
	map["ñ"] <- "n"
	map["ņ"] <- "n"
	map["Ñ"] <- "N"
	map["Ņ"] <- "N"
	# O
	map["ò"] <- "o"
	map["ó"] <- "o"
	map["ö"] <- "o"
	map["ô"] <- "o"
	map["õ"] <- "o"
	map["ǒ"] <- "o"
	map["ŏ"] <- "o"
	map["ő"] <- "o"
	map["ȍ"] <- "o"
	map["ø"] <- "o"
	map["Ò"] <- "O"
	map["Ó"] <- "O"
	map["Ö"] <- "O"
	map["Ô"] <- "O"
	map["Õ"] <- "O"
	map["Ǒ"] <- "O"
	map["Ŏ"] <- "O"
	map["Ő"] <- "O"
	map["Ȍ"] <- "O"
	map["Ø"] <- "O"
	# OE
	map["œ"] <- "oe"
	map["Œ"] <- "OE"
	# R
	map["ŗ"] <- "r"
	map["ȑ"] <- "r"
	map["Ŗ"] <- "R"
	map["Ȑ"] <- "R"
	# S
	map["ş"] <- "s"
	map["Ş"] <- "S"
	# T
	map["ţ"] <- "t"
	map["Ţ"] <- "T"
	# U
	map["ù"] <- "u"
	map["ú"] <- "u"
	map["ü"] <- "u"
	map["û"] <- "u"
	map["ũ"] <- "u"
	map["ů"] <- "u"
	map["ǔ"] <- "u"
	map["ŭ"] <- "u"
	map["ű"] <- "u"
	map["ȕ"] <- "u"
	map["Ù"] <- "U"
	map["Ú"] <- "U"
	map["Ü"] <- "U"
	map["Û"] <- "U"
	map["Ũ"] <- "U"
	map["Ů"] <- "U"
	map["Ǔ"] <- "U"
	map["Ŭ"] <- "U"
	map["Ű"] <- "U"
	map["Ȕ"] <- "U"
	# V
	map["ṽ"] <- "v"
	map["Ṽ"] <- "V"
	# W
	map["W̊"] <- "w"
	map["ẘ"] <- "W"
	# Y
	map["ỳ"] <- "y"
	map["ý"] <- "y"
	map["ÿ"] <- "y"
	map["ŷ"] <- "y"
	map["ỹ"] <- "y"
	map["ẙ"] <- "y"
	map["y̏"] <- "y"
	map["Ỳ"] <- "Y"
	map["Ý"] <- "Y"
	map["Ÿ"] <- "Y"
	map["Ŷ"] <- "Y"
	map["Ỹ"] <- "Y"
	map["Y̊"] <- "Y"
	map["Y̏"] <- "Y"
	# Z
	map["ᵶ"] <- "z"
	
	for(m in names(map))
	{	result <- gsub(
				x=result,
				pattern=m,
				replacement=map[m]
			)
	}
	
	# method from SE https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
	# but does not seem to work here...
#	stri_trans_general(str=result, id="Latin-ASCII")
	
	return(result)
}




#############################################################################################
# Takes a string representing a proper noun (place or person) and normalizes it so that it
# is easier to compare. Basically, we remove all non-letter and non-digit characters, consecutive
# whitespaces, and switch to uppercase. Diacritics are supposed to have been removed before.
#
# strings: vector of original strings.
#
# returns: normalized strings.
#############################################################################################
normalize.proper.nouns <- function(strings)
{	result <- strings
	
	# remove punctuation
	result <- gsub(x=result, pattern="[^a-zA-Z1-9]", replacement=" ")
	# remove consecutive whitespaces
	result <- gsub(x=result, pattern=" +", replacement=" ")
	# convert to uppercase
	result <- toupper(result)
	
	return(result)
}
