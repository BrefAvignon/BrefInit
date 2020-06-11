#############################################################################################
# Main script to process the RNE (historical version from 2018/07/17).
#
# NOTE: it is necessary to convert the original data files to UTF8 in order for these scripts 
# to work correctly on both Windows and Linux.
# 
# 07/2019 Vincent Labatut
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Extraction/Datapol")
# setwd("eclipse/workspaces/Extraction/Datapol")
#############################################################################################
source("src/common/include.R")




# analyse de séquences
# - TODO convertir la table pour qu'elle soit exploitable pour l'analyse de séquences
# - gestion temps/evts
#   - évt sans accumulation (pas de distinction entre mandats consécutifs)
#   - evt avec accu (on distingue mandats consécutifs)
#   - on considère la durée du mandat
# - mettre le focus sur un poste donné plutot que sur un individu donné
#   (séquence des personnes ou classes de personnes ayant occupé un poste donné)
