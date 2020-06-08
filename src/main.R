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




# TODO revoir l'ordre des ids lors de la vérification des homonymes: ordre numérique
# dans fichiers mais alphabétique dans script, pas compatible.


# analyse de séquences
# - gestion temps/evts
#   - évt sans accumulation (pas de distinction entre mandats consécutifs)
#   - evt avec accu (on distingue mandats consécutifs)
#   - on considère la durée du mandat
# - mettre le focus sur un poste donné plutot que sur un individu donné
#   (séquence des personnes ou classes de personnes ayant occupé un poste donné)

# TODO
# - passer en revue les fichiers de problèmes: *CM
# - passer en revue les logs: CD, CM, CR, D, DE, EPCI, M, S
# - faire la fusion et re-tester
# - traiter la comparaison approchée des noms

# pb repéré sur les figures d'évolution du nbre de mandat *après* corrections
# >> pb à voir, notamment sur DE et D (S ?)
# >> pourtant, on trouve dans la BD S ou AN toutes les lignes présentent dans le RNE, donc il ne devrait pas y avoir de pb...
#    et pr députés, on s'assure de ne pas avoir un poste occupé par plusieurs personnes à la fois...

