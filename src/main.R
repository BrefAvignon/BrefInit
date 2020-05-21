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
# voir pq il y a autant de lignes supprimées dans CM vs. CM2
# >> peut être que la correction dans le script de comparaison a résolu le pb ?
# (traitement en cours sur PC LIA)

# TODO
# - passer en revue les fichiers de problèmes: *CM
# - passer en revue les logs: CD, CM, CR, D, DE, EPCI, M, S
# - faire la fusion et re-tester
# - traiter la comparaison approchée des noms

# pb repéré sur les figures d'évolution du nbre de mandat *après* corrections
# >> pb à voir, notamment sur DE et D (S ?)
# >> pourtant, on trouve dans la BD S ou AN toutes les lignes présentent dans le RNE, donc il ne devrait pas y avoir de pb...
#    et pr députés, on s'assure de ne pas avoir un poste occupé par plusieurs personnes à la fois...

#################################
# Check logs and files
# position test D/CD/
#################################
# Desktop
# C1=testing positions: S/M...CM
# C2=caching *CM
# C3=testing positions: CR
# C4=
#################################
# Zenbook S:
# C1=
# C2=
# C3=
# C4=
#################################

# - rajouter sarko dans les députés
 
# TODO
# - tester l'unicité des nouveaux ids dans la table fusionnée (AN, Senat, PE, etc.)
# - vérifier que la subst des noms de partis a marché dans CM
#   >> dupl.nuances <- c("RDG"="PRG", "M-NC"="MAJ")

# TODO table fusionnée
# - compléter les champs parfois vides (données perso ? autres ?)

# TODO
# - on dirait que la ré-application des corrections standard fait encore évoluer certaines tables (CM ? M ?)
#   >> à regarder
# - fusionner M et CM à part, réappliquer tout le traitement ? ou seulement sur les maires ?
