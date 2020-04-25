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


# BESOINS
# - liste des cas de figure complètement interdits en termes de cumul de mandat = quelles sont les positions impossibles à cumuler ?
#   >> on ne peut pas occuper deux positions de même type en même temps (>> semble testable)
#   >> cumul de fonctions : on ne peut pas être président ou maire dans deux conseils différents (>> test reporté à plus tard)

# analyse de séquences
# - gestion temps/evts
#   - évt sans accumulation (pas de distinction entre mandats consécutifs)
#   - evt avec accu (on distingue mandats consécutifs)
#   - on considère la durée du mandat
# - mettre le focus sur un poste donné plutot que sur un individu donné
#   (séquence des personnes ou classes de personnes ayant occupé un poste donné)

# A faire par Émilie
# - Explorer plus profondément la version postgresql de la BD du Sénat
# - Pareil pr BD assemblée
# - Finir de corriger les noms manquants de communautés dans EPCI

# TODO
# voir pq il y a autant de lignes supprimées dans CM vs. CM2
# >> peut être que la correction dans le script de comparaison a résolu le pb ?
# (traitement en cours sur PC LIA)

# TODO
# - passer en revue les fichiers de problèmes: *CM
# - passer en revue les logs: CD, CM, CR, D, DE, EPCI, M, S
# - faire la fusion et re-tester
# - traiter la comparaison approchée des noms

# il existe une BD recensant les EPCI (SIREN)
# https://www.data.gouv.fr/fr/datasets/base-nationale-sur-linter-communalite/
# https://www.collectivites-locales.gouv.fr/liste-et-composition-des-epci-a-fiscalite-propre

# pb repéré sur les figures d'évolution du nbre de mandat *après* corrections
# >> pb à voir, notamment sur DE et D (S ?)
# >> pourtant, on trouve dans la BD S ou AN toutes les lignes présentent dans le RNE, donc il ne devrait pas y avoir de pb...
#    et pr députés, on s'assure de ne pas avoir un poste occupé par plusieurs personnes à la fois...

# TODO tester que la même personne n'occupe pas plusieurs positions en même temps ?
# >> à voir en fonction de la feuille détaillée de Guillaume

# TODO pr EV :
# - remplacer NA par "" partout sauf valeur numériques et dates
# - remplacer NA par NULL dans valeurs numériques et dates

# TODO
# dans AN, certaines personnes ont 2 fonctions en même temps
# ex. dubois

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
# tester l'unicité des nouveaux ids dans la table fusionnée (AN, Senat, PE, etc.)

# TODO
# - dans les tests, ne pas compter/détecter si fonction différentes (même si même mandat)
#   > détecter mandat overlap, puis ne garder que les overlap de fonction (ou pas de fonction)
