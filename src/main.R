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




# install stringdist


# en cours
# - compare_CM
# - check_CM : évolution 1
# 
# à faire
# - check_CM : évolution 2



# Points d'implémentation
# - résoudre le problème des mandats concernant la même position mais qui se chevauchent sur une journée
#   >> le mandat qui s'achève est réduit d'une journée pour éviter le recouvrement
# - vérifier les mandats commençant largement avant 2000
#   hypothèse : il s'agirait de séries de mandats consécutifs fusionnés, dont le dernier est soit encore en cours, soit s'arrête après 2001
#   >> redécouper les mandats en fonction des dates des élections
#   >> test concret: sortir tous les mandats commençant avant 2001, et les autres mandats des mêmes personnes
# - utiliser le tableau de NF listant toutes les dates des élections, pour 
#   - corriger tables de vérification avec nombres de postes (CR, S, DE?)
#   - trancher les dates de début/fin de mandat qui se chevauchent d'un jour
#   - découper les mandats consécutifs abusivement fusionnés
# - tester quand fonction est renseignée mais sans date (même pas début)
# - certaines fonctions sont à ignorer lors du test de recouvrement :
#   - maire délégué
#   - autres responsabilités
# - tester quand aucune date de mandat n'est renseignée
# - tester quand, dans une même table, une même personne a un chevauchement de date de fonction
#   elle ne devrait pas cumuler plusieurs fonctions relatives au même type de position
# - CD : après 2015, c'est deux personnes par canton (1 H + 1 F)
#   >> plus possible de faire la vérification de recouvrement de mandat
#   >> par contre vérification possible sur le sexe des conseillers
# - Dans une même table, détecter les lignes telles que seul un attribut optionnel diffère (ou plusieurs, mais tous optionnels)
#   (les attributs obligatoires étant de valeurs égales)
#   >> probablement une entrée qui a été complétée plus tard, garder la ligne la plus complète et supprimer l'autre 
# - réordonner les colonnes dans les sorties fichiers de manière à ce qu'elles soient plus lisibles
# - quand la date de fin de fonction manque, on peut la déduire
#   - de la date de début de fonction suivante
#   - de la date de fin de mandat


# - tester le recouvrement de mandat pour les CM (en plus de celui de fonction, déjà fait)
#   >> impossible à faire car les mandats ne sont pas uniques (tous sont conseillers municipaux) 

# Noms propres
# - normaliser les noms propres : 
#   - majuscules non-accentuées
#   - trim
# - mettre en place la comparaison flexible des noms propres
# - problèmes repérés sur les noms de lieux
#   - VILLE SUD = VILLE-SUD = VILLE - SUD
#     >> Chercher toutes les variantes de la même chaine modulo un séparateur : espace, tiret, apostrophe, slash
#     >> Garder la majoritaire, ou celle qui a le plus de tirets
# - Le même nom apparait parfois avec ou sans le déterminant : LE, LA, L', LES
# - Saint/St, Sainte/Ste, Saints/Sts, Saintes/Stes 
#   >> normaliser en prenant le mot complet systématiquement
# - Présence de chiffres romains dans les noms de cantons (notamment)
#   >> remplacer par des chiffres indo-arabes

# Points relevés par GM/NF mais déjà traités :
# - existence de fonction sans date

# Graphiques
# - intégrer le nombre attendu de mandats dans les graphiques temporels

# Nouvelle extraction
# - traiter CR2

# Besoins
# - liste des cas de figure complètement interdits en termes de cumul de mandat = quelles sont les positions impossibles à cumuler ?
#   - on ne peut pas occuper deux positions de même type en même temps (>> semble testable)
#   - cumul de fonctions : on ne peut pas être président ou maire dans deux conseils différents (>> test reporté à plus tard)

# Cas problématiques détectés
# - Mêmes nom, prénom, ddn mais ids différents
#   >> vrais ou faux homonymes ? se référer au territoire et faire une vérification manuelle
# - Personne occupant des positions incompatibles
#   >> hypothèse : homonymes complets (ou pas) victimes d'une erreur manuelle et rattachés au même ID
# - maire délégué : doublons ou le même mandat apparait 2 fois, une fois en tant que maire délégué ET adjoint
# - Dates pré 2001 sont réparties dans l'année de façon hétérogène : pq ? remplacements ?
# - CD : pq on n'est pas à 4000 dès 2001 ?
#   >> il manque des conseillers
#   >> chaque pic sur le graphique semble correspondre à un renouvellement de la moitié des CD (mais pas tout à fait)

# fonction_dates_problems_mandate
# 1. >> fonction se prolonge au delà de la fin du mandat
#       hypothèse : il y a un mandat suivant, et la fonction devrait y etre aussi
#                   l'ont il indiquée dans ce second mandat ?
#		solution : découper la fonction sur les deux mandats
# 2. erreur marginale
# 3. mandat prend fin avant la fin normale, et la fonction va au bout
#    >> à vérifier manuellement
		
# mandat_dates_problems_bounds
# - hypothèse: visiblement la date de fin de mandat entrée correspond à la date fin de mandat du prédécesseur
# - solution : vérifier si personne d'autre n'occupe la même position sur le reste de la durée de mandat
#	           >> si oui, on étend la date de fin de mandat à la date de durée légale
#	           >> si non, on utilise la date de début de la personne suivant

# le mandat s'achève t il quand l'élu déménage et change de liste électorale ?
# >> question de droit
# >> pourrait permettre de résoudre de cas où un même id possède simultanément des mandats incompatibles
