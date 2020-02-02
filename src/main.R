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


# TODO
# 2) étudier la façon dont certaines lignes incluent plusieurs mandats, ou le contraire
#    éprouver l'hypothèse voulant que 1 ligne = 1 fonction (ou absence de)
#    >> pout ça: sortir la liste de lignes pour une position bien identifiés
# 3) forcer le découpage des lignes recouvrant plusieurs mandats
#    >> va requérir de tracer les positions pour CD et S.
#    >> créer un nouveau champ pour stocker l'information ?
# 4) arrondir les bornes de mandats pour éviter les problèmes (recouvrement, mandats de 1 jour, etc.)
#    certains de ces problèmes peuvent être issus du découpage forcé des lignes 

# motif de fin de mandat/fonction sans la date qui va avec

# TODO liste des députés ordonnés par cause de fin de mandat

# refaire certains (tous ?) tests sur la table fusionnée, une fois que les tables individuelles sont propres



# Points d'implémentation
#
# - vérifier qu'un mandat ne traverse pas plusieurs élections :
#   - si date de début de mandat correspond à une élection (premier tour), la date de fin ne doit pas être après l'élection suivante (T1)
#   - forcer les dates de fin/début à coincider avec un tour d'élection ?
# - inverse : certains mandats durent seulement quelques jours, et s'enchaînent avec un "vrai" mandat
#   ex : CD
#   "Code département" "Libellé département" "Code canton" "Libellé canton" "ID élu" "Nom élu" "Prénom élu" "Date naissance" "Code sexe" "Nuance politique" "Code profession" "Libellé profession" "Libellé mandat" "Date début mandat" "Date fin mandat" "Motif fin mandat" "Libellé fonction" "Date début fonction" "Date fin fonction" "Motif fin fonction"
#   "01" "AIN" "36" "VIRIEU-LE-GRAND" "106518" "GUILLON" "Pascale" 1961-11-10 "F" "DVG" "02" "Salariés agricoles" "Conseiller Départemental" 2011-03-27 2011-03-30 NA NA NA NA NA
#   "01" "AIN" "36" "VIRIEU-LE-GRAND" "537590" "LAMAISON" "André" 1934-04-12 "M" "SOC" "65" "Autres retraités" "Conseiller Départemental" 2004-03-28 2011-03-30 "FM" NA NA NA NA
#   >> détecter les mandats de moins d'une semaine ?
#
# - résoudre le problème des mandats concernant la même position mais qui se chevauchent sur une journée
#   >> le mandat qui s'achève est réduit d'une journée pour éviter le recouvrement
# - utiliser le tableau de NF listant toutes les dates des élections, pour 
#   - trancher les dates de début/fin de mandat qui se chevauchent d'un jour
#   - découper les mandats consécutifs abusivement fusionnés
#
# - certaines fonctions sont à ignorer lors du test de recouvrement :
#   - maire délégué
#   - autres responsabilités
#
# - tester quand, dans une même table, une même personne a un chevauchement de date de fonction
#   elle ne devrait pas cumuler plusieurs fonctions relatives au même type de position
#
# - Dans une même table, détecter les lignes telles que seul un attribut optionnel diffère (ou plusieurs, mais tous optionnels)
#   (les attributs obligatoires étant de valeurs égales)
#   >> probablement une entrée qui a été complétée plus tard, garder la ligne la plus complète et supprimer l'autre
#
# - parfois une ligne = une fonction ?
#   >> un même mandat peut être décomposé en plusieurs lignes en cas de prise de fonction en cours de mandat


# En attente de vérification par Noémie et/ou Guillaume
# - vérifier les mandats commençant avant 2001
#   hypothèse : il s'agirait de séries de mandats consécutifs fusionnés, dont le dernier est soit encore en cours, soit s'arrête après 2001
#   >> redécouper les mandats en fonction des dates des élections
# - vérifier pourquoi certaines fonctions sont renseignées mais sans date
#   >> possiblement compléter avec les dates de mandat, 
#      voire les dates des autres fonctions occupées avant ou après
#   - quand la date de fin de fonction manque, on peut la déduire
#     - de la date de début de fonction suivante
#     - de la date de fin de mandat
# - le mandat s'achève-t-il quand l'élu déménage et change de liste électorale ?
#   >> question de droit
#   >> pourrait permettre de résoudre de cas où un même id possède simultanément des mandats incompatibles
# - Dans EPCI: 
#	- parfois pas de commune (ex : tout début de la table, 3-4 premières lignes)
#	- parfois un code commune mais pas de libellé.
#   - même chose pour la commune de *rattachement*
#   >> est-ce normal ? acceptable ? 
# - Codes commune : faut-il conserver ou pas les valeurs du type 013SN03, ou les tronquer ?
#   

# Tests abandonnés
# - tester le recouvrement de mandat pour les CM (en plus de celui de fonction, déjà fait)
#   >> impossible à faire car les mandats ne sont pas uniques (tous sont conseillers municipaux) 

# NOMS PROPRES
# - mettre en place la comparaison flexible des noms propres
# 	>> même personne, prénom légèrement différent
#	   1191040 vs 1191041

# GRAPHIQUES
# - intégrer le nombre attendu de mandats dans les graphiques temporels

# BESOINS
# - liste des cas de figure complètement interdits en termes de cumul de mandat = quelles sont les positions impossibles à cumuler ?
#   >> on ne peut pas occuper deux positions de même type en même temps (>> semble testable)
#   >> cumul de fonctions : on ne peut pas être président ou maire dans deux conseils différents (>> test reporté à plus tard)

# Cas problématiques détectés
# - Personne (même id) occupant des positions incompatibles
#   >> hypothèse : homonymes complets (ou pas) victimes d'une erreur manuelle et rattachés au même ID
# - maire délégué : doublons ou le même mandat apparait 2 fois, une fois en tant que maire délégué ET une fois en tant qu'adjoint
# - Dates pré-2001 sont réparties dans l'année de façon hétérogène : pq ? remplacements ?
# - CD : pq on n'est pas à 4000 dès 2001 ?
#   >> il manque des conseillers
#   >> chaque pic sur le graphique semble correspondre à un renouvellement de la moitié des CD (mais pas tout à fait)

# fonction_dates_problems_mandate
# 1. >> fonction se prolonge au delà de la fin du mandat
#       hypothèse : il y a un mandat suivant, et la fonction devrait y etre aussi
#                   l'ont-il indiquée dans ce second mandat ?
#		solution : découper la fonction sur les deux mandats
# 2. erreur marginale ponctuelle
# 3. mandat prend fin avant la fin normale, et la fonction va au bout
#    >> à vérifier manuellement
		
# remarques étudiants BI
# - Certaines positions sont laissées inoccupées, par ex. commune de Saline n'a pas de maire pdt plusieurs semaines
#   et en même temps, la commune présente un recouvrement du type: plusieurs maires à la fois
#   >> détecter ces vacances et les exploiter pour résoudre ce type de recouvrement ?

# Problèmes détectés
# - Dates de la forme 01/01/xxxx représentant apparemment une absence d'information plus précise
# - Période de fonction pas incluse dans la date de mandat
# - Date de naissance postérieure au début de la fonction/mandat
# - Date de début de la fonction/mandat postérieure à sa date de fin, ou bien pas de date de début alors qu’il y a une date de fin
# - Dates antérieures à 01/01/1900 ou postérieures à 01/01/2020


# la normalisation des noms propres rend certaines corrections inutiles (surtout pour les lieux)
# >> à enlever


# divers
# sortir la chronologie de toutes les positions électives pour chaque élu
# >> identifier les trous et superpositions
# pareil pour les fonctions 
# taux de renouvellement sur une période donnée

# analyse de séquences
# - gestion temps/evts
#   - évt sans accumulation (pas de distinction entre mandats consécutifs)
#   - evt avec accu (on distingue mandats consécutifs)
#   - on considère la durée du mandat
# - mettre le focus sur un poste donné plutot que sur un individu donné
#   (séquence des personnes ou classes de personnes ayant occupé un poste donné)

# vérifier si les triple-homonymes le sont vraiment
# pour les incompatibilités de mandats (plusieurs mandats incompatibles en même temps)
# >> vérifier si ce ne sont pas simplement plusieurs homonymes réels 
#    qui auraient été pris pour la même personne et incorrectement fusionnés

# dans la table fusionnée, quand deux lignes sont pareilles à quelques champs non renseignés prés
# il faut les fusionner
# ex: mandat CM + mandat M équivalent (issus des deux tables)

# lors de la recherche de personnes ayant plusieurs ids, il ne faut pas fusionner ceux qui ont des mandats incompatibles
# >> faire un test ad hoc pour vérifier ça, puis intégrer au test qui fait la fusion
# mais même une id donnée peut avoir plusieurs mandats incompatibles, à cause des erreurs présentes dans la BD
# >> corriger d'abord ces erreurs ? 
