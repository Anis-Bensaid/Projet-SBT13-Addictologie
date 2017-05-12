## Nettoyage de l'espace de travail
rm(list=ls())

install.packages("readxl")
install.packages("FactoMineR") 
install.packages("factoextra")
# install.packages("VIM") 
source("http://bioconductor.org/biocLite.R") # essayer avec http:// if not supported
biocLite("impute") #équivalent de install.packages

library(readxl)
library(FactoMineR)
library(factoextra)
library(impute)

#library(VIM) #Ne marche pas sur toutes les machines, ce n'est pas un package important.

#####################################
### Lecture de la base de données ###
#####################################


## Anis
# setwd("D:/Users/enysb/Google Drive/Etudes/Git/Projet-SBT13-Addictologie")
# bd <- read_excel("D:/Users/enysb/Google Drive/Etudes/Git/Projet-SBT13-Addictologie/bdmieRpp2.xls")


## Arthur
# setwd("~/Documents/Projet Enjeux/Projet-SBT13-Addictologie")
# bd <- read_excel("~/Documents/Projet Enjeux/Projet-SBT13-Addictologie/bdmieRpp2.xls")


## Benjamin
# setwd("~/GitHub/Projet-SBT13-Addictologie")
# bd<- read_excel("~/GitHub/Projet-SBT13-Addictologie/bdmieRpp2.xls")


## Emilio
# setwd("C:/Users/Emilio/Desktop/intercambio/clases/enjeux/sbt/Projet-SBT13-Addictologie")
# bd <- read_excel("C:/Users/Emilio/Desktop/intercambio/clases/enjeux/sbt/Projet-SBT13-Addictologie/bdmieRpp2.xls")


## Haim
# setwd("~/Desktop/Projet_SBT13/Projet-SBT13-Addictologie-Github")
# bd <- read_excel("~/Desktop/Projet_SBT13/Projet-SBT13-Addictologie-Github/bdmieRpp2.xls")


## Autre utilisateur
setwd("Chemin") #Il faut remplacer "Chemin" par le chemin de la base bdmieRpp2.xls. Vous pouvez aussi utiliser le bouton Session -> Set Working Directory
bd <- read_excel("bdmieRpp2.xls") #Vous pouvez utiliser le bouton File->Import Dataset->From Excel


#############################################
### Restructuration de la base de données ###
#############################################

# On ne séléctionne que les personnes âgées de moins de 31 ans.
bd1 <-bd[bd$age<31,]


Nl=dim(bd1)[1] #nombre de lignes

#On crée bdscore, dataframe qui contiendra la conversion des réponses en score.
bdscore=data.frame(matrix(data=NA,nrow=Nl,ncol=1))

# ID de l'individu interrogé et du collecteur
# on n'a pas besoin d'utiliser les ID car toutes les données sont rassemblées dans un unique tableau
bdscore$ID_indiv <-bd1[1]
# bdscore$collecteur <- bd1[2]

# Suppression d'une colonne inutile :
bdscore<-bdscore[,-1]

# On transforme les réponses de l'AQOLS en score et on les insére dans la data.frame. 
# L'utilisation de unique permet de contourner tout problème du à l'encodage du script. Noter que nous utilisons UTF-8.


# Age
bdscore$Age<-bd1$age
# Genre
genreunique <- unique(bd1$sex)
bdscore$Genre <- ifelse(bd1$sex== genreunique[2], 1, ifelse(bd1$sex==genreunique[1], 2,ifelse(bd1$sex== genreunique[5], NA ,ifelse(bd1$sex == genreunique[3], NA,NA))))
# Niveau d'étude après le Bac
niveauunique <- unique(bd1$niv)
bdscore$Niveau <- ifelse(bd1$niv==niveauunique[3], 1, ifelse(bd1$niv==niveauunique[1], 2, ifelse(bd1$niv==niveauunique[4],3, ifelse(bd1$niv==niveauunique[5], 4, ifelse(bd1$niv==niveauunique[6],5, ifelse(bd1$niv==niveauunique[2], 6, NA))))))

## Nivautr (dans le tableau bd1)
# c'est une colonne vide, elle n'a été remplie que par 6 personnes (4 ont répondu 0, 2 ont répondu 9)

# Discipline
# bdscore$Disc<-bd1$disc --> colonne qualitative, qu'on a préféré scinder en plusieurs 
# colonnes avec un résultat qualitatif
uniquestudy <- unique(bd1$disc)
bdscore$StudyHuma <- ifelse(bd1$disc==uniquestudy[3],1,NA)
bdscore$StudyHuma[is.na(bdscore$StudyHuma)]<-0
bdscore$StudyProf <- ifelse(bd1$disc==uniquestudy[1],1,NA)
bdscore$StudyProf[is.na(bdscore$StudyProf)]<-0
bdscore$StudyLawEco <- ifelse(bd1$disc==uniquestudy[4],1,NA)
bdscore$StudyLawEco[is.na(bdscore$StudyLawEco)]<-0
bdscore$StudyScience <- ifelse(bd1$disc==uniquestudy[2],1,NA)
bdscore$StudyScience[is.na(bdscore$StudyScience)]<-0
bdscore$StudyMed <- ifelse(bd1$disc==uniquestudy[5],1,NA)
bdscore$StudyMed[is.na(bdscore$StudyMed)]<-0
bdscore$StudyAutre <- ifelse(bd1$disc==uniquestudy[6],1,NA)
bdscore$StudyAutre[is.na(bdscore$StudyAutre)]<-0

# Autre cursus, c'est une donnée qualitative qui nous semble inutilisable
# bdscore$AutreCursus <- bd1[8]


# Fréquence binge-drinking
# Si une frqoh="Jamais" FreqBinge reçoit 0, de même si binge="non". Sinon on affecte un score allant de 1 à 5.
bdscore$FreqBinge <- ifelse(bd1$frqoh==unique(bd1$frqoh)[6], 0, ifelse(bd1$binge==unique(bd1$binge)[2], 0, ifelse(is.na(bd1$frqb1),ifelse(is.na(bd1$frqb2),ifelse(is.na(bd1$frqb3), ifelse(is.na(bd1$frqb6),ifelse(is.na(bd1$frqb10),NA,5),4),3),2),1)))

# Nombre maximum de verres que vous avez bu en UNE occasion
summary(bd1$max1occ) # le maximum est de 120 ce qui semble abhérrant, mais tout dépend de ce qu'on appelle occasion et de la durée sur le temps.
hist(bd1$max1occ,  breaks = 30, xlab= "maximal number of alcohol units in one occasion", main="histogram of frequency of maximal number of alcohol units in one occasion ")
summary(bd1$max1occ>30) #Seulement 34 personnes ont répondu plus de 30 verres en une occasion. Nous avons décidé de tout garder.
bdscore$NbMaxOcc <- bd1$max1occ

# Audit-C 
# Fréquence de consommation d'alcool
frqohunique <- unique(bd1$frqoh)
bdscore$FreqConso <- ifelse(bd1$frqoh==frqohunique[6], 0, ifelse(bd1$frqoh==frqohunique[3], 1, ifelse(bd1$frqoh== frqohunique[2], 2, ifelse(bd1$frqoh == frqohunique[1], 3, ifelse(bd1$frqoh==frqohunique[4], 4, NA)))))
# Nombre de verres consommés en moyenne à une occasion
nbverreunique <- unique(bd1$nbvrtyp)
bdscore$NbVerreMoy <- ifelse(bd1$nbvrtyp==nbverreunique[4], 0, ifelse(bd1$nbvrtyp ==nbverreunique[3], 1, ifelse(bd1$nbvrtyp == nbverreunique[2], 2, ifelse(bd1$nbvrtyp == nbverreunique[5], 3, ifelse(bd1$nbvrtyp ==nbverreunique[1], 4, NA)))))
# Fréquence de consommation de plus de six verres en une occasion
bdscore$FreqSupSixVerre <-bd1$sixvr


# AQoLs
a1unique <- unique(bd1$A1)
bdscore$a1 <- ifelse(bd1$A1== a1unique[1], 0, ifelse(bd1$A1== a1unique[2], 1,ifelse(bd1$A1==a1unique[4], 2,ifelse(bd1$A1==a1unique[3], 3,NA))))

a2unique <- unique(bd1$A2)
bdscore$a2 <- ifelse(bd1$A2== a2unique[1], 0, ifelse(bd1$A2== a2unique[3], 1,ifelse(bd1$A2==a2unique[2], 2,ifelse(bd1$A2==a2unique[4], 3,NA))))

a3unique <- unique(bd1$A3)
bdscore$a3 <- ifelse(bd1$A3== a3unique[1], 0, ifelse(bd1$A3== a3unique[2], 1,ifelse(bd1$A3==a3unique[3], 2,ifelse(bd1$A3==a3unique[4], 3,NA))))

a4unique <- unique(bd1$A4)
bdscore$a4 <- ifelse(bd1$A4== a4unique[1], 0, ifelse(bd1$A4== a4unique[2], 1,ifelse(bd1$A4==a4unique[3], 2,ifelse(bd1$A4==a4unique[4], 3,NA))))

a5unique <- unique(bd1$A5)
bdscore$a5 <- ifelse(bd1$A5== a5unique[1], 0, ifelse(bd1$A5== a5unique[2], 1,ifelse(bd1$A5==a5unique[3], 2,ifelse(bd1$A5==a5unique[4], 3,NA))))

a6unique <- unique(bd1$A6)
bdscore$a6 <- ifelse(bd1$A6== a6unique[1], 0, ifelse(bd1$A6== a6unique[2], 1,ifelse(bd1$A6==a6unique[3], 2,ifelse(bd1$A6==a6unique[4], 3,NA))))

a7unique <- unique(bd1$A7)
bdscore$a7 <- ifelse(bd1$A7== a7unique[1], 0, ifelse(bd1$A7== a7unique[2], 1,ifelse(bd1$A7==a7unique[3], 2,ifelse(bd1$A7==a7unique[4], 3,NA))))

a8unique <- unique(bd1$A8)
bdscore$a8 <- ifelse(bd1$A8== a8unique[1], 0, ifelse(bd1$A8== a8unique[2], 1,ifelse(bd1$A8==a8unique[3], 2,ifelse(bd1$A8==a8unique[4], 3,NA))))

a9unique <- unique(bd1$A9)
bdscore$a9 <- ifelse(bd1$A9== a9unique[1], 0, ifelse(bd1$A9== a9unique[2], 1,ifelse(bd1$A9==a9unique[3], 2,ifelse(bd1$A9==a9unique[4], 3,NA))))

a10unique <- unique(bd1$A10)
bdscore$a10 <- ifelse(bd1$A10== a10unique[1], 0, ifelse(bd1$A10== a10unique[2], 1,ifelse(bd1$A10==a10unique[3], 2,ifelse(bd1$A10==a10unique[4], 3,NA))))

a11unique <- unique(bd1$A11)
bdscore$a11 <- ifelse(bd1$A11== a11unique[1], 0, ifelse(bd1$A11== a11unique[2], 1,ifelse(bd1$A11==a11unique[3], 2,ifelse(bd1$A11==a11unique[4], 3,NA))))

a12unique <- unique(bd1$A12)
bdscore$a12 <- ifelse(bd1$A12== a12unique[1], 0, ifelse(bd1$A12== a12unique[3], 1,ifelse(bd1$A12==a12unique[2], 2,ifelse(bd1$A12==a12unique[4], 3,NA))))

a13unique <- unique(bd1$A13)
bdscore$a13 <- ifelse(bd1$A13== a13unique[1], 0, ifelse(bd1$A13== a13unique[2], 1,ifelse(bd1$A13==a13unique[3], 2,ifelse(bd1$A13==a13unique[4], 3,NA))))

a14unique <- unique(bd1$A14)
bdscore$a14 <- ifelse(bd1$A14== a14unique[1], 0, ifelse(bd1$A14== a14unique[2], 1,ifelse(bd1$A14==a14unique[3], 2,ifelse(bd1$A14==a14unique[4], 3,NA))))

a15unique <- unique(bd1$A15)
bdscore$a15 <- ifelse(bd1$A15== a15unique[1], 0, ifelse(bd1$A15== a15unique[2], 1,ifelse(bd1$A15==a15unique[3], 2,ifelse(bd1$A15==a15unique[4], 3,NA))))

a16unique <- unique(bd1$A16)
bdscore$a16 <- ifelse(bd1$A16== a16unique[1], 0, ifelse(bd1$A16== a16unique[2], 1,ifelse(bd1$A16==a16unique[3], 2,ifelse(bd1$A16==a16unique[4], 3,NA))))

a17unique <- unique(bd1$A17)
bdscore$a17 <- ifelse(bd1$A17== a17unique[2], 0, ifelse(bd1$A17== a17unique[1], 1,ifelse(bd1$A17==a17unique[3], 2,ifelse(bd1$A17==a17unique[4], 3,NA))))

a18unique <- unique(bd1$A18)
bdscore$a18 <- ifelse(bd1$A18== a18unique[1], 0, ifelse(bd1$A18== a18unique[2], 1,ifelse(bd1$A18==a18unique[3], 2,ifelse(bd1$A18==a18unique[4], 3,NA))))

a19unique <- unique(bd1$A19)
bdscore$a19 <- ifelse(bd1$A19== a19unique[1], 0, ifelse(bd1$A19== a19unique[2], 1,ifelse(bd1$A19==a19unique[3], 2,ifelse(bd1$A19==a19unique[4], 3,NA))))

a20unique <- unique(bd1$A20)
bdscore$a20 <- ifelse(bd1$A20== a20unique[1], 0, ifelse(bd1$A20== a20unique[3], 1,ifelse(bd1$A20==a20unique[2], 2,ifelse(bd1$A20==a20unique[4], 3,NA))))

a21unique <- unique(bd1$A21)
bdscore$a21 <- ifelse(bd1$A21== a21unique[1], 0, ifelse(bd1$A21== a21unique[2], 1,ifelse(bd1$A21==a21unique[3], 2,ifelse(bd1$A21==a21unique[4], 3,NA))))

a22unique <- unique(bd1$A22)
bdscore$a22 <- ifelse(bd1$A22== a22unique[2], 0, ifelse(bd1$A22== a22unique[1], 1,ifelse(bd1$A22==a22unique[3], 2,ifelse(bd1$A22==a22unique[4], 3,NA))))

a23unique <- unique(bd1$A23)
bdscore$a23 <- ifelse(bd1$A23== a23unique[1], 0, ifelse(bd1$A23== a23unique[2], 1,ifelse(bd1$A23==a23unique[3], 2,ifelse(bd1$A23==a23unique[4], 3,NA))))

a24unique <- unique(bd1$A24)
bdscore$a24 <- ifelse(bd1$A24== a24unique[2], 0, ifelse(bd1$A24== a24unique[1], 1,ifelse(bd1$A24==a24unique[4], 2,ifelse(bd1$A24==a24unique[3], 3,NA))))

a25unique <- unique(bd1$A25)
bdscore$a25 <- ifelse(bd1$A25== a25unique[1], 0, ifelse(bd1$A25== a25unique[2], 1,ifelse(bd1$A25==a25unique[4], 2,ifelse(bd1$A25==a25unique[3], 3,NA))))

a26unique <- unique(bd1$A26)
bdscore$a26 <- ifelse(bd1$A26== a26unique[2], 0, ifelse(bd1$A26== a26unique[1], 1,ifelse(bd1$A26==a26unique[4], 2,ifelse(bd1$A26==a26unique[3], 3,NA))))

a27unique <- unique(bd1$A27)
bdscore$a27 <- ifelse(bd1$A27== a27unique[1], 0, ifelse(bd1$A27== a27unique[2], 1,ifelse(bd1$A27==a27unique[3], 2,ifelse(bd1$A27==a27unique[4], 3,NA))))

a28unique <- unique(bd1$A28)
bdscore$a28 <- ifelse(bd1$A28== a28unique[1], 0, ifelse(bd1$A28== a28unique[2], 1,ifelse(bd1$A28==a28unique[3], 2,ifelse(bd1$A28==a28unique[4], 3,NA))))

a29unique <- unique(bd1$A29)
bdscore$a29 <- ifelse(bd1$A29== a29unique[1], 0, ifelse(bd1$A29== a29unique[2], 1,ifelse(bd1$A29==a29unique[4], 2,ifelse(bd1$A29==a29unique[3], 3,NA))))

a30unique <- unique(bd1$A30)
bdscore$a30 <- ifelse(bd1$A30== a30unique[1], 0, ifelse(bd1$A30== a30unique[2], 1,ifelse(bd1$A30==a30unique[3], 2,ifelse(bd1$A30==a30unique[4], 3,NA))))

a31unique <- unique(bd1$A31)
bdscore$a31 <- ifelse(bd1$A31== a31unique[1], 0, ifelse(bd1$A31== a31unique[2], 1,ifelse(bd1$A31==a31unique[3], 2,ifelse(bd1$A31==a31unique[4], 3,NA))))

a32unique <- unique(bd1$A32)
bdscore$a32 <- ifelse(bd1$A32== a32unique[1], 0, ifelse(bd1$A32== a32unique[2], 1,ifelse(bd1$A32==a32unique[3], 2,ifelse(bd1$A32==a32unique[4], 3,NA))))

a33unique <- unique(bd1$A33)
bdscore$a33 <- ifelse(bd1$A33== a33unique[1], 0, ifelse(bd1$A33== a33unique[2], 1,ifelse(bd1$A33==a33unique[3], 2,ifelse(bd1$A33==a33unique[4], 3,NA))))

a34unique <- unique(bd1$A34)
bdscore$a34 <- ifelse(bd1$A34== a34unique[2], 0, ifelse(bd1$A34== a34unique[1], 1,ifelse(bd1$A34==a34unique[3], 2,ifelse(bd1$A34==a34unique[4], 3,NA))))

## Mes études et l'alcool
negresunique <- unique(bd1$negres)
bdscore$Etudes <- ifelse(bd1$negres==negresunique[1], 0, ifelse(bd1$negres==negresunique[2], 1, ifelse(bd1$negres==negresunique[3], 2, ifelse(bd1$negres==negresunique[4], 3, NA))))


## Autres consommations ou pratiques
tabacunique <- unique(bd1$tbc)
bdscore$Tabac <- ifelse(bd1$tbc== tabacunique[3], 0, ifelse(bd1$tbc== tabacunique[4], 1, ifelse(bd1$tbc==tabacunique[2], 1, ifelse(bd1$tbc==tabacunique[5], 2, ifelse(bd1$tbc==tabacunique[1], 3, NA)))))

cannabisunique <- unique(bd1$thc)
bdscore$Cannabis <- ifelse(bd1$thc== cannabisunique[4], 0, ifelse(bd1$thc== cannabisunique[5], 1, ifelse(bd1$thc==cannabisunique[3], 1, ifelse(bd1$thc==cannabisunique[1], 2, ifelse(bd1$thc==cannabisunique[2], 3, NA)))))

cocaunique <- unique(bd1$coc)
bdscore$Cocaine <- ifelse(bd1$coc== cocaunique[2], 0, ifelse(bd1$coc == cocaunique[4], 1, ifelse(bd1$coc== cocaunique[1], 1, ifelse(bd1$coc==cocaunique[5], 2, ifelse(bd1$coc==cocaunique[6], 3, NA)))))

herounique<- unique(bd1$hero)
bdscore$Heroine <- ifelse(bd1$hero== herounique[2], 0, ifelse(bd1$hero== herounique[1], 1, ifelse(bd1$hero==herounique[6], 1, ifelse(bd1$hero==herounique[7], 2, ifelse(bd1$hero==herounique[4], 3, NA)))))

MDunique<-unique(bd1$md)
bdscore$MD <- ifelse(bd1$md== MDunique[2], 0, ifelse(bd1$md== MDunique[4], 1, ifelse(bd1$md==MDunique[1], 1, ifelse(bd1$md==MDunique[6], 2, ifelse(bd1$md==MDunique[5], 3, NA)))))

popunique<-unique(bd1$pop)
bdscore$Poppers <- ifelse(bd1$pop== popunique[1], 0, ifelse(bd1$pop== popunique[2], 1, ifelse(bd1$pop==popunique[4], 1, ifelse(bd1$pop==popunique[6], 2, ifelse(bd1$pop==popunique[5], 3, NA)))))

jeuunique<-unique(bd1$jeu)
bdscore$Jeu <- ifelse(bd1$jeu== jeuunique[1], 0, ifelse(bd1$jeu== jeuunique[2], 1, ifelse(bd1$jeu==jeuunique[6], 1, ifelse(bd1$jeu==jeuunique[4], 2, ifelse(bd1$jeu==jeuunique[5], 3, NA)))))

## La façon dont je me perçois : “partier” self-concept 
# Faire la fête fait partie de l'image que j'ai de moi
bdscore$FeteImagePerso <- bd1$idt1
# Faire la fête fait partie de "qui je suis"
bdscore$FeteEtre <- bd1$idt2
# Faire la fête fait partie de ma personnalité
bdscore$FetePerso <-bd1$idt3
# Faire la fête fait partie de mon quotidien
bdscore$FeteQuotidien <- bd1$idt4
# Les autres considérent que faire la fête fait partie de ma personnalité
bdscore$FeteImageAutre <- bd1$idt5

## Ma qualité de vie dans son ensemble
# Mobilité
bdscore$Mobilite <- bd1$eqmob
# Soins autonomes
bdscore$Autonomie <- bd1$eqaut
# Activités habituelles (ex: travail, études, tâches ménagères, activités familiales ou loisirs)
bdscore$Habitudes <- bd1$eqhab
# Douleurs/Malaise
bdscore$Douleur <- bd1$eqdoul
# Inquiétude/Dépression
bdscore$Depression <- bd1$eqdep

## Mes conditions de vie
# Lieu de résidence : Famille/tuteur, logement indépendant, résidence collective, ailleurs
log <- unique(bd1$logou)
bdscore$LogFamille <- ifelse(bd1$logou==log[1],1,NA)
bdscore$LogFamille[is.na(bdscore$LogFamille)]<-0
bdscore$LogInd <-ifelse(bd1$logou==log[3],1,NA)
bdscore$LogInd [is.na(bdscore$LogInd)]<-0
bdscore$LogRes <-ifelse(bd1$logou==log[2],1,NA)
bdscore$LogRes[is.na(bdscore$LogRes)]<- 0
bdscore$LogAutre <-ifelse(bd1$logou==log[5],1,NA)
bdscore$LogAutre[is.na(bdscore$LogAutre)]<-0
# Seul
bdscore$Seul <- ifelse(bd1$logwho1==unique(bd1$logwho1)[2],1,NA)
bdscore$Seul[is.na(bdscore$Seul)]<--0
# En couple
bdscore$Couple <- ifelse(bd1$logwho2==unique(bd1$logwho2)[2],1,NA)
bdscore$Couple[is.na(bdscore$Couple)]<--0
# Avec les enfants
bdscore$Enfants <- ifelse(bd1$logwho3==unique(bd1$logwho3)[2],1,NA)
bdscore$Enfants[is.na(bdscore$Enfants)]<--0
# Colocation avec amis
bdscore$ColocFriend <- ifelse(bd1$logwho4==unique(bd1$logwho4)[2],1,NA)
bdscore$ColocFriend[is.na(bdscore$ColocFriend)]<--0
# Colocation avec autres personnes
bdscore$ColocAutres <- ifelse(bd1$logwho5==unique(bd1$logwho5)[2],1,NA)
bdscore$ColocAutres[is.na(bdscore$ColocAutres)]<--0

# Difficultés financières en ce moment
finunique <- unique(bd1$fin)
bdscore$Argent <- ifelse(bd1$fin==finunique[2], 0, ifelse(bd1$fin == finunique[1], 1, ifelse(bd1$fin==finunique[5], 2, ifelse(bd1$fin==finunique[3], 3, ifelse(bd1$fin==finunique[6], 4, NA)))))
# Maladie chronique Booléen
bdscore$MaladieChroniqueBool <- ifelse(bd1$ald=="Oui",1,ifelse(bd1$ald=="Non",0,NA))
# Bourse
bdscore$Bourse <- ifelse(bd1$bours=="Oui",1, ifelse(bd1$bours =="Non",0,NA))

# Nous avons décidé de ne pas analyser la colonne "aldquoi" car les interrogés ont répondu librement

###### Exportation de la base de données bdscore ######
write.csv2(bdscore,file="bdscore.csv",row.names = FALSE)



############################################################
##############   Descriptions des données  #################
###  Moyenne, écart-type, nombre de NA dans chaque item  ###
############################################################


Nom_stats = c("Moyenne","Mediane","Maximum","Minimum","Nb de NA","Ecart-type","Part de NA")
N_stats = length(Nom_stats)

Nl=dim(bdscore)[1] # nombre de lignes
Nc=dim(bdscore)[2] # nombre d'items
info=data.frame(matrix(data=NA,nrow=N_stats,ncol=Nc-1))
rownames(info) <- Nom_stats
colnames(info) <- colnames(bdscore)[2:Nc]

for (i in (2:Nc)) {
  y=bdscore[i]
  info[1,i-1]<-apply(na.omit(y),2,mean) # moyenne
  info[2,i-1] <-apply(na.omit(y),2,median) # médiane
  info[3,i-1] <- max(na.omit(y)) # maximum
  info[4,i-1] <- min(na.omit(y)) # Minimum
  info[5,i-1] <- sum(1*is.na(bdscore[i])) # nb de NA
  info[6,i-1] <- apply(na.omit(y), 2, sd) # écart-type
  info[7,i-1] <- sum(1*is.na(bdscore[i]))/Nl # Part de NA
}


# Données manquantes 
# Taux de réponse de chaque individu et individu dont le nombre de reponses sont insuffisants

reponses=1*cbind(bdscore[1],is.na(bdscore[2:Nc])) # le 1* permet de changer les False/True en 0/1
reponses$Total <- rowSums(reponses[,3:Nc]) # nombre d'items où l'individu n'a pas repondu 
reponses$Pourcent <- 100*reponses$Total/Nc # taux de "non-réponse" 
faible_taux=reponses[reponses$Pourcent>60,] 
fort_taux=reponses[reponses$Pourcent<=0,]

taux_global=100*sum(reponses$Total)/(Nc*Nl) # taux global de réponses manquantes


###########################################
###  Méthode des plus proches voisins   ###
###########################################

# Si le Package VAR est bien chargé, on peut tracer un graph pour illustrer la part de données manquantes
# aggr(bdscore, col=c('navyblue','red'), numbers=TRUE, combined = FALSE, sortVars=TRUE, labels=names(bdscore), cex.axis=.7, gap=3, ylab=c("Histogram of missing bdscore","Pattern"))


# Imputation : La fonction impute.knn du package impute permet d'appliquer la méthode des plus proches voisins à la bdd bdscore.
NA_max_col=max(info[7,])
NA_max_row= max(reponses$Pourcent)/100

mat = impute.knn(as.matrix(bdscore),k=100,rowmax=NA_max_row,colmax=NA_max_col)
full_data = as.data.frame(mat$data) 

# Nous pouvons ainsi calculer les scores Atot=Somme de AQoLs et l'Audit-C. 
# Cependant, on préfère ne pas les inclure dans la bdd car ils ne seront pas pris en compte dans le clustering.
#Atot
#full_data$atot <- full_data$a1 + full_data$a2 + full_data$a3 + full_data$a4 + full_data$a5 + full_data$a6 + full_data$a7 + full_data$a8 + full_data$a9 + full_data$a10 + full_data$a11 + full_data$a12 + full_data$a13 + full_data$a14 + full_data$a15 + full_data$a16 + full_data$a17 + full_data$a18 + full_data$a19 + full_data$a20 + full_data$a21 + full_data$a22 + full_data$a23 + full_data$a24 + full_data$a25+full_data$a26+ full_data$a27+full_data$a28+full_data$a29+ full_data$a30+full_data$a31+full_data$a32+ full_data$a33+ full_data$a34
#Audit-C
#full_data$Audit <- full_data$FreqConso + full_data$NbVerreMoy+ full_data$FreqSupSixVerre


#####  Exportation de la base de données full_data  ######
write.csv2(full_data,file="full_data.csv",row.names = FALSE)

#################################################################
### Information sur la nouvelle matrice de données (complète) ###
#################################################################

Ncf=dim(full_data)[2] # nombre d'item
info_full=data.frame(matrix(data=NA,nrow=N_stats,ncol=Ncf-1))
rownames(info_full) <- Nom_stats
colnames(info_full) <- colnames(full_data)[2:Ncf]

Ncf=dim(full_data)[2]
for (i in (2:Ncf)) {
  y=full_data[,i]
  info_full[1,i-1]<- mean(y) # moyenne
  info_full[2,i-1] <-median(y) # médiane
  info_full[3,i-1] <- max(y) # maximum
  info_full[4,i-1] <- min(y) # Minimum
  info_full[5,i-1] <- sum(1*is.na(full_data[i])) #nb de NA
  info_full[6,i-1] <- sd(y) # écart-type
  info_full[7,i-1] <- sum(1*is.na(full_data[i]))/Nl # Part de NA
}

# On peut évaluer l'écart entre les statistiques de la base de données 
# non corrigée et les statistiques de la base corrigée

erreur_impute=data.frame(matrix(data=NA,nrow=5,ncol=Nc-1))
rownames(erreur_impute) <- c("Moyenne","Mediane","Maximum","Minimum","Ecart-type")
colnames(erreur_impute) <- colnames(bdscore)[2:Nc]
for (i in (2:Nc)) {
  y=full_data[,i]
  erreur_impute[1,i-1]<- abs(info[1,i-1]-info_full[1,i-1]) # moyenne
  erreur_impute[2,i-1] <-abs(info[2,i-1]-info_full[2,i-1]) # médiane
  erreur_impute[3,i-1] <- abs(info[3,i-1]-info_full[3,i-1]) # maximum
  erreur_impute[4,i-1] <- abs(info[4,i-1]-info_full[4,i-1]) # Minimum
  erreur_impute[5,i-1] <- abs(info[6,i-1]-info_full[6,i-1]) # écart-type
}

# Observation : Les écarts sont très faibles.

# Si le package VIM est bien chargé, on peut obtenir une petite vérification graphique 
# pour nous assurer qu'il n'y a plus de données manquantes
# aggr(full_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bdscore), cex.axis=.7, gap=3, ylab=c("Histogram of missing bdscore","Pattern"))




###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################


# Pour cette partie il faut avoir la base full_data. Il faut donc soit exécuter la première partie du code,
# soit en cas de bug utiliser setwd pour définir le chemin vers la BDD et importer la base full_data.

# setwd("")
# full_data=read.csv2("full_data.csv")


###############
### K-means ###
###############

Kmeans=function(bdscore,nbclus){
  clus= kmeans(na.omit(bdscore), nbclus, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
  Repartition=clus$cluster
  # On range les clusters dans une liste Clusters de bdscoreframes
  Clusters=list()
  for (i in 1:nbclus){
    Clusters[[i]]=bdscore[Repartition==i,]
  }
  return(Clusters)
}

nbkclus=10  # nombre de clusters souhaités
KClusters=Kmeans(full_data,10)

# On accède aux clusters en utilisant KClusters[[i]] avec i entre 1 et nbkclus
# La méthode des K-mean est la méthode que nous avons étudiée en cours.
# Le problème c'est que la validité du clustering repose sur le choix du nombre de clusters.
# Des indices et des testes existent pour déterminer ce nombre.
# Le package NbClust permet d'en calculer certains, mais les calculs sont lourds (Complexité en o(n^3))
# Nous avons donc opté pour une ACP suivie par une Classification Ascendante Hiérarchique (CAH).
# L'ACP permet de réduire la quantité de données en épurant la bdd.
# La CAH permet de former les clusters et offre un outil visuel (en plus des indices et tests) 
# pour déterminer le nombre de clusters idéal.



########################################################################
### ACP: Réduction du nombre de dimension et sélection des variables ###
########################################################################

### 1ère étape : Sélectionner les variables qui contribuent le plus à la variabilité de la BDD ###

## On applique une ACP sur l'ensemble des données avec la fonction PCA du package FactoMinR:
ACP <- PCA(full_data[-1],ncp=78, graph=FALSE)      #Ici le nombre de dimension n'est pas important.

# print(ACP) #Permet de voir les commandes pour accéder aus résultats

## Graphique des variables (Représentation des variables dans le premier plan) : 
fviz_pca_var(ACP, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# Ce graphique permet de visualiser la contribution de chacun des items au deux 
# premières composantes principales (qui sont les plus importants).
# Pour accéder aux valeurs numériques des contributions :
View(ACP$var$contrib)


## % Contributions des variable à la composante pincipale 1 CP1
fviz_contrib(ACP, choice = "var", axes = 1)

## % Contributions des variable à la composante pincipale 2 CP2
fviz_contrib(ACP, choice = "var", axes = 2)


## Si on voulait exploiter ces résultats, une méthode pour determiner 
# le nombre de dim idéal serait le critère du coude. Pour cela on trace :
## Histogramme des valeurs propores
barplot(ACP$eig[1:dim(full_data)[2],2], main="Histogramme des valeurs propres", 
        names.arg=1:dim(full_data)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
        cex.axis=0.8, font.lab=3, ylim=c(0, 12), col="green")


## Graphique des individus (Représentation des individus dans le premier plan)
fviz_pca_ind(ACP, col.ind="cos2", geom = "point")


## Nous enlevons les variables proche du centre dans la représentation Variable factor map (PCA)
## Variables retenues :
NlDBACP=dim(full_data)[1]
DBACP=data.frame(matrix(data=NA,nrow=NlDBACP,ncol=1))
DBACP<-DBACP[,-1]
DBACP$FreqBinge<-full_data$FreqBinge
DBACP$NbMaxOcc<-full_data$NbMaxOcc
DBACP$FreqConso<-full_data$FreqConso
DBACP$NbVerreMoy<-full_data$NbVerreMoy
DBACP$FreqSupSixVerre<-full_data$FreqSupSixVerre
DBACP$a1<-full_data$a1
DBACP$a2<-full_data$a2
# DBACP$a3<-full_data$a3
DBACP$a4<-full_data$a4
DBACP$a5<-full_data$a5
DBACP$a6<-full_data$a6
DBACP$a7<-full_data$a7
DBACP$a8<-full_data$a8
DBACP$a9<-full_data$a9
DBACP$a10<-full_data$a10
DBACP$a11<-full_data$a11
DBACP$a12<-full_data$a12
DBACP$a13<-full_data$a13
DBACP$a14<-full_data$a14
DBACP$a15<-full_data$a15
DBACP$a16<-full_data$a16
DBACP$a17<-full_data$a17
DBACP$a18<-full_data$a18
DBACP$a19<-full_data$a19
DBACP$a20<-full_data$a20
DBACP$a21<-full_data$a21
DBACP$a22<-full_data$a22
DBACP$a23<-full_data$a23
DBACP$a24<-full_data$a24
DBACP$a25<-full_data$a25
DBACP$a26<-full_data$a26
DBACP$a27<-full_data$a27
DBACP$a28<-full_data$a28
DBACP$a29<-full_data$a29
DBACP$a30<-full_data$a30
DBACP$a31<-full_data$a31
DBACP$a32<-full_data$a32
DBACP$a33<-full_data$a33
DBACP$a34<-full_data$a34
DBACP$Etudes<-full_data$Etudes
DBACP$Tabac<-full_data$Tabac
DBACP$FeteImagePerso<-full_data$FeteImagePerso
DBACP$FeteEtre<-full_data$FeteEtre
DBACP$FetePerso<-full_data$FetePerso
DBACP$FeteQuotidien<-full_data$FeteQuotidien
DBACP$FeteImageAutre<-full_data$FeteImageAutre


##### Exportation de la base de données DBACP #####
write.csv2(DBACP,file="DBACP.csv",row.names = FALSE)

### Etape 2: On détermine le nombre idéal de dimensions ###

## On fait une ACP sur la nouvelle base :
ACPred <- PCA(DBACP,ncp=7, graph=FALSE)

## Graphique des variables
fviz_pca_var(ACPred, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# On remarque que les pourcentages ont augmenté !

## % Contributions des variable à la composante pincipale 1 CP1
fviz_contrib(ACPred, choice = "var", axes = 1)

## % Contributions des variable à la composante pincipale 2 CP2
fviz_contrib(ACPred, choice = "var", axes = 2)

## Graphique des individus
fviz_pca_ind(ACP, col.ind="cos2", geom = "point")


## On détermine le nombre de dimension à concerver à l'aide du critère du coude
## Histogramme des valeurs propores
barplot(ACPred$eig[1:dim(full_data)[2],2], main="Histogramme des valeurs propres", 
        names.arg=1:dim(full_data)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
        cex.axis=0.8, font.lab=3, ylim=c(0, 12), col="orange")
## 7 semble un bon nombre de dimension d'après le critère du coude


## Nous pouvons aussi utiliser la fonction estim_ncp qui donne le nombre idéal de dim (ncp)
# Smooth Method
estim_ncp(DBACP, method="Smooth")
# Generalized cross-validation approximation
estim_ncp(DBACP, method="GCV")
## Les deux donnent 7 dimensions !

## Pourcentages cumulés de variabilité expliquée
ACPred$eig
## Avec 7 dimensions on explique 52% de la variabilité


########################################################
### Première classification ascendante hiérarchique  ###
######  Détermination du nombre de Clusters idéal  ##### 
########################################################

## Afin de déterminer le nombre de Clusters idéal, on trace le Dendrogramme.
# Nous pouvons alors déterminer le nombre de Cluster en considérant la taille des Cluster,
# ainsi que les changement d'inertie.

## Création d'un Cluster :

# On calcule la matrice des distances de ACP2
Distance=dist(ACPred$ind$coord,method = "euclidean")

# hclust permet de créer les clusters avec la méthode de WARD
CAH=hclust(Distance,method="ward.D2")

# plot permet de tracer le dendrograme : ATTENTION ! NECESSITE UNE MACHINE PERFORMANTE !
plot(CAH)

# Le bon compromis est 4 Clusters
rect.hclust(CAH,k=5,border=2:10)  #Permet de tracer des carrés autour des clusters choisis

# La fonction cutree(tree,k) permet de couper le dendrograme pour former k clusters:
Repartition=cutree(CAH,7)

# On regroupe les lignes du premier cluster dans une même base
Cluster1=full_data[Repartition==1,]
# On réitère pour le reste des Clusters


##################################################
### Itération de la méthode avec des fonctions ###
##################################################

# ClusterCAH prend en argument la nombre de dimensison de ACP dimacp, le nombre de cluster 
#à créer nbclus, bdscoreACP la base qui regroupe les variables pour l'ACP et full_data la base complète.
# Elle retourne une liste des nbclus Clusters.

ClusterCAH=function(dimacp,nbclus,bdscoreACP,full_data){
  #On applique la méthode de l'Analyse par composantes principales 
  #à l'aide de la fonction PCA du package FactoMineR
  print("ACP encours ... ")
  ACP=PCA(bdscoreACP,ncp=dimacp,graph=FALSE)
  print(c("Pourcentage de variabilité expliqué :",ACP$eig$`cumulative percentage of variance`[dimacp]))
  print("Affichage des variables dans le premier plan ... ")
  fviz_pca_var(ACP, col.var="contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # Avoid text overlapping
  )
  print("Affichage des individus dans le premier plan ... ")
  fviz_pca_ind(ACP, col.ind="cos2", geom = "point")
  print("Traçage de l'histogramme des valeurs propres ...")
  barplot(ACP$eig[1:dim(bdscoreACP)[2],2], main="Histogramme des valeurs propres", 
          names.arg=1:dim(bdscoreACP)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
          cex.axis=0.8, font.lab=3, ylim=c(0, 15), col="orange")

  # La fonction dist prend retourne à partir des coordonner des individus
  #la matrice des distances en utilisant la norme euclidienne
  print("Calcul de la matrice de distance ...")
  Distance=dist(ACP$ind$coord,method = "euclidean")
  
  # La fonction hclust prend comme argument la bdscoreframe et la 
  # matrice de distances et retourne la Classification ascendante hiérarchique
  print('Formation des clusters ...')
  CAH=hclust(Distance,method="ward.D2")
  
  # Le plot de CAH donne le Dendogramme de la classification hiérarchique
  # print("Traçage du dendogramme")
  # plot(CAH)
  # rect.hclust permet de tracer le dendrograme avec des cadres autour des clusters sélectionné
  # rect.hclust(CAH,nbclus)
  
  # La fonction cutree permet de couper le dendogramme et donne nbclus clusters
  Repartition=cutree(CAH,nbclus)
  
  # On range les clusters dans une liste Clusters de bdscoreframes
  Clusters=list()
  for (i in 1:nbclus){
    Clusters[[i]]=full_data[Repartition==i,]
  }
  
  return(Clusters)
}



###################################
### Classification des Clusters ###
###################################


#ClassificationClusters prend en argument une liste de clusters et retourne les 
#indinces des clusters triés par ordre décroissant selon la valeur moyenne de atot
ClassificationClustersAtot=function(Clusters){
  nbclus=length(Clusters)
  ordre=(1:nbclus)
  for (i in (1:nbclus)){
    Temp=as.integer(ordre[i])
    j=i
    while(j>1 && mean(Clusters[[ordre[j-1]]]$atot)<mean(Clusters[[Temp]]$atot)){
      ordre[j]=as.integer(ordre[j-1])
      j=j-1
    }
    ordre[j]=Temp
  }
  return(ordre)
}

#classement à partir de Audit C
ClassificationClustersAudit=function(Clusters){
  nbclus=length(Clusters)
  ordre=(1:nbclus)
  for (i in (1:nbclus)){
    Temp=as.integer(ordre[i])
    j=i
    while(j>1 && mean(Clusters[[ordre[j-1]]]$Audit)<mean(Clusters[[Temp]]$Audit)){
      ordre[j]=as.integer(ordre[j-1])
      j=j-1
    }
    ordre[j]=Temp
  }
  return(ordre)
}



##########################################
### Affichage des comparatifs Clusters ###
##########################################

## Moyenne :

CompareMean=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]-1
  nbclus=length(Clusters)
  NomCol=colnames(Clusters[[1]])[-1]
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    for (j in (1:Comparenbcol)){
      CompareArray[i,j]=mean(Clusters[[i]][[j+1]])
    }
  }
  return(CompareArray)
}

## Ecart-type :

CompareSD=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]-1
  nbclus=length(Clusters)
  NomCol=colnames(Clusters[[1]])[-1]
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    for (j in (1:Comparenbcol)){
      CompareArray[i,j]=sd(Clusters[[i]][[j+1]])
    }
  }
  return(CompareArray)
}

## Minimum

CompareMin=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]-1
  nbclus=length(Clusters)
  NomCol=colnames(Clusters[[1]])[-1]
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    for (j in (1:Comparenbcol)){
      CompareArray[i,j]=min(Clusters[[i]][[j+1]])
    }
  }
  return(CompareArray)
}

## Maximum

CompareMax=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]-1
  nbclus=length(Clusters)
  NomCol=colnames(Clusters[[1]])[-1]
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    for (j in (1:Comparenbcol)){
      CompareArray[i,j]=max(Clusters[[i]][[j+1]])
    }
  }
  return(CompareArray)
}

## Median

CompareQuantile=function(Clusters,percent=0.5){
  Comparenbcol=dim(Clusters[[1]])[2]-1
  nbclus=length(Clusters)
  NomCol=colnames(Clusters[[1]])[-1]
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    for (j in (1:Comparenbcol)){
      CompareArray[i,j]=quantile(Clusters[[i]][[j+1]],percent)
    }
  }
  return(CompareArray)
}


##########################
### Etude des Clusters ###
##########################

nbcl=5
dimacp=7
Clusters=ClusterCAH(dimacp,nbcl,DBACP,full_data)
#NB: utiliser les flèches bleues pour voir les graphs une fois l'execution terminée.

# On affiche les dimension des Clusters obtenues
for (i in (1:length(Clusters))) {
  print(dim(Clusters[[i]]))
}


CompMoyenne=CompareMean(Clusters)
CompEcart=CompareSD(Clusters)
CompMin=CompareMin(Clusters)
CompMax=CompareMax(Clusters)
CompMedian=CompareQuantile(Clusters,0.5)


View(CompMoyenne)
View(CompEcart)
View(CompMin)
View(CompMax)
View(CompMedian)


