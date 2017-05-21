  ## Nettoyage de l'espace de travail
rm(list=ls())

install.packages("readxl")
install.packages("FactoMineR") 
install.packages("factoextra")
install.packages("cluster")
install.packages("NbClust")
install.packages("fpc")
install.packages("modeltools")
source("http://bioconductor.org/biocLite.R") # essayer avec http:// if not supported
biocLite("impute") #équivalent de install.packages
# install.packages("VIM") 

library(readxl)
library(FactoMineR)
library(factoextra)
library(cluster)
library(NbClust)
library(modeltools)
library(fpc)
library(impute)
#library(VIM) #Ne marche pas sur toutes les machines, ce n'est pas un package important.

#####################################
### Lecture de la base de données ###
#####################################

## Nous avons tous travaillé sur le même code via GitHub

# Anis
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
# setwd("Chemin") #Il faut remplacer "Chemin" par le chemin de la base bdmieRpp2.xls. Vous pouvez aussi utiliser le bouton Session -> Set Working Directory
# bd <- read_excel("bdmieRpp2.xls") #Vous pouvez utiliser le bouton File->Import Dataset->From Excel


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

# On transforme les réponses de l'AQOLS en score et on les insére dans la dataframe. 
# L'utilisation de unique permet de contourner tout problème du à l'encodage du script. NB: Nous utilisons UTF-8.


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
# bdscore$Disc<-bd1$disc --> colonne qualitative, qu'on a préféré 
# scinder en plusieurs colonnes avec un résultat qualitatif
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
# Si frqoh="Jamais" FreqBinge reçoit 0, de même si binge="non". Sinon, on affecte un score allant de 1 à 5.
bdscore$FreqBinge <- ifelse(bd1$frqoh==unique(bd1$frqoh)[6], 0, ifelse(bd1$binge==unique(bd1$binge)[2], 0, ifelse(is.na(bd1$frqb1),ifelse(is.na(bd1$frqb2),ifelse(is.na(bd1$frqb3), ifelse(is.na(bd1$frqb6),ifelse(is.na(bd1$frqb10),NA,5),4),3),2),1)))

# Nombre maximum de verres bus en UNE occasion
summary(bd1$max1occ) # le maximum est de 120 ce qui semble aberrant, mais tout dépend de ce qu'on appelle occasion (Quel est la durée ?).
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

## Etudes et alcool
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

## La qualité de vie dans son ensemble
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

## Les conditions de vie
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

# Nous avons décidé de ne pas analyser la colonne "aldquoi" car les interrogés ont répondu librement.

###### Exportation de la base de données bdscore ######
write.csv2(bdscore,file="bdscore.csv",row.names = FALSE)



############################################################
##############   Description des données   #################
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
# Taux de réponse de chaque individu et le nombre d'individus dont le nombre de réponses est insuffisant

reponses=1*cbind(bdscore[1],is.na(bdscore[2:Nc])) # le 1* permet de changer les False/True en 0/1
reponses$Total <- rowSums(reponses[,3:Nc]) # nombre d'items où l'individu n'a pas repondu 
reponses$Pourcent <- 100*reponses$Total/Nc # taux de "non-réponse" 
faible_taux=reponses[reponses$Pourcent>60,] 
fort_taux=reponses[reponses$Pourcent<=0,]

taux_global=100*sum(reponses$Total)/(Nc*Nl) # taux global de réponses manquantes
print(c("Taux de données manquantes",taux_global))

###########################################
###  Méthode des plus proches voisins   ###
###########################################

# Si le Package VAR est bien chargé, on peut tracer un graph pour illustrer la part de données manquantes à l'aide de aggr.
# aggr(bdscore, col=c('navyblue','red'), numbers=TRUE, combined = FALSE, sortVars=TRUE, labels=names(bdscore), cex.axis=.7, gap=3, ylab=c("Histogram of missing bdscore","Pattern"))


# Imputation : La fonction impute.knn du package impute permet d'appliquer la méthode des plus proches voisins à la bdd bdscore.
NA_max_col=max(info[7,])
NA_max_row= max(reponses$Pourcent)/100

mat = impute.knn(as.matrix(bdscore),k=100,rowmax=NA_max_row,colmax=NA_max_col)
full_data = as.data.frame(mat$data) # df complète

# Nous pouvons ainsi calculer les scores Atot=Somme de AQoLs et l'Audit-C. 
# Cependant, on préfère ne pas les inclure dans la bdd car ils ne seront pas pris en compte dans le clustering.
#Atot
#full_data$atot <- full_data$a1 + full_data$a2 + full_data$a3 + full_data$a4 + full_data$a5 + full_data$a6 + full_data$a7 + full_data$a8 + full_data$a9 + full_data$a10 + full_data$a11 + full_data$a12 + full_data$a13 + full_data$a14 + full_data$a15 + full_data$a16 + full_data$a17 + full_data$a18 + full_data$a19 + full_data$a20 + full_data$a21 + full_data$a22 + full_data$a23 + full_data$a24 + full_data$a25+full_data$a26+ full_data$a27+full_data$a28+full_data$a29+ full_data$a30+full_data$a31+full_data$a32+ full_data$a33+ full_data$a34
#Audit-C
#full_data$Audit <- full_data$FreqConso + full_data$NbVerreMoy+ full_data$FreqSupSixVerre


#####  Exportation de la base de données full_data  ######
write.csv2(full_data,file="full_data.csv",row.names = FALSE)

#####################################################
### Informations sur la base de données complétée ###
#####################################################

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
# non complétée et les statistiques de la base complété

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

# Observations : Les écarts sont très faibles.

# Si le package VIM est bien chargé, on peut obtenir une petite vérification graphique 
# pour nous assurer qu'il n'y a plus de données manquantes
# aggr(full_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bdscore), cex.axis=.7, gap=3, ylab=c("Histogram of missing bdscore","Pattern"))




###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################


# Pour cette partie il faut avoir la base full_data. Il faut donc soit exécuter la première partie du code,
# soit, en cas de bug, utiliser setwd pour définir le chemin vers la BDD et importer la base full_data.

# setwd("")
full_data=read.csv2("full_data.csv")

########################################################
### Etude de la tendance de clusterisation de la BDD ###
########################################################
# http://www.sthda.com/english/wiki/assessing-clustering-tendency-a-vital-issue-unsupervised-machine-learning

## Avant de se lancer dans la clusterisation, on veut savoir si la BDD contient des clusters 
# ou si les répartition des individus est complétement aléatoire.
## Pour celà, on utilise la statistique de Hopkins et la fonction get_clust_tendency:

get_clust_tendency(full_data, n=10, graph = TRUE, 
                   gradient = list(low = "red", mid = "white", high = "blue"), seed = 123)

# Résultat:
#   $hopkins_stat
# [1] 0.001383891
# Plus la statistique s'approche de 0,5 plus le clustering est mauvais.
# La valeur obtenue est très petite devant 0,5 ce qui indique qu'on peut trouver de bons clusters.
# Nos machines n'ont pas pu tracer le plot ordered dissimilarity image.

###############
### K-means ###
###############

## La méthode des K-means permet de former des clusters.
help(kmeans)

### Etape 1: Détermination du nombre idéal de Clusters ###
## ATTENTION ! Cette partie demande une machine puissante ! Elle fait planter nos machines à chaque essai.
# On vous conseille de ne pas l'exécuter.
#http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning

# Afin de déterminer le nbr idéal de clusters, on utlise le package NbClust qui permet de calculer 30 indices
# dont chacun suggeste le choix d'un nombre de cluster.

res.nbcl <- NbClust(scale(full_data), distance = "euclidean",
                    min.nc = 2, max.nc = 10, 
                    method = "complete", index ="all") 
# wss:
fviz_nbclust(scale(full_data), kmeans, method = "wss",k.max = 8)
# Silhouette:
fviz_nbclust(scale(full_data), kmeans, method = "silhouette",k.max=8)
# Utilisation de Gap-statistic: 
gap_stat <- clusGap(full_data, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 500) 
# On visualise le résultat de gap-statistique. Le nbr idéal correspond au maximum.
fviz_gap_stat(gap_stat)
# NB: NbClust permet aussi de faire ce calcul



## Formation des clusters
nbkclus=5  # nombre de clusters souhaités
res.Kmeans= kmeans(scale(full_data), nbclus, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
Repartition=res.Kmeans$cluster
KClusters=list()
for (i in 1:nbclus){
  KClusters[[i]]=full_data[Repartition==i,]
}

# Visualisation des Clusters
fviz_cluster(res.Kmeans, data = scale(DBACP), geom = "point",
             stand = TRUE, ellipse.type = "norm")

# On accède aux clusters en utilisant KClusters[[i]] avec i entre 1 et nbkclus
# La méthode des K-mean est la méthode que nous avons étudiée en cours.
# Le problème c'est que la validité du clustering repose sur le choix du nombre de clusters.
# Des indices et des testes existent pour déterminer ce nombre.
# Le package NbClust permet d'en calculer certains, mais les calculs sont lourds (Complexité en o(n^3))
# Nous avons donc opté pour une ACP suivie par une Classification Hiérarchique sur Composantes Principales (CHCP).
# Il s'agit d'une Analyse en Composante Principale (ACP) suivie d'une Classification Ascendante Hiérarchique (CAH)
# L'ACP permet de réduire la quantité de données en enlevant le bruit de la bdd.
# La CAH permet de former les clusters et offre un outil visuel (en plus des indices et tests) 
# pour déterminer le nombre de clusters idéal.



########################################################################
### ACP: Réduction du nombre de dimension et sélection des variables ###
########################################################################


### 1ère étape : Sélectionner les variables qui contribuent le plus à la variabilité de la BDD ###

## On applique une ACP sur l'ensemble des données avec la fonction PCA du package FactoMinR:
ACP <- PCA(full_data[-1],scale.unit=TRUE,ncp=12, graph=FALSE)      

# Ici le nombre de dimension n'est pas important.
# L'argument scale.unit=TRUE permet de réduire la variance des colonne à 1.
# Réduire la variance à 1 permet de donner un même poid à toutes les variables.

print(ACP) #Permet de voir les commandes pour accéder aus résultats

## Graphique des variables (Représentation des variables dans le premier plan) : 
fviz_pca_var(ACP, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# Ce graphique permet de visualiser la contribution de chacun des items au deux 
# premières composantes principales (qui sont les plus importantes).
# Pour accéder aux valeurs numériques des contributions :
View(ACP$var$contrib)


## % Contributions des variables à la composante pincipale 1 (CP1):
fviz_contrib(ACP, choice = "var", axes = 1)

## % Contributions des variables à la composante pincipale 2 (CP2):
fviz_contrib(ACP, choice = "var", axes = 2)


## Graphique des individus (Représentation des individus dans le premier plan):
fviz_pca_ind(ACP, col.ind="cos2", geom = "point")


## Si on voulait exploiter les résultats de cette première ACP, plusieurs méthodes peuvent être appliquées:
## Méthode de Kaiser et Critère du coude :
# http://www.statsoft.fr/concepts-statistiques/analyse-factorielle/analyse-factorielle.htm

# Le critère de Kaiser (Ne retenir que les valeurs propores >1)
ACP$eig$eigenvalue[ACP$eig$eigenvalue>1]
# Ce critère donne 22 dimensions.

# Critère du coude (Canttell):
# On trace l'histogramme des valeurs propores
barplot(ACP$eig[1:dim(full_data)[2],2], main="Histogramme des valeurs propres", 
        names.arg=1:dim(full_data)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
        cex.axis=0.8, font.lab=3, ylim=c(0, 12), col="green")
# On remarque une décrochement d'inertie à partir de 11 axes, puis une baisse constante.
# Ceci indique qu'il faut garder 11 dimensions.

## Nous pouvons aussi utiliser la fonction estim_ncp qui donne le nombre idéal de dim (ncp):
# https://pdfs.semanticscholar.org/76e5/dc096446dbc18b5df6a88f72a1ddb379c48b.pdf
# Smooth Method :
estim_ncp(full_data, method="Smooth")
# Résultat : 11 dimensions
# Generalized cross-validation approximation :
estim_ncp(full_data, method="GCV")


# On peut donc finalement choisir 12 dimensions.

## Pourcentages cumulés de variabilité expliquée par les dimensions:
ACP$eig
# Avec 12 dimensions on explique 46% de la variabilité. 
# Ce pourcentage est faible à cause du "bruit".
# Pour y remérdier on peut éliminer les variables qui contribuent 
# faiblement aux deux premières dimensions.


## On calcule la norme de contribution dans le premier plan:
Nlcon=dim(ACP$var$contrib)[1]
Contrib12=ACP$var$contrib[,1:2]
Contributions=matrix(data=NA,nrow=Nlcon,ncol=1)
rownames(Contributions)=rownames(Contrib12)
colnames(Contributions)=c("Norm")
for (i in (1:Nlcon)){
  Contributions[i]=sqrt(Contrib12[i,1]^2+Contrib12[i,2]^2)
} 
View(Contributions)


## Nous enlevons les variables proche du centre dans la représentation Variable factor map (PCA):
## On choisit de garder les variables avec une norme > 1
## Variables retenues classées en ordre décroissant de contribution :
NlDBACP=dim(full_data)[1]
DBACP=data.frame(matrix(data=NA,nrow=NlDBACP,ncol=1))
DBACP<-DBACP[,-1]

DBACP$FreqSupSixVerre<-full_data$FreqSupSixVerre
DBACP$FreqBinge<-full_data$FreqBinge
DBACP$FreqConso<-full_data$FreqConso
DBACP$NbMaxOcc<-full_data$NbMaxOcc
DBACP$FeteImagePerso<-full_data$FeteImagePerso
DBACP$FetePerso<-full_data$FetePerso
DBACP$FeteEtre<-full_data$FeteEtre
DBACP$a7<-full_data$a7
DBACP$FeteImageAutre<-full_data$FeteImageAutre
DBACP$a21<-full_data$a21
DBACP$FeteQuotidien<-full_data$FeteQuotidien
DBACP$a8<-full_data$a8
DBACP$a20<-full_data$a20
DBACP$a14<-full_data$a14
DBACP$Cannabis<-full_data$Cannabis
DBACP$a12<-full_data$a12
DBACP$NbVerreMoy<-full_data$NbVerreMoy
DBACP$a9<-full_data$a9
DBACP$a26<-full_data$a26
DBACP$a17<-full_data$a17
DBACP$Tabac<-full_data$Tabac
DBACP$a23<-full_data$a23
DBACP$a11<-full_data$a11
DBACP$Etudes<-full_data$Etudes
DBACP$a1<-full_data$a1
DBACP$a27<-full_data$a27
DBACP$a6<-full_data$a6
DBACP$a34<-full_data$a34
DBACP$a30<-full_data$a30
DBACP$a19<-full_data$a19
DBACP$a18<-full_data$a18
DBACP$a32<-full_data$a32
DBACP$a10<-full_data$a10
DBACP$a29<-full_data$a29
DBACP$a15<-full_data$a15
DBACP$a31<-full_data$a31
DBACP$a22<-full_data$a22
DBACP$a25<-full_data$a25
DBACP$a2<-full_data$a2
DBACP$a5<-full_data$a5
DBACP$a33<-full_data$a33
DBACP$a4<-full_data$a4
DBACP$a16<-full_data$a16
DBACP$Depression<-full_data$Depression
DBACP$a28<-full_data$a28
DBACP$Poppers<-full_data$Poppers
DBACP$Genre<-full_data$Genre
DBACP$a13<-full_data$a13
DBACP$a24<-full_data$a24
DBACP$MD<-full_data$MD
DBACP$Habitudes<-full_data$Habitudes
DBACP$Douleur<-full_data$Douleur
DBACP$a3<-full_data$a3


##### Exportation de la base de données DBACP #####
write.csv2(DBACP,file="DBACP.csv",row.names = FALSE)

### Etape 2: On détermine le nombre idéal de dimensions ###

## On fait une ACP sur la nouvelle base :
ACPred <- PCA(DBACP,ncp=10,scale.unit = TRUE, graph=FALSE)

## Graphique des variables
fviz_pca_var(ACPred, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# On remarque que les pourcentages des axes ont augmenté !

## % Contributions des variables à la composante pincipale 1 CP1
fviz_contrib(ACPred, choice = "var", axes = 1)

## % Contributions des variables à la composante pincipale 2 CP2
fviz_contrib(ACPred, choice = "var", axes = 2)

## Graphique des individus
fviz_pca_ind(ACP, col.ind="cos2", geom = "point")


# On peut par ailleurs utiliser le 

## Détrmination du nombre de dimensions:

# Kaiser vs Cantell : http://www.statsoft.fr/concepts-statistiques/analyse-factorielle/analyse-factorielle.htm

# Critère de Kaiser (Ne retenir que les valeurs propores >1)
ACPred$eig$eigenvalue[ACPred$eig$eigenvalue>1]
# Ce critère donne 10 dimensions, donc un même ordre de grandeur.

# Critère du coude (Cattell):
## Histogramme des valeurs propores
barplot(ACPred$eig[1:dim(full_data)[2],2], main="Histogramme des valeurs propres", 
        names.arg=1:dim(full_data)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
        cex.axis=0.8, font.lab=3, ylim=c(0, 12), col="orange")
## 9 semble être un bon nombre de dimensions d'après le critère du coude.


## Nous pouvons aussi utiliser la fonction estim_ncp qui donne le nombre idéal de dim (ncp):
#https://pdfs.semanticscholar.org/76e5/dc096446dbc18b5df6a88f72a1ddb379c48b.pdf
# Smooth Method
estim_ncp(DBACP, method="Smooth")
# Generalized cross-validation approximation
estim_ncp(DBACP, method="GCV")
## Les deux donnent 8 dimensions !

## Il faut savoir que tous ces critères sont pertinents. Il faut cependant faire attention à garder un taux 
# d'explication de la variabilité important pour ne pas déformer le nuage des points. 
# En pratique, un aspect important à prendre en compte est le degré auquel une solution est interprétable. 
# C'est pourquoi, nous avons examiné plusieurs solutions avec des dimensions allant de 8 à 9, et nous avons
# choisi la plus pertinente: 10 dimensions.

## Pourcentages cumulés de variabilité expliquée :
ACPred$eig
## Avec 10 dimensions on explique 55% de la variabilité.



#########################################################
### CHCP et Détermination du nombre de Clusters idéal ###
#########################################################
# http://www.sthda.com/english/wiki/hybrid-hierarchical-k-means-clustering-for-optimizing-clustering-outputs-unsupervised-machine-learning
# http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning


## On applique la HCPC à l'aide de la fonction HPCP de FactroMinR. Cette fonction est très complète.
# Si nb.clust=0 et graph=TRUE, la fonction affiche le dendrograme à l'aide d'une console grâce à 
# laquelle l'utilisateur peut choisir le nombre de clusters en cliquant su la hauteur de coupe souhaitée.
# Si nb.clust=-1. La fonction retourne le nombre de cluster idéal c'est à dire maximisant l'inertie:
help(HCPC) # La méthode est expliquée dans la partie Details de la doc.
# De plus HCPC offre la possibilité de consolider les cluster avec la méthode des K-means.
# K-means peut soit être appliqué avant la CAH (kk=nb de cluster initial) et nécessite un nbr idéal de clusters pour K-means
# soit après la CAH (consol=TRUE), et dans ce cas K-means utilise le nombre de clusters établi par CAH.


## On applique la HCPC sur ACPred :
res.hcpc <- HCPC(ACPred,nb.clust=-1,consol=TRUE, metric="euclidean", 
                 method="ward", iter.max = 100,min=4, max=20, order=FALSE, 
                 graph.scale = "inertia", graph = FALSE, kk=Inf)
## On trace le dendrograme pour visualiser les clusters :
plot.HCPC(res.hcpc, choice="tree", rect=TRUE,
          ind.names=FALSE, t.level="all", title=NULL,
          new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
          centers.plot=FALSE)



## On restitut l'ensemble des variables (full_data) sous forme de liste de Clusters : 
nbclus=res.hcpc$call$t$nb.clust # Nbr de clusters
Clusters=list()
for (i in 1:nbclus){
  Clusters[[i]]=full_data[res.hcpc$data.clust$clust==i,]
}

## Visualisation des clusters:
fviz_cluster(res.hcpc, data = scale(full_data), geom = "point",
             stand = TRUE, ellipse.type = "norm")

# Pour accéder à un des cluster, il suffit d'utiliser Clusters[[i]] avec i le n° du cluster.


##########################################
### Affichage des comparatifs Clusters ###
##########################################

## Moyenne :

CompareMean=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]
  nbclus=length(Clusters)
  NomCol=c("Dim",colnames(Clusters[[1]])[-1])
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    CompareArray[i,1]=dim(Clusters[[i]])[1]
    for (j in (2:Comparenbcol)){
      CompareArray[i,j]=mean(Clusters[[i]][[j]])
    }
  }
  return(CompareArray)
}

## Ecart-type :

CompareSD=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]
  nbclus=length(Clusters)
  NomCol=c("Dim",colnames(Clusters[[1]])[-1])
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    CompareArray[i,1]=dim(Clusters[[i]])[1]
    for (j in (2:Comparenbcol)){
      CompareArray[i,j]=sd(Clusters[[i]][[j]])
    }
  }
  return(CompareArray)
}

## Minimum

CompareMin=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]
  nbclus=length(Clusters)
  NomCol=c("Dim",colnames(Clusters[[1]])[-1])
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    CompareArray[i,1]=dim(Clusters[[i]])[1]
    for (j in (2:Comparenbcol)){
      CompareArray[i,j]=min(Clusters[[i]][[j]])
    }
  }
  return(CompareArray)
}

## Maximum

CompareMax=function(Clusters){
  Comparenbcol=dim(Clusters[[1]])[2]
  nbclus=length(Clusters)
  NomCol=c("Dim",colnames(Clusters[[1]])[-1])
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    CompareArray[i,1]=dim(Clusters[[i]])[1]
    for (j in (2:Comparenbcol)){
      CompareArray[i,j]=max(Clusters[[i]][[j]])
    }
  }
  return(CompareArray)
}

## Median

CompareQuantile=function(Clusters,percent=0.5){
  Comparenbcol=dim(Clusters[[1]])[2]
  nbclus=length(Clusters)
  NomCol=c("Dim",colnames(Clusters[[1]])[-1])
  CompareArray=data.frame(matrix(data=NA,nrow=nbclus,ncol=Comparenbcol))
  colnames(CompareArray)=NomCol
  for (i in (1:nbclus)){
    CompareArray[i,1]=dim(Clusters[[i]])[1]
    for (j in (2:Comparenbcol)){
      CompareArray[i,j]=quantile(Clusters[[i]][[j]],percent)
    }
  }
  return(CompareArray)
}


##########################
### Etude des Clusters ###
##########################


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

# Nous pouvons étudier les individus "para" qui sont les plus proches du centre du cluster.
# Ces individus représente le plus le cluster.
res.hcpc$desc.ind$para

