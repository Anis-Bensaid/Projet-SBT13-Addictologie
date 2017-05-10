## Nettoyage de l'espace de travail
rm(list=ls())

# install.packages("readxl")
# install.packages("plot3D")
# install.packages("FactoMineR") 

## Packages pour la méthode des plus proches voisins 
# install.packages("VIM")
# library(VIM)
# source("http://bioconductor.org/biocLite.R") # essayer avec http:// if not supported
# biocLite("impute") #équivalent de install.packages

library(plot3D)
library(FactoMineR)
library(readxl)
library(impute)


#####################################
### Lecture de la base de données ###
#####################################


## Anis
# bd <- read_excel("D:/Users/enysb/Google Drive/Etudes/Git/Projet-SBT13-Addictologie/bdmieRpp2.xls")
# setwd("D:/Users/enysb/Google Drive/Etudes/Git/Projet-SBT13-Addictologie")

## Arthur
# bd <- read_excel("~/Documents/Projet Enjeux/Projet-SBT13-Addictologie/bdmieRpp2.xls")
# setwd("")

## Benjamin
# bd<- read_excel("~/GitHub/Projet-SBT13-Addictologie/bdmieRpp2.xls")
# setwd("")

## Emilio
# bd <- read_excel("C:/Users/Emilio/Desktop/intercambio/clases/enjeux/sbt/Projet-SBT13-Addictologie/bdmieRpp2.xls")
# setwd("")

## Haim
# bd <- read_excel("~/Desktop/Projet_SBT13/Projet-SBT13-Addictologie-Github/bdmieRpp2.xls")
# setwd("")


#############################################
### Restructuration de la base de données ###
#############################################

# On ne séléctionne que les personnes âgées de moins de 31 ans.
bd1 <-bd[bd$age<31,]


Nl=dim(bd1)[1] #nombre de lignes

bdscore=data.frame(matrix(data=NA,nrow=Nl,ncol=1))

# ID de l'individu interrogé et du collecteur
# on n'a pas besoin d'utiliser les ID car toutes les données sont rassemblées dans un unique tableau
bdscore$ID_indiv <-bd1[1]
# bdscore$collecteur <- bd1[2]

# Suppression d'une colonne inutile :
bdscore<-bdscore[,-1]

# on transforme les réponses de l'AQOLS en score et on les insére dans la data.frame
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

# Age
bdscore$Age<-bd1$age
# Genre
genreunique <- unique(bd1$sex)
bdscore$Genre <- ifelse(bd1$sex== genreunique[2], 1, ifelse(bd1$sex==genreunique[1], 2,ifelse(bd1$sex== genreunique[5], NA ,ifelse(bd1$sex == genreunique[3], NA,NA))))
# Niveau d'étude après le Bac
niveauunique <- unique(bd1$niv)
bdscore$Niveau <- ifelse(bd1$niv==niveauunique[3], 1, ifelse(bd1$niv==niveauunique[1], 2, ifelse(bd1$niv==niveauunique[4],3, ifelse(bd1$niv==niveauunique[5], 4, ifelse(bd1$niv==niveauunique[6],5, ifelse(bd1$niv==niveauunique[2], 6, NA))))))

## Nivautr (dans le tableau bd1)
# c'est une colonne vide, elle n'a pas été remplie par les personnes interrogées
# Test :
## A <- bd1$nivautre
## N = 16930
## B=matrix(bdscore = NA, nrow=N, ncol = 1)
## for (i in (1:N)){
##  if (is.na(A[i])){B[i]=0}
##  else {B[i]=1}
## }
## S=0
## for (i in (1:N)){
##   if (is.null(B[i])){S=S+1}
## }
## on obtient S = 0

# Discipline
# bdscore$Disc<-bd1$disc --> colonne qualitative, qu'on a préféré scinder en plusieurs 
# colonnes avec un résultat qualitatif
study <- unique(bd1$disc)
bdscore$StudyHuma <- ifelse(bd1$disc==study[3],1,NA)
bdscore$StudyHuma[is.na(bdscore$StudyHuma)]<-0
bdscore$StudyProf <- ifelse(bd1$disc==study[1],1,NA)
bdscore$StudyProf[is.na(bdscore$StudyProf)]<-0
bdscore$StudyLawEco <- ifelse(bd1$disc==study[4],1,NA)
bdscore$StudyLawEco[is.na(bdscore$StudyLawEco)]<-0
bdscore$StudyScience <- ifelse(bd1$disc==study[2],1,NA)
bdscore$StudyScience[is.na(bdscore$StudyScience)]<-0
bdscore$StudyMed <- ifelse(bd1$disc==study[5],1,NA)
bdscore$StudyMed[is.na(bdscore$StudyMed)]<-0
bdscore$StudyAutre <- ifelse(bd1$disc==study[6],1,NA)
bdscore$StudyAutre[is.na(bdscore$StudyAutre)]<-0

# Autre cursus, c'est une donnée qualitative qui nous semble inutilisable
# bdscore$AutreCursus <- bd1[8]

# Fréquence binge-drinking
bdscore$FreqBinge <- ifelse(bd1$frqoh== "Jamais", 0, ifelse(bd1$binge== "non", 0, ifelse(bd1$frqb1=="1 fois", 1, ifelse(bd1$frqb2=="2 fois", 2, ifelse(bd1$frqb3=="3 ? 5 fois", 3, ifelse(bd1$frqb6=="6 ? 9 fois", 4, ifelse(bd1$frqb10=="10 fois ou plus", 5, NA)))))))

# Autres substances
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

#Argent
finunique <- unique(bd1$fin)
bdscore$Argent <- ifelse(bd1$fin==finunique[2], 0, ifelse(bd1$fin == finunique[1], 1, ifelse(bd1$fin==finunique[5], 2, ifelse(bd1$fin==finunique[3], 3, ifelse(bd1$fin==finunique[6], 4, NA)))))

# Audit-C et consommation d'alcool
# Fréquence de consommation d'alcool
frqohunique <- unique(bd1$frqoh)
bdscore$FreqConso <- ifelse(bd1$frqoh==frqohunique[6], 0, ifelse(bd1$frqoh==frqohunique[3], 1, ifelse(bd1$frqoh== frqohunique[2], 2, ifelse(bd1$frqoh == frqohunique[1], 3, ifelse(bd1$frqoh==frqohunique[4], 4, NA)))))
# Nombre de verres consommés en moyenne à une occasion
nbverreunique <- unique(bd1$nbvrtyp)
bdscore$NbVerreMoy <- ifelse(bd1$nbvrtyp==nbverreunique[4], 0, ifelse(bd1$nbvrtyp ==nbverreunique[3], 1, ifelse(bd1$nbvrtyp == nbverreunique[2], 2, ifelse(bd1$nbvrtyp == nbverreunique[5], 3, ifelse(bd1$nbvrtyp ==nbverreunique[1], 4, NA)))))
#Fréquence de consommation de plus de six verres en une occasion
bdscore$FreqSupSixVerre <-bd1$sixvr


# Image
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
# Mobilité
bdscore$Mobilite <- bd1$eqmob
# Autonomie
bdscore$Autonomie <- bd1$eqaut
# Habitudes
bdscore$Habitudes <- bd1$eqhab
# Douleurs/Malaise
bdscore$Douleur <- bd1$eqdoul
# Dépression
bdscore$Depression <- bd1$eqdep

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
# Maladie chronique Booléen
bdscore$MaladieChroniqueBool <- ifelse(bd1$ald=="Oui",1,ifelse(bd1$ald=="Non",0,NA))
# Bourse
bdscore$Bourse <- ifelse(bd1$bours=="Oui",1, ifelse(bd1$bours =="Non",0,NA))

# Nous avons décidé de ne pas analyser la colonne "aldquoi" car les interrogés ont répondu librement

## Exportation de la base de données full_data
write.csv2(bdscore,file="bdscore.csv",row.names = FALSE)

# Descriptions des données
# moyenne, écart-type, nombre de NA dans chaque items

Nom_stats = c("Moyenne","Mediane","Maximum","Minimum","Nb de NA","Ecart-type","Part de NA")
N_stats = length(Nom_stats)

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
  info[5,i-1] <- sum(1*is.na(bdscore[i])) #nb de NA
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

#aggr(bdscore, col=c('navyblue','red'), numbers=TRUE, combined = FALSE, sortVars=TRUE, labels=names(bdscore), cex.axis=.7, gap=3, ylab=c("Histogram of missing bdscore","Pattern"))
# graphique de gauche pour illustrer la part de données manquantes


# imputation
NA_max_col=max(info[7,])
NA_max_row= max(reponses$Pourcent)/100

mat = impute.knn(as.matrix(bdscore),k=100,rowmax=NA_max_row,colmax=NA_max_col)
full_data = as.data.frame(mat$data) 

#Atot
full_data$atot <- full_data$a1 + full_data$a2 + full_data$a3 + full_data$a4 + full_data$a5 + full_data$a6 + full_data$a7 + full_data$a8 + full_data$a9 + full_data$a10 + full_data$a11 + full_data$a12 + full_data$a13 + full_data$a14 + full_data$a15 + full_data$a16 + full_data$a17 + full_data$a18 + full_data$a19 + full_data$a20 + full_data$a21 + full_data$a22 + full_data$a23 + full_data$a24 + full_data$a25+full_data$a26+ full_data$a27+full_data$a28+full_data$a29+ full_data$a30+full_data$a31+full_data$a32+ full_data$a33+ full_data$a34
#Audit-C
full_data$Audit <- full_data$FreqConso + full_data$NbVerreMoy+ full_data$FreqSupSixVerre


## Exportation de la base de données full_data
write.csv2(full_data,file="full_data.csv",row.names = FALSE)


# information sur la nouvelle matrice de données
Ncf=dim(full_data)[2]
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
# j'aimerais évaluer l'écart entre les statistiques de la base de données non corrigée et 
# les statistiques de la base corrigée

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

#aggr(full_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bdscore), cex.axis=.7, gap=3, ylab=c("Histogram of missing bdscore","Pattern"))
# ce dernier affichage est une petite vérification graphique pour s'assurer 
# qu'il n'y a plus de données manquantes



###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################


# Pour cette partie il faut avoir la base full_data. Il faut donc soit exécuter la première partie du code,
# soit en cas de bug utiliser setwd pour définir le chemin vers la BDD et importer la base full_data.

# setwd("")
# full_data=read.csv2("full_data.csv")



###############################
### Correlation de Spearman ###
###############################

# Première étapes de l'étude de la base. Aucune corrélation forte ne semble être présente.

# CorrelationP=matrix(data=NA,nrow=35,ncol=43)
# CorrelationR=matrix(data=NA,nrow=35,ncol=43)
# nomlignes=c()
# nomcolonnes=c()
# for (i in (1:35)){nomlignes=c(nomlignes,names(bdscore[i]))}
# for (i in (36:78)){nomcolonnes=c(nomcolonnes,names(bdscore[i]))}
# rownames(CorrelationP)=nomlignes
# colnames(CorrelationP)=nomcolonnes
# rownames(CorrelationR)=nomlignes
# colnames(CorrelationR)=nomcolonnes
# 
# for (i in (1:35))
# {for (j in (36:78))
# {Testspm=cor.test(as.numeric(unlist(bdscore[i])), as.numeric(unlist(bdscore[j])), method="spearman")
# CorrelationP[i,j-35]=as.numeric(Testspm[3])
# CorrelationR[i,j-35]=as.numeric(Testspm[4])}}
# 
# View(CorrelationP)
# View(CorrelationR)
# 
# persp3D(z = CorrelationP, theta=30,phi=15,xlab='AQoLS',ylab='Consommations',zlab='p-value',expand=0.5,shade=0.8,ticktype="detailed")
# persp3D(z = CorrelationR, theta=30,phi=15,xlab='AQoLS',ylab='Consommations',zlab='R',expand=0.5,shade=0.8,ticktype="detailed")
# 


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

KClusters=Kmeans(full_data,10)


########################################################################
### ACP: Réduction du nombre de dimension et sélection des variables ###
########################################################################

## On applique une ACP sur l'ensemble des données :

ACP <- PCA(full_data,ncp=30)
#La fonction plot.PCA permet d'afficher la représentation des variables () et des individus (Individuals factor map (PCA)) dans le plan des deux premiers facteurs principaux
plot.PCA(ACP,col.quali="blue", label="quali")


## On détermine le nombre de dimension à concerver à l'aide du critère du coude
## Histogramme des valeurs propores
barplot(ACP$eig[1:dim(full_data)[2],2], main="Histogramme des valeurs propres", 
        names.arg=1:dim(full_data)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
        cex.axis=0.8, font.lab=3, ylim=c(0, 12), col="green")



## Nous enlevons les variables proche du centre dans la représentation Variable factor map (PCA)
## Variables retenues :

DBACP=data.frame(matrix(data=NA,nrow=Nl,ncol=1))
DBACP<-DBACP[,-1]
DBACP$a1<-full_data$a1
DBACP$a2<-full_data$a2
DBACP$a3<-full_data$a3
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
DBACP$atot<-full_data$atot
DBACP$Genre<-full_data$Genre
DBACP$Douleur<-full_data$Douleur
DBACP$Depression<-full_data$Depression
DBACP$Mobilité<-full_data$Mobilité
DBACP$Autonomie<-full_data$Autonomie
DBACP$Habitudes<-full_data$Habitudes
DBACP$FeteImagePerso<-full_data$FeteImagePerso
DBACP$FeteEtre<-full_data$FeteEtre
DBACP$FetePerso<-full_data$FetePerso
DBACP$FeteQuotidien<-full_data$FeteQuotidien
DBACP$FeteImageAutre<-full_data$FeteImageAutre
DBACP$Tabac<-full_data$Tabac
DBACP$Cannabis<-full_data$Cannabis
DBACP$NbVerreMoy<-full_data$NbVerreMoy
DBACP$FreqBinge<-full_data$FreqBinge
DBACP$FreqConso<-full_data$FreqConso
DBACP$FreqSupSixVerre<-full_data$FreqSupSixVerre
DBACP$Audit<-full_data$Audit


## Exportation de la base de données DBACP
write.csv2(DBACP,file="DBACP.csv",row.names = FALSE)

## On fait une ACP sur la nouvelle base :

ACP2 <- PCA(DBACP,ncp=9,graph=T)

#La fonction plot.PCA permet d'afficher la représentation des variables 
#(Variable factor map (PCA)) et des individus (Individuals factor map (PCA)) dans 
#le plan des deux premiers facteurs principaux
plot.PCA(ACP2,col.quali="blue", label="quali")


## On détermine le nombre de dimension à concerver à l'aide du critère du coude
## Histogramme des valeurs propores
barplot(ACP2$eig[1:dim(full_data)[2],2], main="Histogramme des valeurs propres", 
        names.arg=1:dim(full_data)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
        cex.axis=0.8, font.lab=3, ylim=c(0, 12), col="orange")
## 9 semble un bon nombre de dimension d'après le critère du coude
print(ACP2$eig)
## Avec 12 dimension on explique 60% de la variabilité


########################################################
### Première classification  hiérarchique ascendante ###
########################################################

## Création d'un Cluster :

#On calcule la matrice des distances de ACP2
Distance=dist(ACP2$ind$coord)

#hclust permet de créer les clusters avec la méthode de WARD
CHA=hclust(Distance,method="ward.D2")

#plot permet de tracer le dendrograme : ATTENTION ! NECESSITE UNE MACHINE PERFORMANTE !
plot(CHA)

#cutree(tree,k) permet de couper le dendrograme pour former k clusters:
Repartition=cutree(CHA,7)

#On regroupe les ligne du premier cluster dans une même base
Cluster1=full_data[Repartition==1,]


##################################################
### Itération de la méthode avec des fonctions ###
##################################################

#ClusterCHA prend en argument la nombre de dimensison de ACP dimacp, le nombre de cluster 
#à créer nbclus, bdscoreACP la base qui regroupe les variables pour l'ACP et full_data la base complète.
#Elle retourne une liste des nbclus Clusters.

ClusterCHA=function(dimacp,nbclus,bdscoreACP,full_data){
  #On applique la méthode de l'Analyse par composantes principales 
  #à l'aide de la fonction PCA du package FactoMineR
  print("ACP encours ... ")
  ACP=PCA(bdscoreACP,ncp=dimacp)
  print(c("Pourcentage de variabilité expliqué :",ACP$eig$`cumulative percentage of variance`[dimacp]))
  plot.PCA(ACP,col.quali="blue", label="quali")
  print("Traçage de l'histogramme des valeurs propres ...")
  barplot(ACP$eig[1:dim(bdscoreACP)[2],2], main="Histogramme des valeurs propres", 
          names.arg=1:dim(bdscoreACP)[2], xlab="Axes", ylab="Pourcentage d'inertie", 
          cex.axis=0.8, font.lab=3, ylim=c(0, 15), col="orange")
  #La fonction plot.PCA permet d'afficher la représentation des variables
  #et des individus (Individuals factor map (PCA)) dans le plan des deux premiers facteurs principaux
  #plot.PCA(ACP,col.quali="blue", label="quali")
  
  # La fonction dist prend comme argument la bdscoreframe et retourne
  #la matrice des distances en utilisant la norme euclidienne
  print("Calcul de la matrice de distance ...")
  Distance=dist(ACP$ind$coord)
  
  # La fonction hclust prend comme argument la bdscoreframe et la 
  # matrice de distances et retourne la Classification ascendante hiérarchique
  print('Formation des clusters ...')
  CHA=hclust(Distance,method="ward.D2")
  
  # Le plot de CHA donne le Dendogramme de la classification hiérarchique
  # print("Traçage du dendogramme")
  # plot(CHA)
  # rect.hclust permet de tracer le dendrograme avec des cadres autour des clusters sélectionné
  # rect.hclust(CHA,nbclus)
  
  # La fonction cutree permet de couper le dendogramme et donne nbclus clusters
  Repartition=cutree(CHA,nbclus)

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

CompareQuantile=function(Clusters,percent){
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

nbclus=5
dimacp=7
Clusters=ClusterCHA(7,5,DBACP,full_data)


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



