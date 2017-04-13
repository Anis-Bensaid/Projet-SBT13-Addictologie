rm(list=ls())
# install.packages("readxl")
# install.packages("plot3D")
# install.packages("FactoMineR") 
library(plot3D)
library(FactoMineR)
library(readxl)

# Haim base de données
#bd <- read_excel("~/Desktop/Projet_SBT13/Projet-SBT13-Addictologie-Github/bdmieRpp2.xls")

# Arthur Base de données
# bd <- read_excel("~/Documents/Projet Enjeux/Projet-SBT13-Addictologie/bdmieRpp2.xls")

#Benjamin base de donn?es
#bd<- read_excel("~/GitHub/Projet-SBT13-Addictologie/bdmieRpp2.xls")


# Emilio Base de données
#bd <- read_excel("C:/Users/Emilio/Desktop/intercambio/clases/enjeux/sbt/Projet-SBT13-Addictologie/bdmieRpp2.xls")


# Anis Base de données
#bd <- read_excel("D:/Users/enysb/Google Drive/Etudes/Git/Projet-SBT13-Addictologie/bdmieRpp2.xls")


bd1 <-bd[bd$age<31,]
# on cherche la corrélation entre chaque item de AQOLS contre tout le reste

# Il faut transformer les réponses aux autres questions en score et les ranger dans une matrice
Nl=dim(bd1)[1] #nombre de lignes

data=data.frame(matrix(data=NA,nrow=Nl,ncol=1))

# ID de l'individu interrogé et du collecteur
# ID de l'individu interrogé et du collecteur
# on n'a pas besoin d'utiliser les ID car toutes les données sont rassemblées dans un unique tableau
data$ID_indiv <-bd1[1]
# data$collecteur <- bd1[2]

# Suppression d'une colonne inutile :
data<-data[,-1]

# on transforme les réponses de l'AQOLS en score et on les insére dans la data.frame
data$a1 <- ifelse(bd1$A1=="Pas du tout", 0, ifelse(bd1$A1=="Un peu", 1,ifelse(bd1$A1== "Beaucoup", 2,ifelse(bd1$A1 == "Enormément", 3,NA))))
data$a2 <- ifelse(bd1$A2=="Pas du tout", 0, ifelse(bd1$A2=="Un peu", 1,ifelse(bd1$A2== "Beaucoup", 2,ifelse(bd1$A2 == "Enormément", 3,NA))))
data$a3 <- ifelse(bd1$A3=="Pas du tout", 0, ifelse(bd1$A3=="Un peu", 1,ifelse(bd1$A3== "Beaucoup", 2,ifelse(bd1$A3 == "Enormément", 3,NA))))
data$a4 <- ifelse(bd1$A4=="Pas du tout", 0, ifelse(bd1$A4=="Un peu", 1,ifelse(bd1$A4== "Beaucoup", 2,ifelse(bd1$A4 == "Enormément", 3,NA))))
data$a5 <- ifelse(bd1$A5=="Pas du tout", 0, ifelse(bd1$A5=="Un peu", 1,ifelse(bd1$A5== "Beaucoup", 2,ifelse(bd1$A5== "Enormément", 3,NA))))
data$a6 <- ifelse(bd1$A6=="Pas du tout", 0, ifelse(bd1$A6=="Un peu", 1,ifelse(bd1$A6== "Beaucoup", 2,ifelse(bd1$A6 == "Enormément", 3,NA))))
data$a7 <- ifelse(bd1$A7=="Pas du tout", 0, ifelse(bd1$A7=="Un peu", 1,ifelse(bd1$A7== "Beaucoup", 2,ifelse(bd1$A7 == "Enormément", 3,NA))))
data$a8 <- ifelse(bd1$A8=="Pas du tout", 0, ifelse(bd1$A8=="Un peu", 1,ifelse(bd1$A8== "Beaucoup", 2,ifelse(bd1$A8 == "Enormément", 3,NA))))
data$a9 <- ifelse(bd1$A9=="Pas du tout", 0, ifelse(bd1$A9=="Un peu", 1,ifelse(bd1$A9== "Beaucoup", 2,ifelse(bd1$A9 == "Enormément", 3,NA))))
data$a10 <- ifelse(bd1$A10=="Pas du tout", 0, ifelse(bd1$A10=="Un peu", 1,ifelse(bd1$A10== "Beaucoup", 2,ifelse(bd1$A10 == "Enormément", 3,NA))))
data$a11 <- ifelse(bd1$A11=="Pas du tout", 0, ifelse(bd1$A11=="Un peu", 1,ifelse(bd1$A11== "Beaucoup", 2,ifelse(bd1$A11 == "Enormément", 3,NA))))
data$a12 <- ifelse(bd1$A12=="Pas du tout", 0, ifelse(bd1$A12=="Un peu", 1,ifelse(bd1$A12== "Beaucoup", 2,ifelse(bd1$A12 == "Enormément", 3,NA))))
data$a13 <- ifelse(bd1$A13=="Pas du tout", 0, ifelse(bd1$A13=="Un peu", 1,ifelse(bd1$A13== "Beaucoup", 2,ifelse(bd1$A13 == "Enormément", 3,NA))))
data$a14 <- ifelse(bd1$A14=="Pas du tout", 0, ifelse(bd1$A14=="Un peu", 1,ifelse(bd1$A14== "Beaucoup", 2,ifelse(bd1$A14 == "Enormément", 3,NA))))
data$a15 <- ifelse(bd1$A15=="Pas du tout", 0, ifelse(bd1$A15=="Un peu", 1,ifelse(bd1$A15== "Beaucoup", 2,ifelse(bd1$A15 == "Enormément", 3,NA))))
data$a16 <- ifelse(bd1$A16=="Pas du tout", 0, ifelse(bd1$A16=="Un peu", 1,ifelse(bd1$A16== "Beaucoup", 2,ifelse(bd1$A16 == "Enormément", 3,NA))))
data$a17 <- ifelse(bd1$A17=="Pas du tout", 0, ifelse(bd1$A17=="Un peu", 1,ifelse(bd1$A17== "Beaucoup", 2,ifelse(bd1$A17 == "Enormément", 3,NA))))
data$a18 <- ifelse(bd1$A18=="Pas du tout", 0, ifelse(bd1$A18=="Un peu", 1,ifelse(bd1$A18== "Beaucoup", 2,ifelse(bd1$A18 == "Enormément", 3,NA))))
data$a19 <- ifelse(bd1$A19=="Pas du tout", 0, ifelse(bd1$A19=="Un peu", 1,ifelse(bd1$A19== "Beaucoup", 2,ifelse(bd1$A19 == "Enormément", 3,NA))))
data$a20 <- ifelse(bd1$A20=="Pas du tout", 0, ifelse(bd1$A20=="Un peu", 1,ifelse(bd1$A1== "Beaucoup", 2,ifelse(bd1$A1 == "Enormément", 3,NA))))
data$a21 <- ifelse(bd1$A1=="Pas du tout", 0, ifelse(bd1$A21=="Un peu", 1,ifelse(bd1$A21== "Beaucoup", 2,ifelse(bd1$A21 == "Enormément", 3,NA))))
data$a22 <- ifelse(bd1$A22=="Pas du tout", 0, ifelse(bd1$A22=="Un peu", 1,ifelse(bd1$A22== "Beaucoup", 2,ifelse(bd1$A22 == "Enormément", 3,NA))))
data$a23 <- ifelse(bd1$A23=="Pas du tout", 0, ifelse(bd1$A23=="Un peu", 1,ifelse(bd1$A23== "Beaucoup", 2,ifelse(bd1$A23 == "Enormément", 3,NA))))
data$a24 <- ifelse(bd1$A24=="Pas du tout", 0, ifelse(bd1$A24=="Un peu", 1,ifelse(bd1$A24== "Beaucoup", 2,ifelse(bd1$A24 == "Enormément", 3,NA))))
data$a25 <- ifelse(bd1$A25=="Pas du tout", 0, ifelse(bd1$A25=="Un peu", 1,ifelse(bd1$A25== "Beaucoup", 2,ifelse(bd1$A25== "Enormément", 3,NA))))
data$a26 <- ifelse(bd1$A26=="Pas du tout", 0, ifelse(bd1$A26=="Un peu", 1,ifelse(bd1$A26== "Beaucoup", 2,ifelse(bd1$A26 == "Enormément", 3,NA))))
data$a27 <- ifelse(bd1$A27=="Pas du tout", 0, ifelse(bd1$A27=="Un peu", 1,ifelse(bd1$A27== "Beaucoup", 2,ifelse(bd1$A27 == "Enormément", 3,NA))))
data$a28 <- ifelse(bd1$A28=="Pas du tout", 0, ifelse(bd1$A28=="Un peu", 1,ifelse(bd1$A28== "Beaucoup", 2,ifelse(bd1$A28 == "Enormément", 3,NA))))
data$a29 <- ifelse(bd1$A29=="Pas du tout", 0, ifelse(bd1$A29=="Un peu", 1,ifelse(bd1$A29== "Beaucoup", 2,ifelse(bd1$A29 == "Enormément", 3,NA))))
data$a30 <- ifelse(bd1$A30=="Pas du tout", 0, ifelse(bd1$A30=="Un peu", 1,ifelse(bd1$A30== "Beaucoup", 2,ifelse(bd1$A30 == "Enormément", 3,NA))))
data$a31 <- ifelse(bd1$A11=="Pas du tout", 0, ifelse(bd1$A31=="Un peu", 1,ifelse(bd1$A31== "Beaucoup", 2,ifelse(bd1$A31 == "Enormément", 3,NA))))
data$a32 <- ifelse(bd1$A32=="Pas du tout", 0, ifelse(bd1$A32=="Un peu", 1,ifelse(bd1$A32== "Beaucoup", 2,ifelse(bd1$A32 == "Enormément", 3,NA))))
data$a33 <- ifelse(bd1$A33=="Pas du tout", 0, ifelse(bd1$A33=="Un peu", 1,ifelse(bd1$A33== "Beaucoup", 2,ifelse(bd1$A33 == "Enormément", 3,NA))))
data$a34 <- ifelse(bd1$A34=="Pas du tout", 0, ifelse(bd1$A34=="Un peu", 1,ifelse(bd1$A34== "Beaucoup", 2,ifelse(bd1$A34 == "Enormément", 3,NA))))
data$atot <- data$a1+data$a2+data$a3+ data$a4+data$a5+data$a6+ data$a7+data$a8+data$a9+ data$a10+data$a11+data$a12+ data$a13+ data$a14+data$a15+data$a16+ data$a17+ data$a18+data$a19+ data$a20+data$a21+data$a22+ data$a23+ data$a24+data$a25+data$a26+ data$a27+data$a28+data$a29+ data$a30+data$a31+data$a32+ data$a33+ data$a34

# Age
data$Age<-bd1$age
# Genre
data$Genre <- ifelse(bd1$sex=="Un homme", 1, ifelse(bd1$sex=="Une femme", 2,ifelse(bd1$sex== "Indéterminé", NA ,ifelse(bd1$sex == "Ne sait pas", NA,NA))))
# Niveau d'étude après le Bac
data$Niveau <- ifelse(bd1$niv=="Bac +1", 1, ifelse(bd1$niv=="Bac +2", 2, ifelse(bd1$niv=="Bac +3",3, ifelse(bd1$niv=="Bac +4", 4, ifelse(bd1$niv=="Bac +5", 5, ifelse(bd1$niv=="Bac>+5", 6, NA))))))

## Nivautr (dans le tableau bd1)
# c'est une colonne vide, elle n'a pas été remplie par les personnes interrogées
# Test :
## A <- bd1$nivautre
## N = 16930
## B=matrix(data = NA, nrow=N, ncol = 1)
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
# data$Disc<-bd1$disc --> colonne qualitative, qu'on a préféré scinder en plusieurs colonnes avec un résultat
# qualitatif
data$StudyHuma <- ifelse(bd1$disc=="Sciences Humaines et sociales / Lettres / Langues / Art",1,NA)
data$StudyHuma[is.na(data$StudyHuma)]<--0
data$StudyProf <- ifelse(bd1$disc=="Enseignement / STAPS",1,NA)
data$StudyProf[is.na(data$StudyProf)]<--0
data$StudyLawEco <- ifelse(bd1$disc=="Droit / Eco-gestion / Management",1,NA)
data$StudyLawEco[is.na(data$StudyLawEco)]<--0
data$StudyScience <- ifelse(bd1$disc=="Sciences / Ingénierie /Architecture",1,NA)
data$StudyScience[is.na(data$StudyScience)]<--0
data$StudyMed <- ifelse(bd1$disc=="Santé",1,NA)
data$StudyMed[is.na(data$StudyMed)]<--0
data$StudyMed <- ifelse(bd1$disc=="Santé",1,NA)
data$StudyMed[is.na(data$StudyMed)]<--0
data$StudyAutre <- ifelse(bd1$disc=="Autre (veuillez préciser)",1,NA)
data$StudyAutre[is.na(data$StudyAutre)]<--0

# Autre cursus, c'est une donnée qualitative qui nous semble inutilisable
# data$AutreCursus <- bd1[8]

# Fréquence binge-drinking
data$FreqBinge <- ifelse(bd1$frqoh== "Jamais", 0, ifelse(bd1$binge== "non", 0, ifelse(bd1$frqb1=="1 fois", 1, ifelse(bd1$frqb2=="2 fois", 2, ifelse(bd1$frqb3=="3 à 5 fois", 3, ifelse(bd1$frqb6=="6 à 9 fois", 4, ifelse(bd1$frqb10=="10 fois ou plus", 5, NA)))))))
# Autres substances
data$Tabac <- ifelse(bd1$tbc== "jamais consommé", 0, ifelse(bd1$tbc== "il y a plus d'un an", 1, ifelse(bd1$tbc=="au cours de la dernière année", 1, ifelse(bd1$tbc=="au cours du mois dernier", 2, ifelse(bd1$tbc=="au cours de la dernière semaine", 3, NA)))))
data$Cannabis <- ifelse(bd1$thc== "jamais consommé", 0, ifelse(bd1$thc== "il y a plus d'un an", 1, ifelse(bd1$thc=="au cours de la dernière année", 1, ifelse(bd1$thc=="au cours du mois dernier", 2, ifelse(bd1$thc=="au cours de la dernière semaine", 3, NA)))))
data$Cocaine <- ifelse(bd1$coc== "jamais consommé", 0, ifelse(bd1$coc == "il y a plus d'un an", 1, ifelse(bd1$coc=="au cours de la dernière année", 1, ifelse(bd1$coc=="au cours du mois dernier", 2, ifelse(bd1$coc=="au cours de la dernière semaine", 3, NA)))))
data$Heroine <- ifelse(bd1$hero== "jamais consommé", 0, ifelse(bd1$hero== "il y a plus d'un an", 1, ifelse(bd1$hero=="au cours de la dernière année", 1, ifelse(bd1$hero=="au cours du mois dernier", 2, ifelse(bd1$hero=="au cours de la dernière semaine", 3, NA)))))
data$MD <- ifelse(bd1$md== "jamais consommé", 0, ifelse(bd1$md== "il y a plus d'un an", 1, ifelse(bd1$md=="au cours de la dernière année", 1, ifelse(bd1$md=="au cours du mois dernier", 2, ifelse(bd1$md=="au cours de la dernière semaine", 3, NA)))))
data$Poppers <- ifelse(bd1$pop== "jamais consommé", 0, ifelse(bd1$pop== "il y a plus d'un an", 1, ifelse(bd1$pop=="au cours de la dernière année", 1, ifelse(bd1$pop=="au cours du mois dernier", 2, ifelse(bd1$pop=="au cours de la dernière semaine", 3, NA)))))
data$Jeu <- ifelse(bd1$jeu== "jamais consommé", 0, ifelse(bd1$jeu== "il y a plus d'un an", 1, ifelse(bd1$jeu=="au cours de la dernière année", 1, ifelse(bd1$jeu=="au cours du mois dernier", 2, ifelse(bd1$jeu=="au cours de la dernière semaine", 3, NA)))))
#Argent
data$Argent <- ifelse(bd1$fin=="Pas de difficultés financières  -    0", 0, ifelse(bd1$fin == "1.000000", 1, ifelse(bd1$fin=="2.000000", 2, ifelse(bd1$fin=="3.000000", 3, ifelse(bd1$fin=="Difficultés financières trés importantes     -   4", 4, NA)))))
# Audit-C et consommation d'alcool
# Fréquence de consommation d'alcool
data$FreqConso <- ifelse(bd1$frqoh=="Jamais", 0, ifelse(bd1$frqoh=="Une fois par mois ou moins", 1, ifelse(bd1$frqoh== "2 à 4 fois par mois", 2, ifelse(bd1$frqoh == "2 à 3 fois par semaine", 3, ifelse(bd1$frqoh=="4 fois par semaine ou plus", 4, NA)))))
# Nombre de verres consommés en moyenne à une occasion
data$NbVerreMoy <- ifelse(bd1$nbvrtyp=="1 ou 2", 0, ifelse(bd1$nbvrtyp =="3 ou 4", 1, ifelse(bd1$nbvrtyp == "5 ou 6", 2, ifelse(bd1$nbvrtyp == "7 à 9", 3, ifelse(bd1$nbvrtyp =="10 ou plus", 4, NA)))))
#Fréquence de consommation de plus de six verres en une occasion
data$FreqSupSixVerre <-bd1$sixvr
#Audit-C
data$Audit <- data$FreqConso + data$NbVerreMoy+ data$FreqSupSixVerre
# Image
# Faire la fête fait partie de l'image que j'ai de moi
data$FeteImagePerso <- bd1$idt1
# Faire la fête fait partie de "qui je suis"
data$FeteEtre <- bd1$idt2
# Faire la fête fait partie de ma personnalité
data$FetePerso <-bd1$idt3
# Faire la fête fait partie de mon quotidien
data$FeteQuotidien <- bd1$idt4
# Les autres considérent que faire la fête fait partie de ma personnalité
data$FeteImageAutre <- bd1$idt5
# Mobilité
data$Mobilité <- bd1$eqmob
# Autonomie
data$Autonomie <- bd1$eqaut
# Habitudes
data$Habitudes <- bd1$eqhab
# Douleurs/Malaise
data$Douleur <- bd1$eqdoul
# Dépression
data$Depression <- bd1$eqdep

# Lieu de résidence : Famille/tuteur, logement indépendant, résidence collective, ailleurs
data$LogFamille <- ifelse(bd1$logou=="Chez mes 2 parents /Chez ma mère / Chez mon père /Chez un autre membre de ma famille (oncle, tante...) / Chez mon tuteur",1,NA)
data$LogFamille[is.na(data$LogFamille)]<--0
data$LogInd <--ifelse(bd1$logou=="Dans un logement indépendant (en location, en colocation, dans un logement dont je suis propriétaire, au domicile d’un autre membre de ma famille...)",1,NA)
data$LogInd [is.na(data$LogInd )]<--0
data$LogRes <--ifelse(bd1$logou=="En résidence collective (foyer, internat, résidence universitaire...)",1,NA)
data$LogRes[is.na(data$LogRes)]<--0
data$LogAutre <--ifelse(bd1$logou=="Ailleurs",1,NA)
data$LogAutre[is.na(data$LogAutre)]<--0
# Seul
data$Seul <- ifelse(bd1$logwho1=="Je vis seul-e",1,NA)
data$Seul[is.na(data$Seul)]<--0
# En couple
data$Couple <- ifelse(bd1$logwho2=="Je vis en couple avec mon/ma petit-e ami-e ou conjoint-e",1,NA)
data$Couple[is.na(data$Couple)]<--0
# Avec les enfants
data$Enfants <- ifelse(bd1$logwho3=="Avec mes enfants",1,NA)
data$Enfants[is.na(data$Enfants)]<--0
# Colocation avec amis
data$ColocFriend <- ifelse(bd1$logwho4=="En colocation avec un ou des ami-e(s)",1,NA)
data$ColocFriend[is.na(data$ColocFriend)]<--0
# Colocation avec autres personnes
data$ColocAutres <- ifelse(bd1$logwho5=="En colocation avec une ou plusieurs autres personnes",1,NA)
data$ColocAutres[is.na(data$ColocAutres)]<--0
# Maladie chronique Booléen
data$MaladieChroniqueBool <- ifelse(bd1$ald=="Oui",1,ifelse(bd1$ald=="Non",0,NA))
# Bourse
data$Bourse <- ifelse(bd1$bours=="Oui",1, ifelse(bd1$bours =="Non",0,NA))


# Nous avons décidé de ne pas analyser la colonne "aldquoi" car les interrogés ont répondu librement

# Descriptions des données
# moyenne, écart-type, nombre de NA dans chaque items

Nom_stats = c("Moyenne","Mediane","Maximum","Minimum","Nb de NA","Ecart-type")
N_stats = length(Nom_stats)

Nc=dim(data)[2] # nombre d'items
info=data.frame(matrix(data=NA,nrow=N_stats,ncol=Nc-1))
rownames(info) <- Nom_stats
colnames(info) <- colnames(data)[2:Nc]


for (i in (2:Nc)) {
  y=data[i]
  info[1,i-1]<-apply(na.omit(y),2,mean) # moyenne
  info[2,i-1] <-apply(na.omit(y),2,median) # médiane
  info[3,i-1] <- max(na.omit(y)) # maximum
  info[4,i-1] <- min(na.omit(y)) # Minimum
  info[5,i-1] <- length(data[i][is.na(data[i])]) #nb de NA
  info[6,i-1] <- apply(na.omit(y), 2, sd) # écart-type
}

# Données manquantes 
# Taux de réponse de chaque individu et individu dont le nombre de reponses sont insuffisants

reponses=1*cbind(data[1],is.na(data[2:Nc])) # le 1* permet de changer les False/True en 0/1
reponses$Total <- rowSums(reponses[,3:Nc]) # nombre d'items où l'individu n'a pas repondu 
reponses$Pourcent <- 100*reponses$Total/Nc # taux de "non-réponse" 
faible_taux=reponses[reponses$Pourcent>60,] 
fort_taux=reponses[reponses$Pourcent<=0,]

taux_global=100*sum(reponses$Total)/(Nc*Nl) # taux global de réponses manquantes

# Méthode des plus proches voisins



################################################
### Correlation de Spearman :
################################################

CorrelationP=matrix(data=NA,nrow=35,ncol=43)
CorrelationR=matrix(data=NA,nrow=35,ncol=43)
nomlignes=c()
nomcolonnes=c()
for (i in (1:35)){nomlignes=c(nomlignes,names(data[i]))}
for (i in (36:78)){nomcolonnes=c(nomcolonnes,names(data[i]))}
rownames(CorrelationP)=nomlignes
colnames(CorrelationP)=nomcolonnes
rownames(CorrelationR)=nomlignes
colnames(CorrelationR)=nomcolonnes

for (i in (1:35))
{for (j in (36:78))
{Testspm=cor.test(as.numeric(unlist(data[i])), as.numeric(unlist(data[j])), method="spearman")
CorrelationP[i,j-35]=as.numeric(Testspm[3])
CorrelationR[i,j-35]=as.numeric(Testspm[4])}}

View(CorrelationP)
View(CorrelationR)

persp3D(z = CorrelationP, theta=30,phi=15,xlab='AQoLS',ylab='Consommations',zlab='p-value',expand=0.5,shade=0.8,ticktype="detailed")
persp3D(z = CorrelationR, theta=30,phi=15,xlab='AQoLS',ylab='Consommations',zlab='R',expand=0.5,shade=0.8,ticktype="detailed")



#######################################################
### K-means
######################################################
k=5
clus= kmeans(na.omit(data), k, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
(clus$cluster)



#############################################
### ACP :
#############################################


res_pca <- PCA(data,ncp=30)

#La fonction plot.PCA permet d'afficher la représentation des variables () et des individus (Individuals factor map (PCA)) dans le plan des deux premiers facteurs principaux
plot.PCA(res_pca,col.quali="blue", label="quali")



#####################################################
### classification  hiérarchique ascendante
#####################################################

# Les calcules pour faire la Classification ascendante hiérarchique sont trop lourds vu la quantité de données dont on dispose. 
# On décide de réduire la matrice des distance en réduisant le nombre de dimension à l'aide de l'ACP.



# La fonction dist prend comme argument la dataframe et retourne la matrice des distances en utilisant la norme euclidienne
distdataacp=dist(res_pca$ind$coord)

# La fonction hclust prend comme argument la dataframe et la matrice de distances et retourne la Classification ascendante hiérarchique
cha=hclust(distdataacp,method="ward.D2")

# Le plot de cah.ward donne le Dendogramme de la classification hiérarchique
# plot(CHA)

rect.hclust(cha,10)

clustercha=cutree(cha, 10)



ClusterCHA=function(dimacp,nbclus,data){
  #On applique la méthode de l'Analyse par composantes principales 
  #à l'aide de la fonction PCA du package FactoMineR
  ACP=PCA(data,ncp=dimacp)
  
  #La fonction plot.PCA permet d'afficher la représentation des variables
  #et des individus (Individuals factor map (PCA)) dans le plan des deux premiers facteurs principaux
  #plot.PCA(ACP,col.quali="blue", label="quali")
  
  # La fonction dist prend comme argument la dataframe et retourne
  #la matrice des distances en utilisant la norme euclidienne
  Distance=dist(ACP$ind$coord)
  
  # La fonction hclust prend comme argument la dataframe et la 
  # matrice de distances et retourne la Classification ascendante hiérarchique
  CHA=hclust(Distance,method="ward.D2")
  
  # Le plot de cah.ward donne le Dendogramme de la classification hiérarchique
  # plot(CHA)
  # rect.hclust(CHA,nbclus)
  
  # La fonction cutree permet de couper le dendogramme et donne nbclus clusters
  Repartition=cutree(CHA,nbclus)
  
  # On range les clusters dans une liste Clusters de dataframes
  Clusters=list()
  for (i in 1:nbclus){
    Clusters[[i]]=data[Repartition==i,]
  }
  return(Clusters)
}

Clusters=ClusterCHA(30,10,data)




# regression PLS
# regarder les question où il y a le plus de données manquantes et peut-être les enlever.
# complete case
# regarder le nombre de na par lignes
# Enregistrer les variables saveRDS
# méthodes explicatives : Anova ou faire des ACP sur les consommation et des ACP sur les quali.

