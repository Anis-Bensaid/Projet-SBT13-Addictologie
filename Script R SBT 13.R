rm(list=ls())
# install.packages("readxl")
library(readxl)
bd <- read_excel("~/Desktop/Projet_SBT13/Github/SBT13/bdmieRpp2.xls")
bd1 <-bd[bd$age<31,]
# on cherche la corrélation entre chaque item de AQOLS contre tout le reste 

# Il faut transformer les réponses aux autres questions en score et les ranger dans une matrice
Nl=dim(bd1)[1] #nombre de lignes
Nc=dim(bd1)[2] # nombre de colonnes dans la base de données
data=data.frame(matrix(data=NA,nrow=Nl,ncol=1))

# on transforme les réponses é l'AQOLS en score et on les insére dans la data.frame

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

# suppression d'une colonne inutile :
data<-data[,-1]
# Genre
data$Genre <- ifelse(bd1$sex=="Un homme", 1, ifelse(bd1$sex=="Une femme", 2,ifelse(bd1$sex== "Indéterminé", NA ,ifelse(bd1$sex == "Ne sait pas", NA,NA))))
# Fréquence binge-drinking
data$FreqBinge <- ifelse(bd1$frqoh== "Jamais", 0, ifelse(bd1$binge== "non", 0, ifelse(bd1$frqb1=="1 fois", 1, ifelse(bd1$frqb2=="2 fois", 2, ifelse(bd1$frqb3=="3 é 5 fois", 3, ifelse(bd1$frqb6=="6 é 9 fois", 4, ifelse(bd1$frqb10=="10 fois ou plus", 5, NA)))))))
# Autres substances 
data$Tabac <- ifelse(bd1$tbc== "jamais consommé", 0, ifelse(bd1$tbc== "il y a plus d'un an", 1, ifelse(bd1$tbc=="au cours de la derniére année", 1, ifelse(bd1$tbc=="au cours du mois dernier", 2, ifelse(bd1$tbc=="au cours de la derniére semaine", 3, NA)))))
data$Cannabis <- ifelse(bd1$thc== "jamais consommé", 0, ifelse(bd1$thc== "il y a plus d'un an", 1, ifelse(bd1$thc=="au cours de la derniére année", 1, ifelse(bd1$thc=="au cours du mois dernier", 2, ifelse(bd1$thc=="au cours de la derniére semaine", 3, NA)))))
data$Cocaine <- ifelse(bd1$coc== "jamais consommé", 0, ifelse(bd1$coc == "il y a plus d'un an", 1, ifelse(bd1$coc=="au cours de la derniére année", 1, ifelse(bd1$coc=="au cours du mois dernier", 2, ifelse(bd1$coc=="au cours de la derniére semaine", 3, NA)))))
data$Heroine <- ifelse(bd1$hero== "jamais consommé", 0, ifelse(bd1$hero== "il y a plus d'un an", 1, ifelse(bd1$hero=="au cours de la derniére année", 1, ifelse(bd1$hero=="au cours du mois dernier", 2, ifelse(bd1$hero=="au cours de la derniére semaine", 3, NA)))))
data$MD <- ifelse(bd1$md== "jamais consommé", 0, ifelse(bd1$md== "il y a plus d'un an", 1, ifelse(bd1$md=="au cours de la derniére année", 1, ifelse(bd1$md=="au cours du mois dernier", 2, ifelse(bd1$md=="au cours de la derniére semaine", 3, NA)))))
data$Poppers <- ifelse(bd1$pop== "jamais consommé", 0, ifelse(bd1$pop== "il y a plus d'un an", 1, ifelse(bd1$pop=="au cours de la derniére année", 1, ifelse(bd1$pop=="au cours du mois dernier", 2, ifelse(bd1$pop=="au cours de la derniére semaine", 3, NA)))))
data$Jeu <- ifelse(bd1$jeu== "jamais consommé", 0, ifelse(bd1$jeu== "il y a plus d'un an", 1, ifelse(bd1$jeu=="au cours de la derniére année", 1, ifelse(bd1$jeu=="au cours du mois dernier", 2, ifelse(bd1$jeu=="au cours de la derniére semaine", 3, NA)))))
#Argent
data$Argent <- ifelse(bd1$fin=="Pas de difficultés financiéres  -    0", 0, ifelse(bd1$fin == "1", 1, ifelse(bd1$fin=="2", 2, ifelse(bd1$fin=="3", 3, ifelse(bd1$fin=="Difficultés financiéres trés importantes     -   4", 4, NA)))))
# Audit-C et consommation d'alcool
# Fréquence de consommation d'alcool
data$FreqConso <- ifelse(bd1$frqoh=="Jamais", 0, ifelse(bd1$frqoh=="Une fois par mois ou moins", 1, ifelse(bd1$frqoh== "2 é 4 fois par mois", 2, ifelse(bd1$frqoh == "2 é 3 fois par semaine", 3, ifelse(bd1$frqoh=="4 fois par semaine ou plus", 4, NA)))))
# Nombre de verres consommés en moyenne é une occasion
data$NbVerreMoy <- ifelse(bd1$nbvrtyp=="1 ou 2", 0, ifelse(bd1$nbvrtyp =="3 ou 4", 1, ifelse(bd1$nbvrtyp == "5 ou 6", 2, ifelse(bd1$nbvrtyp == "7 é 9", 3, ifelse(bd1$nbvrtyp =="10 ou plus", 4, NA)))))
#Fréquence de consommation de plus de six verres en une occasion
data$FreqSupSixVerre <-bd1$sixvr
#Audit-C
data$Audit <- data$FreqConso + data$NbVerreMoy+ data$FreqSupSixVerre
# Image
# Faire la féte fait partie de l'image que j'ai de moi
data$FeteImagePerso <- bd1$idt1
# Faire la féte fait partie de "qui je suis"
data$FeteEtre <- bd1$idt2
# Faire la féte fait partie de ma personnalité
data$FetePerso <-bd1$idt3
# Faire la féte fait partie de mon quotidien
data$FeteQuotidien <- bd1$idt4
# Les autres considérent que faire la féte fait partie de ma personnalité
data$FeteImageAutre <- bd1$idt5

Correlation=matrix(data=NA,nrow=35,ncol=18)
nomlignes=c()
nomcolonnes=c()

for (i in (1:35)){nomlignes=c(nomlignes,names(data[i]))}

for (i in (36:52)){nomcolonnes=c(nomcolonnes,names(data[i]))}

for (i in (1:35))  
  
{for (j in (36:52)) 
{Correlation[i,j-35]=as.numeric(cor.test(as.numeric(unlist(data[i])), as.numeric(unlist(data[j])), method="spearman")[3])}}


rownames(Correlation)=nomlignes
colnames(Correlation)=nomcolonnes

persp(z = Correlation, ,theta=30,phi=15,xlab='AQoLS',ylab='Consommations',zlab='p-value',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")



