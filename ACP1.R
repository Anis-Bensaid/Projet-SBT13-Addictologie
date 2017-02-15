summary(data)
datapropre <- na.omit(data[,1:34])
data.actifs <- datapropre[,1:34]

acp.data <- princomp(data.actifs, cor = 34, scores = 34)
print(acp.data)
print(summary(acp.data))
print(attributes(acp.data))
val.propres <- acp.data$sdev^2
print(val.propres)
#scree plot (graphique des éboulis des valeurs propres)
plot(1:34,val.propres,type="b",ylab="Valeurs
     propres",xlab="Composante",main="Scree plot")
#intervalle de confiance des val.propres à 95% (cf.Saporta, page 172)
val.basse <- val.propres * exp(-1.96 * sqrt(2.0/(n-1)))
val.haute <- val.propres * exp(+1.96 * sqrt(2.0/(n-1)))
#affichage sous forme de tableau
tableau <- cbind(val.basse,val.propres,val.haute)
colnames(tableau) <- c("B.Inf.","Val.","B.Sup")
print(tableau,digits=3)

#**** corrélation variables-facteurs ****
c1 <- acp.data$loadings[,1]*acp.data$sdev[1]
c2 <- acp.data$loadings[,2]*acp.data$sdev[2]
#affichage
correlation <- cbind(c1,c2)
print(correlation,digits=2)

#*** cercle des corrélations - variables actives ***
plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
abline(h=0,v=0)
text(c1,c2,labels=colnames(data.actifs),cex=0.5)
symbols(0,0,circles=1,inches=F,add=34)

plot(acp.data$scores[,1],acp.data$scores[,2],type="n",xlab="Comp.1 -
74%",ylab="Comp.2 - 14%")
abline(h=0,v=0)
text(acp.data$scores[,1],acp.data$scores[,2],labels=rownames(data.actifs),cex=0.75)

biplot(acp.data,cex=0.75)