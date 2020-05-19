############################
### Chargement � adapter ###
############################
#setwd("C:/Users/csoub/OneDrive/Bureau/DM_STA203/input")
getwd()
load("input/music.RData")

p=ncol(music)
n=nrow(music)

############################
###      Question 1      ###
############################
# La methode Ridge est une methode de r�gularisation en classification supervis�. Cette methode � pour but de pallier au defaut de contrainte dans un jeu de donn�es
# o� le nombre de variables est plus important que le nombre d'observation (cadre des grandes dimensions).
# L'objectif est alors d'ajouter une contrainte afin de pouvoir malgr� tout conserver nos variables explicatives malgr� la non injectivit�.
# L'inconv�niant r�site en le fait que l'estimateur obtenu est biais� mais que sa variance est meilleure que celle obtenue par les moindres carr�.
# Il faut alors trouv� un compromis biais/variance.

# Dans cette �tude ? 
# � completer


############################
###      Question 2      ###
############################
library(glmnet)

set.seed(103)
train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 

var = c("PAR_TC", "PAR_SC", "PAR_SC_V","PAR_ASC_V" , "PAR_ASE_M", "PAR_ASE_MV")
X_subset = music[var] 
X_subset$PAR_SC_V=log(X_subset$PAR_SC_V)
X_subset$PAR_ASC_V=log(X_subset$PAR_ASC_V)
# X_subset = music[-p]
# X_subset = X_subset[-148:-167] #j'enl�ve quand meme les r�p�titions 
Y = 1*(music$GENRE == 'Classical')
X_train = X_subset[train,]
Y_train = Y[train]
X_test = X_subset[!train,]
Y_test = Y[!train]


grid=10^seq(10,-2,length=100)
x=as.matrix(X_train)
y=as.matrix(Y_train)
ridge.fit=glmnet(x,y,alpha=0,lambda=grid)
coef.ridge=coef(ridge.fit)[-1,] # l'intercept n'apporte rien

###### Norme L2
matplot(apply(coef.ridge^2,2,mean),t(coef.ridge),
        col=1:length(names(X_train)), lty=length(names(X_train)), type="l",xlab="norme L2", ylab="coefficients")
legend("bottomleft", names(X_train),lty=1:length(names(X_train)),col=1:length(names(X_train)),cex=0.5)

###### Norme L1
matplot(apply(abs(coef.ridge),2,sum),t(coef.ridge), main="ridge",
        col=1:length(names(X_train)),lty=1:length(names(X_train)),type="l",xlab="norme L1", ylab="coefficients")  
legend("bottomleft", names(X_train),lty=1:length(names(X_train)),col=1:length(names(X_train)),cex=0.5)

###### Plot classique
plot(ridge.fit)


# Pour des valeurs de lambda tr�s grande (comme $10^10$) les coefficients pr�dit tendent vers 0, � l'inverse pour des valeurs faible et se rapprochant de 0 (comme $10^{-2}$) l'estimateur de Ridge tend vers l'EMCO.
# Ces graphiques repr�sentent les trajectoires des coordonn�s de l'estimateur en fonction de la norme L1 ou L2 de l'estimateur.





############################
###      Question 3      ###
############################
set.seed(314)

train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 
var = c("PAR_TC", "PAR_SC", "PAR_SC_V","PAR_ASC_V" , "PAR_ASE_M", "PAR_ASE_MV")
X_subset = music[var] 
X_subset$PAR_SC_V=log(X_subset$PAR_SC_V)
X_subset$PAR_ASC_V=log(X_subset$PAR_ASC_V)
# X_subset = music[-p]
# X_subset = X_subset[-148:-167] #j'enl�ve quand meme les r�p�titions 
Y = 1*(music$GENRE == 'Classical')
X_train = X_subset[train,]
Y_train = Y[train]
X_test = X_subset[!train,]
Y_test = Y[!train]


x=as.matrix(X_train)
y=as.matrix(Y_train)
cv.ridge=cv.glmnet(x,y,alpha=0,nfolds=10,family="binomial")   
plot(cv.ridge)

# estimation de l'erreur sur l'�chantillon de test
bestlambda=cv.ridge$lambda.min
ridge.pred=predict(cv.ridge,s=bestlambda,newx=as.matrix(X_test))

mean((ridge.pred-as.matrix(Y_test))^2)         # EQM: 0.607337             

#Classifieur de base
classification=ifelse( ridge.pred>.5,1,0) 
sum(classification==Y_test)/length(Y_test)*100 #61.10612



predict(ridge.fit,s=bestlambda,type="coefficients")  # aucun n'est nul on ne selectionne pas de var avec ridge


############################
###      Question 4      ###
############################

set.seed(4658)

train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 
X_subset = music[-p] 
Y = 1*(music$GENRE == 'Classical')
X_train = X_subset[train,]
Y_train = Y[train]
X_test = X_subset[!train,]
Y_test = Y[!train]


x=as.matrix(X_train)
y=as.matrix(Y_train)
cv.ridge=cv.glmnet(x,y,alpha=0,nfolds=10,family="binomial")   
plot(cv.ridge)

# estimation de l'erreur sur l'�chantillon de test
bestlambda=cv.ridge$lambda.min
log(bestlambda)
coef(cv.ridge)
ridge.pred=predict(cv.ridge,s=bestlambda,newx=as.matrix(X_test))

mean((ridge.pred-as.matrix(Y_test))^2)         # EQM: 5.695797             

#Classifieur de base
classification=ifelse( ridge.pred>.5,1,0) 
sum(classification==Y_test)/length(Y_test)*100 #88.89388 

#ALORS LA! c'est un peu l'echec mdr, car on a 89% de bonne classification sur l'ensemble de test (performence de g�n�ralisation)
# avec toutes les variabla contre 61% avec seulement les variables "importantes".
# on voit que l'EQM est plus grande mais c'est pas ab�rant � mes yeux car on est dans un espace � plus grande dimention, l'hyperplan est necessairement pas optimal pour chaque variables

predict(ridge.fit,s=bestlambda,type="coefficients")  # aucun n'est nul on ne selectionne pas de var avec ridge
