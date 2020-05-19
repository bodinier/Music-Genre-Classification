
############################
### Chargement ? adapter ###
############################
setwd("C:/Users/csoub/OneDrive/Bureau/DM_STA203/input")
#getwd()
load("music.RData")

p=ncol(music)
n=nrow(music)

############################
###      Question 1      ###
############################

# La methode des KNN (k-nearest neighbor) est une methode de classification supervis?e. Elle consiste ? classer un nouvelle individu d'un espace de dimension p (nombre de param?tre) 
# en fonction de la classe de ses k plus proches voisins (au sens de la distance euclidienne) faisant partie de l'ensemble d'entrainement. 
# Autrement dit ? partir d'un jeu de donn?es labellis?, on attribut un label ? un jeu inconnu en fonction de sa position dans l'espace. 
# Dans le cas ou k=1 on donne ? une nouvelle donn?e le label de la donn?e la plus proche de l'ensemble d'entrainement


############################
###      Question 2      ###
############################
library(class)
library(ROCR)
library(ggplot2)

set.seed(103)
train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 


######################################################################
############## avec les variables du mod?le T
######################################################################
var = c("PAR_SFM_M", "PAR_SC", "PAR_ASE1", "PAR_ASE2", "PAR_ASE3", "PAR_ASE4","PAR_ASE5","PAR_ASE6","PAR_ASE7","PAR_ASE8","PAR_ASE24", "PAR_ASE30","PAR_ASE23", "PAR_ASC", "PAR_ASC_V", "PAR_SFM_MV", "PAR_ASE_M", "PAR_ASE_MV", "PAR_PEAK_RMS_TOT", "PAR_ASS_V", "PAR_THR_3RMS_TOT", "PAR_THR_2RMS_TOT", "PAR_THR_1RMS_TOT", "PAR_SC_V", "PAR_SFMV16", "PAR_SFMV15")
X_subset = music[var] 
X_subset_normalized = scale(X_subset)
X_subset_normalized$PAR_SC_V=log(X_subset_normalized$PAR_SC_V)
X_subset_normalized$PAR_ASC_V=log(X_subset_normalized$PAR_ASC_V)

Y = music$GENRE
Y = music$GENRE == 'Classical'
X_train = X_subset_normalized[train,]
Y_train = Y[train]
X_test = X_subset_normalized[!train,]
Y_test = Y[!train]

res.knn=knn(X_train,X_test,cl=Y_train,k=1)
print(paste0("Accuracy=",sum(res.knn==Y_test)/length(Y_test)*100),quote=FALSE)    

##Approfondissement
m=length(Y_test)
VP=length(which(res.knn==Y_test & res.knn==TRUE))
VN=length(which(res.knn==Y_test & res.knn==FALSE))
FP=length(which(res.knn==TRUE & Y_test==FALSE))
FN=length(which(res.knn==FALSE & Y_test==TRUE)) 
tc=data.frame(rbind(c(VP,FP),c(FN,VN))) 


names(tc)=c("V","F")
tc
## On peut maintenant s'amuser ? comparer les tests en termes de performances mais aussi en terme de qualit?: 
specificite=VN/(FP+VN)*100
sensibilite=VP/(VP+FN)*100
print(paste0("sensibilit?=",sensibilite),quote = FALSE)
print(paste0("sp?cificit?=",specificite),quote = FALSE)

######################################################################
############## ROC
######################################################################

xaxis=seq(1:100)
yaxis=c()
sensi=c()
minus_spe=c()
for (i in xaxis) {
  res.knn=knn(X_train,X_test,cl=Y_train,k=i)
  yaxis=c(yaxis,sum(res.knn==Y_test)/length(Y_test)*100)     
  ##Approfondissement
  m=length(Y_test)
  VP=length(which(res.knn==Y_test & res.knn==TRUE))
  VN=length(which(res.knn==Y_test & res.knn==FALSE))
  FP=length(which(res.knn==TRUE & Y_test==FALSE))
  FN=length(which(res.knn==FALSE & Y_test==TRUE)) 
  tc=data.frame(rbind(c(VP,FP),c(FN,VN))) 
  
  
  names(tc)=c("V","F")
  tc
  
  ## On peut maintenant s'amuser ? comparer les tests en termes de performances mais aussi en terme de qualit?: 
  minus_spe=c(minus_spe,(VN/(FP+VN)))
  sensi=c(sensi,VP/(VP+FN))
}

plot(minus_spe,sensi,col="blue",pch=2,title("ROC construit pour k allant de 1 ? 100"),xlim=c(0,1),ylim=c(0,1),xlab = "1-sp?cificit?",ylab = "sensibilit?")


######################################################################
############## meilleur k
######################################################################
set.seed(103)
xaxis=seq(1:5)


var = c("PAR_SFM_M", "PAR_SC", "PAR_ASE1", "PAR_ASE2", "PAR_ASE3", "PAR_ASE4","PAR_ASE5","PAR_ASE6","PAR_ASE7","PAR_ASE8","PAR_ASE24", "PAR_ASE30","PAR_ASE23", "PAR_ASC", "PAR_ASC_V", "PAR_SFM_MV", "PAR_ASE_M", "PAR_ASE_MV", "PAR_PEAK_RMS_TOT", "PAR_ASS_V", "PAR_THR_3RMS_TOT", "PAR_THR_2RMS_TOT", "PAR_THR_1RMS_TOT", "PAR_SC_V", "PAR_SFMV16", "PAR_SFMV15")
X_subset = music[var] 
X_subset$PAR_SC_V=log(X_subset$PAR_SC_V)
X_subset$PAR_ASC_V=log(X_subset$PAR_ASC_V)

Y = music$GENRE
Y = music$GENRE == 'Classical'
X_train = X_subset[train,]
Y_train = Y[train]
X_test = X_subset[!train,]
Y_test = Y[!train]

accura=c()
for (i in xaxis) {
  res.knn=knn(X_train,X_test,cl=Y_train,k=i)
  yaxis=c(yaxis,sum(res.knn==Y_test)/length(Y_test)*100)     
  ##Approfondissement
  m=length(Y_test)
  VP=length(which(res.knn==Y_test & res.knn==TRUE))
  VN=length(which(res.knn==Y_test & res.knn==FALSE))
  FP=length(which(res.knn==TRUE & Y_test==FALSE))
  FN=length(which(res.knn==FALSE & Y_test==TRUE)) 
  tc=data.frame(rbind(c(VP,FP),c(FN,VN))) 
  
  
  names(tc)=c("V","F")
  tc
  
  accura=c(accura,sum(res.knn==Y_test)/length(Y_test)*100)
}
X_subset = music[-p]
X_subset = X_subset[-148:-167] #j'enl?ve quand meme les r?p?titions 
train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 
X_subset$PAR_SC_V=log(X_subset$PAR_SC_V)
X_subset$PAR_ASC_V=log(X_subset$PAR_ASC_V)

Y = music$GENRE == 'Classical'
X_train = X_subset[train,]
Y_train = Y[train]
X_test = X_subset[!train,]
Y_test = Y[!train]
accura_tot=c()
for (i in xaxis) {
  res.knn=knn(X_train,X_test,cl=Y_train,k=i)
  yaxis=c(yaxis,sum(res.knn==Y_test)/length(Y_test)*100)     
  ##Approfondissement
  m=length(Y_test)
  VP=length(which(res.knn==Y_test & res.knn==TRUE))
  VN=length(which(res.knn==Y_test & res.knn==FALSE))
  FP=length(which(res.knn==TRUE & Y_test==FALSE))
  FN=length(which(res.knn==FALSE & Y_test==TRUE)) 
  tc=data.frame(rbind(c(VP,FP),c(FN,VN))) 
  
  
  names(tc)=c("V","F")
  tc
  
  accura_tot=c(accura_tot,sum(res.knn==Y_test)/length(Y_test)*100)
}



plot(c(xaxis,xaxis),c(accura,accura_tot),col=2:3,pch=2:3,title("taux de bonne r?ponse (accuracy) en fonction de k"),xlab = "k",ylab = "% de bonne r?ponse (accuracy)")
k_opt=which.max(accura)
opt=accura[k_opt]
segments(k_opt,0,k_opt,opt,lty=3,lwd=2,col=2)        
segments(k_opt,opt,-10,opt,lty=3,lwd=2,col=2)
opt=accura_tot[k_opt]
segments(k_opt,0,k_opt,opt,lty=3,lwd=2,col=2)        
segments(k_opt,opt,-10,opt,lty=3,lwd=2,col=2)
######################################################################
############## avec toutes les variables 
######################################################################
X_subset = music[-p]
X_subset = X_subset[-148:-167] #j'enl?ve quand meme les r?p?titions 
train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 
X_subset$PAR_SC_V=log(X_subset$PAR_SC_V)
X_subset$PAR_ASC_V=log(X_subset$PAR_ASC_V)

Y = music$GENRE == 'Classical'
X_train = X_subset[train,]
Y_train = Y[train]
X_test = X_subset[!train,]
Y_test = Y[!train]
res.knn=knn(X_train,X_test,cl=Y_train,k=1)
sum(res.knn==Y_test)/length(Y_test)*100    #63.64, avec plus de variables et le m?me jeu de donn?es on est 1% meilleurs que le mod?le r?duit ? quelques variables. Le temps de calcul vaut-il le coup ?


###########################################################################
## Approfondissement avec un petit train/test/validation

train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(0.6, 0.4) )
test=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(3/4, 1/4) )

X_train = X_subset[train,]
Y_train = Y[train]
X_test_val = X_subset[!train,]
Y_test_val = Y[!train]

X_test = X_subset[test,]
Y_test = Y[test]

X_valid = X_subset[!test,]
Y_valid = Y[!test]

res.knn=knn(X_train,X_test,cl=Y_train,k=1)
sum(res.knn==Y_test)/length(Y_test)*100      #[1] 86.20% quali ! 
# C'est pas bizarre qu'on ait trouvé 62.4% tout à l'heure, alors qu'on avait aussi k=1 ?

xaxis=seq(1:20)
yaxis=c()
for (k in xaxis) {
  res.knn=knn(X_train,X_test,cl=Y_train,k=k)
  yaxis=c(yaxis,sum(res.knn==Y_test)/length(Y_test)*100)
}

plot(xaxis,yaxis)
abline(v=1,col="red") #on voit que k =1 reste meilleur

res.knn=knn(X_train,X_valid,cl=Y_train,k=1)
perf=sum(res.knn==Y_valid)/length(Y_valid)*100 ## performance de validation: 85% ce qui correspond ? un erreur de g?n?ralisation de 15%
points(1,perf,col="blue",pch=4) 

############################
###      Question 3      ###
############################