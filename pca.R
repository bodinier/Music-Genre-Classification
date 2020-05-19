#########################
#####   PARTIE 1:   #####
#########################
  getwd()
  load("input/music.RData")

#########################
######  PCA #######
#########################
  
  # PCA :
  n=nrow(music)
  X = scale(music[,-p],center=TRUE,scale=TRUE)/sqrt((n-1)/n) # Centre et réduit les variables
  C = cor(X)  
  
  valp = eigen(C)$values
  valp
  sum(valp)
  p_inertie = valp/sum(valp)*100 # pourcentage d'inertie
  names(p_inertie) = names(X)
  p_inertie
  
  barplot(p_inertie)
  100/sum(valp)
  abline(h=100/7)
  
  library(FactoMineR)
  par(mfrow=c(1,1))
  res = PCA(music[,-p])
  round(res$eig,4) # variance de chacun des 7 axes
  barplot(res$eig[,2],main="% inertie",names=paste("Dim",1:nrow(res$eig)), ylab="% inertie", xlab='dim')
  abline(h=100/sum(res$eig[,1]),lty=2,col='blue' )  
  abline(h = 2.5, col='red', lty=3)
  abline(v=42, col = 'blue', lty=3)
  abline(v=8, col = 'red', lty=3)
  legend("topright", legend = c('règle du coude', '100/sum(vp)'), col = c('red', 'blue'), lty =1:2)
  
  V=res$var
  plot(res,choix="var")

  V$cos2
  V$cor^2
  
  # corr?lation
  V$cor
  cbind(G1,G2)
  
  #contribution ? l'axe
  V$contrib
  V$cos2[,1]/sum(V$cos2[,1])
  
  #visualisation
  par(mfrow=c(1,2))
  plot(res,axes = c(1,2), choix = "var")
  # axe 1 = effet taille
  # axe 2 = opposition asymetrie - compacit?
  # axe 3 = compacit? + asym?trie  
  plot(res,axes = c(2,3), choix = "var")  
  