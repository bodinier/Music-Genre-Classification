#########################
#####   PARTIE 1:   #####
#########################

# Mise en place de l'environnement 
  #?a prends pas mal de temps, c'est normal music.txt est lourd, je le convertis en Rdata 
  #pour travailler directement avec ce Rdata dans le git et ne plus avoir ? faire cela
  rm(list=objects()) ; graphics.off()
  # music=read.table(url("https://raw.githubusercontent.com/Coohrentiin/DM_STA203/master/input/Music.txt"))
  music=read.table("Music.txt",header = TRUE,sep=";") #mdr d?j? j'ai pass? 30 min pour mettre les donn?es au bon format ^^'
  save(music,file="music.RData")
  rm(music)

#R?cup?ration des donn?es en local:
  #?a prends quelques seconde aussi
  
  load("music.RData")

#########################
###### Question 1 #######
#########################
  
  ## Analyse descriptive
    dim(music) # [1] 6447  192, donc 192 variables et 6447 obsevations dont 191 variables quantitatives et une seule qualitative le genre (à retrouver)
    p=ncol(music)
    n = nrow(music)
    summary(music) #d?but d'analyne univari? des 192 variables, pas top 
    
    # On remarque que toutes les variables sont quantitative ? l'exeception de la variable genre qui prends deux valeurs possibles:
    summary(music$GENRE)
    # Classical      Jazz 
    # 3444      3003 
    #La r?partition des donn?es entre jazz est clasique est assez ?quitables
    3444/6447*100 #[1] 53.4202
    
    #On remarque ?galement que les 191 autres variables donc des ordres de grandeurs pouvant ?tre tr?s diff?rent.
    # ex: PAR_SC_V de l'ordre de 10^4, PAR_ASE25 de l'ordre de 10^-1 (comme beaucoup d'autres), PAR_ASEV30 10^-4 (comme pas mal d'autres aussi)
    
    plot(music[,-p]) #ne marche pas, beaucoup trop de variables bien evidement
    
    library(corrplot)
    corrplot(cor(music[,-p]),method = "circle") #fonctionne mais on ne voit pas grand chose
    corrplot(cor(music[,1:50]),method = "circle") # comme ce n'est pas tr?s lisible avec 191 variables regardons d?ja pour les 50 premi?res.
    corrplot(cor(music[,50:100]),method = "circle")
    # on remarque d?j? que autour de la diagonales les corellations sont assez forte ce qui se voit aussi avec le corr des 191 variables
    # on trouve ?galement un bon nombre de variales anticorr?l? .
    # le fait que des variables soit tr?s correl?s ou au contraire peu, am?ne ? se demander si un reduction de dimension avec par exemple une acp ne serait pas inutile
    

  ## Transformation Log
      # pourquoi faire un log ? 
      summary(music$PAR_SC_V)
      # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      # 605   22714   46309  103732   99464 5003700 
      # Grosse diff?rence entre le min et le max confirm? par la m?diane proche du min
      
      X=sort(music$PAR_SC_V) #je trie les donn?es en fonction de leur valeur
      plot(X) #on remarque que beaucoup de donn?es on de faibles valeurs et que quelques une ont des valeurs tr?s ?lev?
      
      boxplot(music[,1:10])
      boxplot(music$PAR_SC_V)
      # constat que l'on observe dans la diagramme moustave les outsider sont tres loins de la moyenne
      
      #une methode pour uniformiser l'echelle est de passer en logaritmique 
      plot(log(X)) # on a uniformis? la repartition
      boxplot(log(music$PAR_SC_V)) #la r?partition est d?ja plus correcte
      summary(log(music$PAR_SC_V))
      # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      # 6.405  10.031  10.743  10.758  11.508  15.426 
      # Beaucoup plus repr?sentatif

      
      
      summary(music$PAR_ASC_V)
      # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
      # 0.005926 0.165035 0.292860 0.426270 0.532465 4.998000 
      X=sort(music$PAR_ASC_V) 
      plot(X) #m?me probl?mes un peu moins marqu? cependant 
      boxplot(music$PAR_ASC_V)
      plot(log(X)) # on a uniformis? la repartition
      boxplot(log(music$PAR_ASC_V)) #la r?partition est d?ja plus correcte
      summary(log(music$PAR_ASC_V))
      # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      # -5.1285 -1.8016 -1.2281 -1.2256 -0.6302  1.6090 
  ## Variables 148 ? 167:
      # De 148 ? 147 on a PAR_MFCCVi avec i de 1 ? 20
      # Or si on regarde les 20 variables pr?c?dantes (128 ? 147) on remarque que les PAR_MFCCi sont tr?s corr?ll? au PAR_MFCCVi correspondant
      #Cette redondance de donn?e est inutile, il est donc pr?f?rable d'enlever les variables 148 ? 167
      corrplot(cor(music[,128:167]),method = "circle")
    
      
      
  ## Variables tr?s corr?l?s 
      corrplot(cor(music[,20:50]),method = "circle")
      corrplot(cor(music[,36:37]),method = "circle")
      cor(music[,36:37])
      cor(music[,31:40])
      #La correlation entre les variables PAR_ASE33 et PAR_ASE34 sont corr?ll?s ? plus de 99,9% 
      #de plus la correlation de ces deux variables avec les autres semblent ?tre la m?me il n'y a donc pas d'interet de garder les deux pour l'?tude
      # ASE = 'audio spectrum enveloppe'. Les variables ASE (au nombre de 34) sont les mesures d'enveloppe )
      
      # Pour se faire une idée :
      s = seq(0, 34)
      jazz = which(music$GENRE == 'Jazz')
      classique = which(music$GENRE == 'Classical')
      par(mfrow=c(2,4))
      for (i in seq(1, 4) ) {
        plot(s, music[jazz[i+100], 4:38], main=paste("spectre n°",i, '(jazz)', sep=' '), xlab = "fréquence", ylab="amplitude (dB)")
      }
      for (i in seq(1, 4) ) {
        plot(s, music[classique[i+100], 4:38], main=paste("spectre n°",i+4, '(classical)', sep=' '), xlab = "fréquence", ylab="amplitude (dB)")
      }
      # les "ASE" représentent la forme de l'enveloppe spectrale. Il est donc normal que les variables soient similaires de proche en proche puisqu'il s'agit de bandes voisine
      # Ici la corrélation rend compte d'un phénomène physique : la continuité du spectre. Les variables corrélées disent donc la même chose
      # Intuitivement on pourrait avoir peut être deux familles de spectres, on pourrait imaginer séparer les genres par leur forme de spectre.
      # Malheureusement, on voit que ce n'est pas un très bon séparateur puisque il existe des morceaux qui ont un spectre similaire et un genre différent (cf. spectres 2 et 5)
      # C'est pourquoi on va pouvoir ces variables ne vont pas être selectionnées mais on va plutôt choisir des agrégats : (cf suite)
      
  ## Variables sp?ciales
      plot.new()
      par(mfrow=c(1,3))
      # Repr?sentation des variables sp?ciales les une en fonction des autres
      # On colore en fonrtion du style musical 
      X1 = cbind(music$PAR_ASE_M,music$PAR_ASE_MV)
      plot(X1, col=music$GENRE)
      title("ASE_M ~ ASE_MV")
      
      X2 = cbind(music$PAR_SFM_M,music$PAR_SFM_MV)
      plot(X2, col=music$GENRE)
      title("SFM_M ~ SFM_MV")
      
      X3 = cbind(music$PAR_TC, music$PAR_SC)
      plot(X3, col=music$GENRE)
      title("PAR_TC ~ PAR_SC")
      
      par(mfrow=c(1,1))
      X=cbind(music$PAR_ASE_M,music$PAR_ASE_MV,music$PAR_SFM_M,music$PAR_SFM_MV)
      plot(X, col=music$GENRE)
      title("ASE_M ~ ASE_MV")
      
      
      #Influence de TC, SC et SC_V
      par(mfrow=c(1,3))
      boxplot(music[, 1]~ music$GENRE, xlab='', ylab='répartition', main='TC') #TC
      
      boxplot(music[, 2]~music$GENRE, xlab='', ylab='répartition',main='SC') # SC
      
      #boxplot(music[, 3]~music$GENRE, xlab='', ylab='répartition') # pas représentatif : log transformation
      
      boxplot(log(music[, 3])~music$GENRE, xlab='', ylab='répartition', main='SC_V') # SC_V
      
      
      # influence des ASE
      par(mfrow=c(1,2))
      boxplot(music[, 38] ~ music$GENRE, main="ASE_M", ylab="répartition", xlab='') # ASE_M
      boxplot(music[, 73] ~ music$GENRE, main="ASE_V", ylab="répartition", xlab='') # ASE_V
      
      # influence des SFM
      par(mfrow=c(1,2))
      boxplot(music[, 102]~music$GENRE, main='SFM_M', ylab='répartition', xlab='') # SFM_M semble être un bon critère
      boxplot(music[, 127]~music$GENRE, main='SFM_V', ylab='répartition', xlab='') # SFM_V
      
      # Conclusion des boxplot : les aggrégats sont un bon levier de séparation puisqu'on observe des répartitions clairement distinctes (en termes de moyenne, quartiles, variance, étendue).
      
      par(mfrow=c(1,1))
      names(X)=c("par_ase_m","par_ase_mv","par_sfm_m","par_sfm_mv")
      corrplot(cor(X),method = "circle")
      cor(X)
      # 2 et 3 légèrement anti corrélées (-.33)=> logique puisque si un spectre a une forte variabilité (2 grand, donc des pics), il a peu de chances d'être 'flat' (3)
      # 2 et 4 légèrement corrélées (.55) => la remarque inverse qu'au desssus 

      
      
      
      # Bilan
      #-> on enl?ve les variables 148 ? 167
      #-> pour les ASE bonne question je ne sais pas trop comment enlever: 
      corrplot(cor(music[,3:38]),method = "circle") #n'apporte pas d'aide particuli?re
      # mais les plot que tu as fais peuvent apporter une info sur quoi garder
      # Comme je viens de le mettre juste après, je pense que les ASE individuelles n'apportent pas grand chose, mais leur aggrégats oui !
      
## Mod?le logistique
      # Cadre Classification binaire, donc c'est nice. Il repose sur l'hypothèse que ln( P(X/Y=+)/P(X/Y=-) ) = a0 + sum(BiXi)
      # Ici + = classique et - = Jazz par exemple
      # à vérifier : avoir N_echantillon >> N_variables, ici facteur 300 donc c'est bon
      # pour la robustesse attentions aux variables sensibles    

      # 2) Modèle
      
      var = c("PAR_TC", "PAR_SC", "PAR_SC_V", "PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV")
      X_subset = music[var] 
      Y = music$GENRE
      summary(Y)
      Y = music$GENRE == 'Classical'
      summary(Y)
      # Pour la suite, on a besoin de transformer le genre en binaire : 0 = Jazz, 1 = classique (n'y voyez aucun jugement de valeur)

#########################
###### Question 2 #######
#########################
      set.seed(103)
      train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) #(A) Pour la Validation croisé #(C) non c'est juste pour un apprentissage normal ? mes yeux pour un VC on ferais bien plus de classes
                                                                  # (A) oui tu as raison, c'est juste un apprentissage avec un seul test
      
      X_train = X_subset[train,]
      Y_train = Y[train]
      X_test = X_subset[!train,]
      Y_test = Y[!train]
      
      dim(X_train) # = NULL bizarre
      dim(X_test)
      dim(X_train)[1] + dim(X_test)[1] # = au nombre de lignes : OK !
      summary(Y_train)
      summary(Y_test)
#########################
###### Question 3 #######
#########################
      # Estimation du mod?le
      
      ## Mod0:
      
      # (X_train$PAR_TC + X_train$PAR_SC + X_train$PAR_SC_V + exp(X_train$PAR_ASE_M) + X_train$PAR_ASE_MV + log(X_train$PAR_SFM_M) + log(X_train$PAR_SFM_MV))
      X_train$PAR_SFM_M = log( X_train$PAR_SFM_M)
      X_train$PAR_SFM_MV = log( X_train$PAR_SFM_MV)
      #Mod0 = glm( Y_train ~ PAR_TC + PAR_SC + PAR_SC_V + PAR_ASE_M + PAR_ASE_MV + log(PAR_SFM_M) + log(PAR_SFM_MV), family=binomial(logit), data=X_train ) #(C): du coup l? si il faudrait dans l'ideal utiliser ton Xtrain et pas ton X entier pour pouvoir calculer derriere l'erreur de pr?vision # (A) oui exactement ! et je me demande 
      Mod0 = glm( Y_train ~ ., family=binomial(logit), data=X_train )
      Mod0$coefficients
      
      summary(Mod0) #? voir mais TC semble poser un peu PB ainsi que MV, mais on ne peut pas ? ce stade d?cider de l'?liminer comme ?a pour l'?liminer il faut utiliser des crit?res, ou appliquer un Fwd / Bwd ou mixte (Tp11)
      library(questionr)
      odds.ratio(Mod0) # effectivemet on a de gros problèmes avec 
      
      # Affichage :
      f=function(x,a,b)    1/(1+exp(-a-b*x))  
      par(mfrow=c(1,1))
      plot(log(X_train$PAR_SFM_M), Y_train, xlab='PAR_SFM_M', ylab='classe')
      curve(f(x,Mod0$coef[1], Mod0$coef[2]),from=-10, to=10,col=2,add=TRUE, main='pred sur SFM_M seulement')
      legend("bottomleft", c('classes observées', 'prediction'), col=c(1,2),lty=0:1,pch=c(1,-1))
      
      # Evaluation :
      genres_predicted_proba = predict(Mod0, newdata = X_test, type = 'response')
      genres_predicted = genres_predicted_proba >= 0.5
      res = data.frame(yt=Y_test, pred = genres_predicted)
      id_err = (res$yt != res$pred)
      t_err = sum(id_err)/sum(Y_test)
      t_err
      pred0 = prediction(genres_predicted_proba, Y_test)
      perf0 = performance(pred0, measure = 'tpr', x.measure = 'fpr')
      plot(perf0)
      
      
      ## ModT:
      #XT = cbind( #todo )
      #YT = music$GENRE
      #XT_train = XT[train]
      #YT_train = YT[train]
      #XT_test = XT[!train]
      #YT_test = YT[!train]
      #ModT = glm(cbind(YT_train=='Classical')~XT_train, family='binomial')
      
      # Au vu des tests de significativité du Mod0, et en accord avec les intuitions des visualisations des digrammes à moustache, 
      # On aurait envie de se débarasser des variables PAR_TC et PAR_SC 
      ModT = glm( Y_train ~ PAR_SC_V + PAR_ASE_MV + log(PAR_SFM_M) + log(PAR_SFM_MV), family=binomial(logit), data = X_train)
      #(C): le log c'est pour SC_V et ASC_V pas SFM je pense
      ModT$coefficients
      
      summary(ModT) 
      library(questionr)
      odds.ratio(ModT)
      
      # Affichage :
      f=function(x,a,b)    1/(1+exp(-a-b*x))  
      par(mfrow=c(1,1))
      plot(log(X_train$PAR_SFM_M), Y_train, xlab='PAR_SFM_M', ylab='classe')
      curve(f(x,ModT$coef[1], ModT$coef[2]),from=-10, to=10,col=2,add=TRUE, main='pred sur SFM_M seulement')
      legend("bottomleft", c('classes observées', 'prediction'), col=c(1,2),lty=0:1,pch=c(1,-1))
      
      # Validation :
      genres_predicted_proba = predict(ModT, newdata = X_test, type = 'response')
      genres_predicted = genres_predicted_proba >= 0.5
      head(genres_predicted_proba)
      head(genres_predicted)
      res = data.frame(yt=Y_test, pred = genres_predicted)
      id_err = (res$yt != res$pred)
      t_err = sum(id_err)/sum(Y_test)
      t_err
      predT = prediction(predict(ModT, newdata = X_test, type='response'), Y_test)
      perfT = performance(predT, measure = 'tpr', x.measure = 'fpr')
      plot(perfT)
      
      ## Mod1:
      #Selection des variables significatives à 5% (on enlève que PAR_TC, PARC_SC et PAR_SFM_MV)
      
      ## Mod2:
      #Selection des variables significatives à 20% (on enlève que PAR_TC et PAR_SFM_MV)
      
      
      ## ModAIC:
      # stepwise AIC
      library(MASS)
      library(dplyr)
      ModAIC = glm(Y_train ~., data = X_train, family = binomial) %>% stepAIC(trace = TRUE)
      summary(ModAIC) # On voit que la méthode a éliminé les variables que l'on avait choisies en Modèle ModT (SC, TC, SFM_MV) #(C) bingo
      plot(ModAIC)
      
      # Validation :
      genres_predicted_proba = predict(ModAIC, newdata = X_test, type = 'response')
      genres_predicted = genres_predicted_proba >= 0.5
      res = data.frame(yt=Y_test, pred = genres_predicted)
      id_err = (res$yt != res$pred)
      t_err = sum(id_err)/sum(Y_test)
      t_err
      predAIC = prediction(predict(ModAIC, newdata = X_test, type='response'), Y_test)
      perfAIC = performance(predAIC, measure = 'tpr', x.measure = 'fpr')
      plot(perfAIC)
      
      
#########################
###### Question 4 #######
#########################
  
  
  
#########################
###### Question 5 #######
#########################
  
  
  