  getwd()
  load("input/music.RData")
  library(ROCR) 
  
  n = nrow(music)
  p = ncol(music)
  set.seed(103)
  train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 
  
  #########################
  #########################
  ###### Question 3 #######   #Elaboration des modèles
  #########################
  #########################
  
  ###################
  ###### Mod0 #######
  ###################
  
  # Sélection des variables (données dans l'énoncé ici )
  Mod0.var = c("PAR_TC", "PAR_SC", "PAR_SC_V", "PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV")
  X_subset = music[Mod0.var] 
  
  # Préparation des données (train, test)
  
  Y = music$GENRE == 'Classical'
  X_train = X_subset[train,]
  Y_train = Y[train]
  X_test = X_subset[!train,]
  Y_test = Y[!train]

  #Apprentissage du modèle
  Mod0 = glm( Y_train ~ ., family=binomial, data=X_train ) 
  summary(Mod0) 
  
  # Evaluation :
  genres_predicted_proba = predict(Mod0, newdata = X_test, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  mod0.res.test = data.frame(yt=Y_test, pred = genres_predicted)
  
  genres_predicted_proba = predict(Mod0, newdata = X_train, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  mod0.res.train = data.frame(yt=Y_train, pred = genres_predicted)
  
  mod0.accu = c(train = sum( (mod0.res.train$yt == mod0.res.train$pred) / sum(Y_train+ !Y_train) )*100 , test= sum( (mod0.res.test$yt == mod0.res.test$pred) / sum(Y_test+ !Y_test) )*100)
  
  pred0.train = prediction(predict(Mod0, newdata = X_train, type = 'response'), Y_train)
  pred0.test = prediction(predict(Mod0, newdata = X_test, type = 'response'), Y_test)
  
  
  perf0.train = performance(pred0.train, measure = 'tpr', x.measure = 'fpr')
  perf0.test = performance(pred0.test, measure = 'tpr', x.measure = 'fpr')
  
  perf0.test.auc = performance(pred0.test, "auc")
  auc0 = round(perf0.test.auc@y.values[[1]], 3)
  
  
  ###################
  ###### ModT #######
  ###################
  
  # Méthode de sélection : selection des variables les plus contributives aux axes principaux donnés par une ACP
  ModT.var = c("PAR_SFM_M", "PAR_SC", "PAR_ASE1", "PAR_ASE2", "PAR_ASE3", "PAR_ASE4","PAR_ASE5","PAR_ASE6","PAR_ASE7","PAR_ASE8","PAR_ASE24", "PAR_ASE30","PAR_ASE23", "PAR_ASC", "PAR_ASC_V", "PAR_SFM_MV", "PAR_ASE_M", "PAR_ASE_MV", "PAR_PEAK_RMS_TOT", "PAR_ASS_V", "PAR_THR_3RMS_TOT", "PAR_THR_2RMS_TOT", "PAR_THR_1RMS_TOT", "PAR_SC_V", "PAR_SFMV16", "PAR_SFMV15")
  
  X_subset = music[ModT.var] 
  Y = music$GENRE == 'Classical'
  X_train = X_subset[train,]
  Y_train = Y[train]
  X_test = X_subset[!train,]
  Y_test = Y[!train]
  
  ModT = glm( Y_train ~ ., family=binomial, data=X_train ) 
  summary(ModT) 
  
  # Evaluation :
  genres_predicted_proba = predict(ModT, newdata = X_test, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  modT.res.test = data.frame(yt=Y_test, pred = genres_predicted)
  
  genres_predicted_proba = predict(ModT, newdata = X_train, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  modT.res.train = data.frame(yt=Y_train, pred = genres_predicted)
  
  modT.accu = c(train = sum( (modT.res.train$yt == modT.res.train$pred) / sum(Y_train+ !Y_train) )*100 , test= sum( (modT.res.test$yt == modT.res.test$pred) / sum(Y_test+ !Y_test) )*100)
  
  predT.test = prediction(predict(ModT, newdata = X_test, type = 'response'), Y_test)
  perfT.test = performance(predT.test, measure = 'tpr', x.measure = 'fpr')
  
  predT.train = prediction(predict(ModT, newdata = X_train, type = 'response'), Y_train)
  perfT.train = performance(predT.train, measure = 'tpr', x.measure = 'fpr')
  
  perfT.train.auc = performance(predT.train, "auc")
  perfT.test.auc = performance(predT.test, "auc")
  
  aucT.train = round(perfT.train.auc@y.values[[1]], 3)
  aucT.test = round(perfT.test.auc@y.values[[1]], 3)
  
  plot(perfT.train)

  ###################
  ###### Mod1 #######
  ###################
  
  Mod1.var = c("PAR_SFM_M", "PAR_SC","PAR_ASE24","PAR_ASE1", "PAR_ASE2", "PAR_ASE3", "PAR_ASE4","PAR_ASE7", "PAR_ASE30","PAR_ASE23", "PAR_ASC", "PAR_SFM_MV", "PAR_ASE_M", "PAR_ASE_MV", "PAR_ASS_V", "PAR_THR_2RMS_TOT", "PAR_THR_1RMS_TOT", "PAR_SC_V", "PAR_SFMV15")
  X_subset = music[Mod1.var] 
  Y = music$GENRE == 'Classical'
  
  X_train = X_subset[train,]
  Y_train = Y[train]
  X_test = X_subset[!train,]
  Y_test = Y[!train]
  
  Mod1 = glm( Y_train ~ ., family=binomial, data=X_train ) 
  summary(Mod1) #? voir mais TC semble poser un peu PB ainsi que MV, mais on ne peut pas ? ce stade d?cider de l'?liminer comme ?a pour l'?liminer il faut utiliser des crit?res, ou appliquer un Fwd / Bwd ou mixte (Tp11)
  
  # Evaluation :
  genres_predicted_proba = predict(Mod1, newdata = X_test, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  mod1.res.test = data.frame(yt=Y_test, pred = genres_predicted)
  
  genres_predicted_proba = predict(Mod1, newdata = X_train, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  mod1.res.train = data.frame(yt=Y_train, pred = genres_predicted)
  
  mod1.accu = c(train = sum( (mod1.res.train$yt == mod1.res.train$pred) / sum(Y_train+ !Y_train) )*100 , test= sum( (mod1.res.test$yt == mod1.res.test$pred) / sum(Y_test+ !Y_test) )*100)
  
  pred1.test = prediction(predict(Mod1, newdata = X_test, type = 'response'), Y_test)
  perf1.test = performance(pred1.test, measure = 'tpr', x.measure = 'fpr')
  
  perf1.test.auc = performance(pred1.test, "auc")
  auc1 = round(perf1.test.auc@y.values[[1]], 3)
  
  plot(perf1.test)
  
  ###################
  ###### Mod2 #######
  ###################
  
  Mod2.var = c("PAR_SFM_M", "PAR_SC","PAR_ASE24", "PAR_ASE5", "PAR_ASE30","PAR_ASE23", "PAR_ASC", "PAR_ASC_V", "PAR_SFM_MV", "PAR_ASE_M", "PAR_ASE_MV", "PAR_PEAK_RMS_TOT", "PAR_ASS_V", "PAR_THR_3RMS_TOT", "PAR_THR_2RMS_TOT", "PAR_THR_1RMS_TOT", "PAR_SC_V", "PAR_SFMV16", "PAR_SFMV15")
  X_subset = music[Mod2.var] 
  Y = music$GENRE == 'Classical'
  
  X_train = X_subset[train,]
  Y_train = Y[train]
  X_test = X_subset[!train,]
  Y_test = Y[!train]
  
  Mod2 = glm( Y_train ~ ., family=binomial, data=X_train ) 
  summary(Mod2) #? voir mais TC semble poser un peu PB ainsi que MV, mais on ne peut pas ? ce stade d?cider de l'?liminer comme ?a pour l'?liminer il faut utiliser des crit?res, ou appliquer un Fwd / Bwd ou mixte (Tp11)
  
  # Evaluation :
  genres_predicted_proba = predict(Mod2, newdata = X_test, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  mod2.res.test = data.frame(yt=Y_test, pred = genres_predicted)
  
  genres_predicted_proba = predict(Mod2, newdata = X_train, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  mod2.res.train = data.frame(yt=Y_train, pred = genres_predicted)
  
  mod2.accu = c(train = sum( (mod2.res.train$yt == mod2.res.train$pred) / sum(Y_train+ !Y_train) )*100 , test= sum( (mod2.res.test$yt == mod2.res.test$pred) / sum(Y_test+ !Y_test) )*100)
  
  pred2.test = prediction(predict(Mod2, newdata = X_test, type = 'response'), Y_test)
  perf2.test = performance(pred2.test, measure = 'tpr', x.measure = 'fpr')
  
  perf2.test.auc = performance(pred2.test, "auc")
  auc2 = round(perf2.test.auc@y.values[[1]], 3)
  
  plot(perf2.test)
  
  ###################
  ###### ModAIC #####           #Attention prend des heures
  ###################
  
  # stepwise AIC
  library(MASS)
  library(dplyr)
  X_train = music[train,]
  X_train = X_train[,-p] # On retire genre du set 
  Y_train = Y[train]
  X_test = music[!train,]
  Y_test = Y[!train]
  
  # On se limite à Forward parce que bien bien plus rapide
  #ModAIC = glm(Y_train ~., data = X_train, family = binomial) %>% stepAIC(trace = TRUE, k=log(length(music$PAR_TC)), direction = "forward") #BIC
  ModAIC = glm(Y_train ~., data = X_train, family = binomial) %>% stepAIC(trace = TRUE, direction = "forward") #AIC
  summary(ModAIC) # On voit que la méthode a éliminé les variables que l'on avait choisies en Modèle ModT (SC, TC, SFM_MV) #(C) bingo
  plot(ModAIC)
  
  # Validation :
  genres_predicted_proba = predict(ModAIC, newdata = X_test, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  modAIC.res.test = data.frame(yt=Y_test, pred = genres_predicted)
  
  genres_predicted_proba = predict(ModAIC, newdata = X_train, type = 'response')
  genres_predicted = genres_predicted_proba >= 0.5
  modAIC.res.train = data.frame(yt=Y_train, pred = genres_predicted)
  
  modAIC.accu = c(train = sum( (modAIC.res.train$yt == modAIC.res.train$pred) / sum(Y_train+ !Y_train) )*100 , test= sum( (modAIC.res.test$yt == modAIC.res.test$pred) / sum(Y_test+ !Y_test) )*100)
  
  predAIC = prediction(predict(ModAIC, newdata = X_test, type='response'), Y_test)
  perfAIC = performance(predAIC, measure = 'tpr', x.measure = 'fpr')
  
  perfAIC.auc = performance(predAIC, "auc")
  aucAIC = round(perfAIC.auc@y.values[[1]], 3)
  
  plot(perfAIC)
  
  
  #########################
  ###### Question 4 #######   #Visualisation
  #########################
  
  
  ###################
  # Le ModT en détail :
  ###################
  
  predT.train = prediction(predict(ModT, newdata = X_train, type = 'response'), Y_train)
  predT.test = prediction(predict(ModT, newdata = X_test, type = 'response'), Y_test)
  
  perfT.train = performance(predT.train, measure = 'tpr', x.measure = 'fpr')
  perfT.test = performance(predT.test, measure = 'tpr', x.measure = 'fpr')
  
  plot(perfT.train)
  plot(perfT.test)
  
  
  res = data.frame(mod0 = mod0.res, modT = modT.res$pred, mod1 = mod1.res$pred, mod2 = mod2.res$pred)
  res
  
  ###################
  # Multi ROC :
  ###################
  
  plot(perf0.test, col=1)
  plot(perfT.test, col=2, add=TRUE)
  plot(perf1.test, col=3, add=TRUE)
  plot(perf2.test, col=4, add=TRUE)
  plot(perfAIC, col=5, add=TRUE)  
  
  legend("bottomright", legend = c(paste('Mod0',auc0), paste('ModT', aucT.test), paste('Mod1', auc1), paste('Mod2', auc2), paste('ModAIC', aucAIC)), col=1:5, lty=1)
  
  
  ###################
  # Erreurs : 
  ###################
  par(mfrow=c(2,2))
  bp = barplot(mod0.accu, ylim=c(70, 90), width = 0.5, main='Mod0', ylab='accuracy')
  text(bp, 80, round(mod0.accu, 2))
  bp = barplot(modT.accu, ylim=c(70, 90), width = 0.5, main='ModT', ylab='accuracy')
  text(bp, 85, round(modT.accu, 2))
  bp = barplot(mod1.accu, ylim=c(70, 90), width = 0.5, main='Mod1', ylab='accuracy')
  text(bp, 85, round(mod1.accu, 2))
  bp = barplot(mod2.accu, ylim=c(70, 90), width = 0.5, main='Mod2', ylab='accuracy')
  text(bp, 85, round(mod2.accu, 2))
  #bp = barplot(modAIC.accu, ylim=c(70, 90), width = 0.5, main='ModAIC', ylab='accuracy')
  #text(bp, 85, round(modAIC.accu, 2))
  
  