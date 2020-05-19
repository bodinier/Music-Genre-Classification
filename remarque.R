load("input/music.RData")
n = nrow(music)
set.seed(103)
train=sample(c(TRUE, FALSE), n, rep=TRUE, prob=c(2/3, 1/3)) 


library(ROCR) 
library(ggplot2)

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

#############################################################################


ModT.var = c("PAR_SFM_M", "PAR_SC", "PAR_ASE1", "PAR_ASE2", "PAR_ASE3", "PAR_ASE4","PAR_ASE5","PAR_ASE6","PAR_ASE7","PAR_ASE8","PAR_ASE24", "PAR_ASE30","PAR_ASE23", "PAR_ASC", "PAR_ASC_V", "PAR_SFM_MV", "PAR_ASE_M", "PAR_ASE_MV", "PAR_PEAK_RMS_TOT", "PAR_ASS_V", "PAR_THR_3RMS_TOT", "PAR_THR_2RMS_TOT", "PAR_THR_1RMS_TOT", "PAR_SC_V", "PAR_SFMV16", "PAR_SFMV15")

X_subset = music[ModT.var] 

X_subset$PAR_SC_V=log(X_subset$PAR_SC_V)
X_subset$PAR_ASC_V=log(X_subset$PAR_ASC_V)


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

