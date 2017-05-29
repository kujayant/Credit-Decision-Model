library(h2o)

h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="8g", nthreads=4)

htrain_ovr_smpl<-as.h2o(new_train)
htrain<-as.h2o(train_set)
htest<-as.h2o(test_set)
htrain_col<-htrain[-c(1,29,30)]
htrain_col<-h2o.cbind(htrain_col,htrain[30])
htest_col<-htest[-c(1,29,30)]
htest_col<-h2o.cbind(htest_col,htest[30])
htrain_ovr<-h2o.cbind(htrain_ovr_smpl[-c(1,29,30)],htrain_ovr_smpl[30])

red_train_frame$ylab<-as.factor(red_train_frame$ylab)
red_test_frame$ylab_test<-as.factor(red_test_frame$ylab_test)
htrain_red<-as.h2o(red_train_frame)
htest_red<-as.h2o(red_test_frame)


model_dl <- h2o.deeplearning(x = 1:98,  # column numbers for predictors
                          y = 99,   # column number for label
                          training_frame = htrain_red,
                          model_id="dl_h2o",
                          activation = "TanhWithDropout",
                          balance_classes = TRUE,
                          hidden = c(100, 100, 100),  
                          epochs = 50,
                          seed=1234,
                          nfolds=10,
                          fold_assignment = "Modulo",
                          keep_cross_validation_fold_assignment=TRUE,
                          keep_cross_validation_predictions=TRUE)

yhat_train <- h2o.predict(model_dl, htrain_red[-99])$predict
yhat_train <- as.factor(as.matrix(yhat_train))
yhat_test <- h2o.predict(model_dl, htest_red[-99])$predict
yhat_test <- as.factor(as.matrix(yhat_test))
table(yhat_test,as.factor(as.matrix(htest_red[99])))
table(yhat_train,as.factor(as.matrix(htrain_red[99])))


h2o.giniCoef(h2o.performance(model_dl, newdata = htest_red))
##[1] 0.2256404
h2o.giniCoef(h2o.performance(model_dl, newdata = htrain_red))
##[1] 0.2549808

##Neural Network has been an unstable model. Definitely not recommended

###H2o GBM###

model_gbm <- h2o.gbm(x = 1:819,
                  y = 820,
                  training_frame = htrain_col,
                  model_id="gb_h2o",
                  distribution = "bernoulli",
                  ntrees = 10,
                  max_depth = 5,
                  min_rows = 2,
                  learn_rate = 0.035,
                  nfolds = 10,
                  fold_assignment = "Modulo",
                  balance_classes=TRUE,
                  keep_cross_validation_predictions = TRUE,
                  seed = 1234)

y_train_gb <- h2o.predict(model_gbm, htrain_col[-820])$predict
y_train_gb <- as.factor(as.matrix(y_train_gb))
y_test_gb <- h2o.predict(model_gbm, htest_col[-820])$predict
y_test_gb <- as.factor(as.matrix(y_test_gb))
table(y_test_gb,as.factor(as.matrix(htest_col[820])))
table(y_train_gb,as.factor(as.matrix(htrain_col[820])))

h2o.giniCoef(h2o.performance(model_gbm, newdata = htest_col))
##[1] 0.2749749
h2o.giniCoef(h2o.performance(model_gbm, newdata = htrain_col))
##[1] 0.4920463

####H2o RandomForest###
model_rf <- h2o.randomForest(x = 1:819,
                          y = 820,
                          training_frame = htrain_col,
                          model_id = "rf_h2o",
                          ntrees = 600,
                          nfolds = 10,
                          ignore_const_cols = TRUE,
                          max_depth = 10,
                          mtries=9,
                          balance_classes=TRUE,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1234)

y_train_rf <- h2o.predict(model_rf, htrain_col[-820])$predict
y_train_rf <- as.factor(as.matrix(y_train_rf))
y_test_rf <- h2o.predict(model_rf, htest_col[-820])$predict
y_test_rf <- as.factor(as.matrix(y_test_rf))
table(y_test_rf,as.factor(as.matrix(htest_col[820])))
table(y_train_rf,as.factor(as.matrix(htrain_col[820])))

h2o.giniCoef(h2o.performance(model_rf, newdata = htest_col))
##[1] 0.3073046
h2o.giniCoef(h2o.performance(model_rf, newdata = htrain_col))
##[1] 0.5951025


#stacked Ensemble#
ensemble <- h2o.stackedEnsemble(x = 1:819,
                                y = 820,
                                training_frame = htrain_col,
                                model_id = "my_ensemble_binomial",
                                base_models = list(model_rf@model_id, model_gbm@model_id))

y_train_ens <- h2o.predict(ensemble, htrain_col[-820])$predict
y_train_ens <- as.factor(as.matrix(y_train_ens))
y_test_ens <- h2o.predict(ensemble, htest_col[-820])$predict
y_test_ens <- as.factor(as.matrix(y_test_ens))
table(y_test_ens,as.factor(as.matrix(htest_col[820])))
table(y_train_ens,as.factor(as.matrix(htrain_col[820])))

h2o.giniCoef(h2o.performance(ensemble, newdata = htest_col))
##[1] 0.3135956
h2o.giniCoef(h2o.performance(ensemble, newdata = htrain_col))
##[1] 0.5907351

## The stacked ensemble is just predicting one class even though gini is good.



##########Reduced col data set

model_dl_red <- h2o.deeplearning(x = 1:98,  # column numbers for predictors
                             y = 99,   # column number for label
                             training_frame = htrain_red,
                             model_id="dl_h2o_red",
                             activation = "TanhWithDropout",
                             hidden = c(100, 100, 100),  
                             epochs = 100,
                             balance_classes = TRUE,
                             seed=4200,
                             nfolds=10,
                             fold_assignment = "Modulo",
                             keep_cross_validation_fold_assignment=TRUE,
                             keep_cross_validation_predictions=TRUE)

yhat_train_red <- h2o.predict(model_dl_red, htrain_red[-99])$predict
yhat_train_red <- as.factor(as.matrix(yhat_train_red))
yhat_test_red <- h2o.predict(model_dl_red, htest_red[-99])$predict
yhat_test_red <- as.factor(as.matrix(yhat_test_red))
table(yhat_test_red,as.factor(as.matrix(htest_red[99])))
table(yhat_train_red,as.factor(as.matrix(htrain_red[99])))

#yhat_test_red    0    1
#              0  208 2646
#              1  254 7123
#100 epochs




h2o.giniCoef(h2o.performance(model_dl_red, newdata = htest_red))
##[1] 0.2624157
h2o.giniCoef(h2o.performance(model_dl_red, newdata = htrain_red))
##[1] 0.2960044
##Consistent but predicting one class only

###H2o GBM reduced cols###

model_gbm_red <- h2o.gbm(x = 1:98,
                     y = 99,
                     training_frame = htrain_red,
                     model_id="gb_h2o_red",
                     distribution = "bernoulli",
                     ntrees = 50,
                     max_depth = 5,
                     min_rows = 2,
                     learn_rate = 0.035,
                     nfolds = 10,
                     fold_assignment = "Modulo",
                     balance_classes=TRUE,
                     keep_cross_validation_predictions = TRUE,
                     seed = 1234)

y_train_gb_red <- h2o.predict(model_gbm_red, htrain_red[-99])$predict
y_train_gb_red <- as.factor(as.matrix(y_train_gb_red))
y_test_gb_red <- h2o.predict(model_gbm_red, htest_red[-99])$predict
y_test_gb_red <- as.factor(as.matrix(y_test_gb_red))
table(y_test_gb_red,as.factor(as.matrix(htest_red[99])))
table(y_train_gb_red,as.factor(as.matrix(htrain_red[99])))

h2o.giniCoef(h2o.performance(model_gbm_red, newdata = htest_red))
##[1] 0.2853635
h2o.giniCoef(h2o.performance(model_gbm_red, newdata = htrain_red))
##[1] 0.6166519
##Again overfits. Generalization in doubt.

###2nd gbm####
model_gbm_red2 <- h2o.gbm(x = 1:98,
                         y = 99,
                         training_frame = htrain_red,
                         model_id="gb_h2o_red2",
                         distribution = "bernoulli",
                         ntrees = 80,
                         max_depth = 5,
                         min_rows = 2,
                         learn_rate = 0.035,
                         nfolds = 10,
                         fold_assignment = "Modulo",
                         balance_classes=TRUE,
                         keep_cross_validation_predictions = TRUE,
                         seed = 1234)

y_train_gb_red2 <- h2o.predict(model_gbm_red2, htrain_red[-99])$predict
y_train_gb_red2 <- as.factor(as.matrix(y_train_gb_red2))
y_test_gb_red2 <- h2o.predict(model_gbm_red2, htest_red[-99])$predict
y_test_gb_red2 <- as.factor(as.matrix(y_test_gb_red2))
table(y_test_gb_red2,as.factor(as.matrix(htest_red[99])))
table(y_train_gb_red2,as.factor(as.matrix(htrain_red[99])))

h2o.giniCoef(h2o.performance(model_gbm_red2, newdata = htest_red))
##[1] 0.2977111
h2o.giniCoef(h2o.performance(model_gbm_red2, newdata = htrain_red))
##[1] 0.6839273

##Overfits. Could be that these models are good for stacking for ensembling?



###Ensemble 2###
ensemble2 <- h2o.stackedEnsemble(x = 1:98,
                                y = 99,
                                training_frame = htrain_red,
                                model_id = "my_ensemble_binomial2",
                                base_models = list(model_gbm_red4@model_id,model_rf_red@model_id))

y_train_ens2 <- h2o.predict(ensemble2, htrain_red[-99])$predict
y_train_ens2 <- as.factor(as.matrix(y_train_ens2))
y_test_ens2 <- h2o.predict(ensemble2, htest_red[-99])$predict
y_test_ens2 <- as.factor(as.matrix(y_test_ens2))
table(y_test_ens2,as.factor(as.matrix(htest_red[99])))
table(y_train_ens2,as.factor(as.matrix(htrain_red[99])))

h2o.giniCoef(h2o.performance(ensemble2, newdata = htest_red))
##[1]  0.3028934
h2o.giniCoef(h2o.performance(ensemble2, newdata = htrain_red))
##[1] 0.9308917

##Stacked ensembles are massively overfitting!!


####3rd gbm##
model_gbm_red3 <- h2o.gbm(x = 1:98,
                          y = 99,
                          training_frame = htrain_red,
                          model_id="gb_h2o_red3",
                          distribution = "bernoulli",
                          ntrees = 100,
                          max_depth = 5,
                          min_rows = 2,
                          learn_rate = 0.035,
                          nfolds = 10,
                          fold_assignment = "Modulo",
                          balance_classes=TRUE,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1234)

y_train_gb_red3 <- h2o.predict(model_gbm_red3, htrain_red[-99])$predict
y_train_gb_red3 <- as.factor(as.matrix(y_train_gb_red3))
y_test_gb_red3 <- h2o.predict(model_gbm_red3, htest_red[-99])$predict
y_test_gb_red3 <- as.factor(as.matrix(y_test_gb_red3))
table(y_test_gb_red3,as.factor(as.matrix(htest_red[99])))
table(y_train_gb_red3,as.factor(as.matrix(htrain_red[99])))

h2o.giniCoef(h2o.performance(model_gbm_red3, newdata = htest_red))
##[1] 0.2559749
h2o.giniCoef(h2o.performance(model_gbm_red3, newdata = htrain_red))
##[1] 0.9991768

## Massively overfits

##gbm4##
model_gbm_red4 <- h2o.gbm(x = 1:98,
                          y = 99,
                          training_frame = htrain_red,
                          model_id="gb_h2o_red3",
                          distribution = "bernoulli",
                          ntrees = 140,
                          max_depth = 9,
                          min_rows = 2,
                          learn_rate = 0.04,
                          nfolds = 10,
                          fold_assignment = "Modulo",
                          balance_classes=TRUE,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1234)

y_train_gb_red4 <- h2o.predict(model_gbm_red4, htrain_red[-99])$predict
y_train_gb_red4 <- as.factor(as.matrix(y_train_gb_red4))
y_test_gb_red4 <- h2o.predict(model_gbm_red4, htest_red[-99])$predict
y_test_gb_red4 <- as.factor(as.matrix(y_test_gb_red4))
table(y_test_gb_red4,as.factor(as.matrix(htest_red[99])))
table(y_train_gb_red4,as.factor(as.matrix(htrain_red[99])))

h2o.giniCoef(h2o.performance(model_gbm_red4, newdata = htest_red))
##[1] 0.2559749
h2o.giniCoef(h2o.performance(model_gbm_red4, newdata = htrain_red))
##[1] 0.9991768

## Massively overfits

######RF new reduced####
####H2o RandomForest###
model_rf_red <- h2o.randomForest(x = 1:98,
                             y = 99,
                             training_frame = htrain_red,
                             model_id = "rf_h2o_red",
                             ntrees = 1000,
                             nfolds = 10,
                             ignore_const_cols = TRUE,
                             max_depth = 10,
                             mtries=-1,
                             balance_classes=TRUE,
                             fold_assignment = "Modulo",
                             keep_cross_validation_predictions = TRUE,
                             seed = 1234)

y_train_rf_red <- h2o.predict(model_rf_red, htrain_red[-99])$predict
y_train_rf_red <- as.factor(as.matrix(y_train_rf_red))
y_test_rf_red <- h2o.predict(model_rf_red, htest_red[-99])$predict
y_test_rf_red <- as.factor(as.matrix(y_test_rf_red))
table(y_test_rf_red,as.factor(as.matrix(htest_red[99])))
table(y_train_rf_red,as.factor(as.matrix(htrain_red[99])))

h2o.giniCoef(h2o.performance(model_rf_red, newdata = htest_red))
##[1]  0.3028251
h2o.giniCoef(h2o.performance(model_rf_red, newdata = htrain_red))
##[1] 0.9654449
h2o.auc(h2o.performance(model_rf_red, newdata = htest_red))
h2o.auc(h2o.performance(model_rf_red, newdata = htrain_red))

##Overfits massively. 
