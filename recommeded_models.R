library(caret)
library(xgboost)
###best params tweaked###
set.seed(4200)
param_tw<-list(eta = 0.035,
               max_depth = 5,
               subsample = 0.64,
               colsample_bytree = 0.77,
               min_child_weight=1,
               max_delta_step=2,
               gamma=0.2,
               scale_pos_weight=0.05,
               eval_metric = "auc",
               objective = "binary:logistic")
nround_tw<-10
md2 <- xgb.train(data=train_mat_2, params=param_tw, nrounds=nround_tw, nthread=4)

pred_md2 <- predict(md2, train_mat_2)
pred_md2<-as.data.frame(pred_md2)
pred_class_md2<-ifelse(pred_md2$pred_md2>0.5,1,0)

pred_test_md2<-predict(md2,test_mat_2)
pred_test_md2<-as.data.frame(pred_test_md2)
pred_class_test_md2<-ifelse(pred_test_md2>0.5,1,0)
table(test_set$bad_flag_worst6,pred_class_test_md2)
Gini(y_pred=pred_test_md2$pred_test_md2,y_true=test_set$bad_flag_worst6)

Gini(y_pred=pred_md2$pred_md2,y_true=train_set$bad_flag_worst6)

table(pred_class_md2,train_set$bad_flag_worst6)

####Model Performance of md2
##Test Gini 0.3020612
##Train Gini 0.5076573
##Still a case of a little overfitting but by far one of the most stable models.
##The model most likely I will recommend(reasonable accuracy and generalization)


######
zero_index<-nearZeroVar(train_set[-c(1,29,30)])
t_set<-train_set[-c(1,29,30)]
train_wo_zero<-t_set[-c(zero_index)]
ylab<-train_set$bad_flag_worst6
te_set<-test_set[-c(1,29,30)]
test_wo_zero<-te_set[-c(zero_index)]
ylab_test<-test_set$bad_flag_worst6
ylab<-as.numeric(as.character(ylab))
ylab_test<-as.numeric(as.character(ylab_test))

red_train_frame<-as.data.frame(cbind(train_wo_zero,ylab))
red_test_frame<-as.data.frame(cbind(test_wo_zero,ylab_test))

red_train_mat<-xgb.DMatrix(data = data.matrix(train_wo_zero), label=ylab)
red_test_mat<-xgb.DMatrix(data = data.matrix(test_wo_zero), label=ylab_test)

set.seed(4200)
param_tw2<-list(eta = 0.04,
                max_depth = 5,
                subsample = 0.64,
                colsample_bytree = 0.77,
                min_child_weight=1,
                max_delta_step=2,
                gamma=0.2,
                scale_pos_weight=0.05,
                eval_metric = "auc",
                objective = "binary:logistic")
nround_tw2<-120
md3 <- xgb.train(data=red_train_mat, params=param_tw2, nrounds=nround_tw2, nthread=4)

pred_md3 <- predict(md3, red_train_mat)
pred_md3<-as.data.frame(pred_md3)
pred_class_md3<-ifelse(pred_md3$pred_md3>0.5,1,0)

pred_test_md3<-predict(md3,red_test_mat)
pred_test_md3<-as.data.frame(pred_test_md3)
pred_class_test_md3<-ifelse(pred_test_md3>0.5,1,0)
table(test_set$bad_flag_worst6,pred_class_test_md3)
Gini(y_pred=pred_test_md3$pred_test_md3,y_true=test_set$bad_flag_worst6)
## Test set [1] 0.334041
Gini(y_pred=pred_md3$pred_md3,y_true=train_set$bad_flag_worst6)
## Train set [1] 0.7186369
table(pred_class_md3,train_set$bad_flag_worst6)


####Model Performance of md3
##Test Gini 0.334041
##Train Gini 0.7186369
##Still a case of a little overfitting but highest accuracy achieved on test set.
##I would like to collect another validation set and check if the results scale on another set
##of data before deploying it. The increase in accuracy is marginal but the stability of previous model is better

