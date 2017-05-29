library(caret)
library(xgboost)
library(DMwR)


##Smote on reduced columns data##
set.seed(1234)
smote_train_red<-SMOTE(ylab ~ ., data=red_train_frame,perc.over=500,perc.under=100)                         
table(smote_train_red$ylab)
class(smote_train_red$ylab)
smote_train_red$ylab<-as.numeric(as.character(smote_train_red$ylab))
###smoted xgb###
smote_red_train_mat<-xgb.DMatrix(data = data.matrix(smote_train_red[,c(1:98)]), label=smote_train_red$ylab)
set.seed(4200)
param_tw_red<-list(eta = 0.08,
                max_depth = 5,
                subsample = 0.64,
                colsample_bytree = 0.7,
                min_child_weight=1,
                max_delta_step=2,
                gamma=5,
                eval_metric = "auc",
                objective = "binary:logistic")
nround_tw_red<-185
md_red <- xgb.train(data=smote_red_train_mat, params=param_tw_red, nrounds=nround_tw_red, nthread=4)

pred_md_red <- predict(md_red, smote_red_train_mat)
pred_md_red<-as.data.frame(pred_md_red)
pred_class_md_red<-ifelse(pred_md_red$pred_md_red>0.5,1,0)

pred_test_md_red<-predict(md_red,red_test_mat)
pred_test_md_red<-as.data.frame(pred_test_md_red)
pred_class_test_md_red<-ifelse(pred_test_md_red>0.5,1,0)
table(test_set$bad_flag_worst6,pred_class_test_md_red)
Gini(y_pred=pred_test_md_red$pred_test_md_red,y_true=test_set$bad_flag_worst6)

Gini(y_pred=pred_md_red$pred_md_red,y_true=smote_train_red$ylab)

table(pred_class_md_red,smote_train_red$ylab)

####
#Gini on test set
#[1] 0.290055
#Gini on train set
#[1] 0.954698
#The smoted model hugely overfits. The idea to introduce noise in minority class
# to learn more has not worked well in this case.