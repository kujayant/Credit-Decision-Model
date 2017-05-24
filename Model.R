##Model building
library(xgboost)
train_set$bad_flag_worst6[train_set$bad_flag_worst6==0]<-"N"
train_set$bad_flag_worst6[train_set$bad_flag_worst6==1]<-"Y"
test_set$bad_flag_worst6[test_set$bad_flag_worst6==0]<-"N"
test_set$bad_flag_worst6[test_set$bad_flag_worst6==1]<-"Y"
train_set$bad_flag_worst6[train_set$bad_flag_worst6=="N"]<-1
train_set$bad_flag_worst6[train_set$bad_flag_worst6=="Y"]<-0
test_set$bad_flag_worst6[test_set$bad_flag_worst6=="N"]<-1
test_set$bad_flag_worst6[test_set$bad_flag_worst6=="Y"]<-0
##Switched labels so that classification can happen at 0.5 kinda threshold.

y=train_set$bad_flag_worst6
##col 31 bad_flaf_worst6
pos_weight=sum(length(y[y==0])/length(y[y==1]))
set.seed(1234)
xgb3 <- xgboost(data = data.matrix(train_set[,-c(1,30)]), 
                label = y, 
                eta = 0.1,
                max_depth = 25, 
                nround=3, 
                subsample = 0.5,
                colsample_bytree = 0.5,
                colsample_bylevel=0.5,
                scale_pos_weight=pos_weight,
                eval_metric = "auc",
                objective = "binary:logistic",
                nthread = 4
                
                
)

train_mat <- xgb.DMatrix(data = data.matrix(train_set[-c(1,30)]), label=y)
test_mat <- xgb.DMatrix(data = data.matrix(test_set[-c(1,30)]), label=test_set$bad_flag_worst6)


library(pROC)
pred <- predict(xgb3, train_mat)
pred<-as.data.frame(pred)
pred_class<-ifelse(pred$pred>0.5,1,0)
pred_test<-predict(xgb3,test_mat)
auc(train_set$bad_flag_worst6,pred$pred)
#Area under the curve: 0.9975
table(train_set$bad_flag_worst6,pred_class)
#pred_class
#      0     1
#0   947    56
#1   464 22414
pred_test<-as.data.frame(pred_test)
pred_class_test<-ifelse(pred_test>0.5,1,0)
table(pred_class_test,test_set$bad_flag_worst6)
#pred_class_test    0    1
#0                 399  215
#1                  63 9554
 
auc(test_set$bad_flag_worst6,pred_test$pred_test)
#Area under the curve: 0.9928

library(MLmetrics)
Gini(y_pred=pred_test$pred_test,y_true=test_set$bad_flag_worst6)
##[1] 0.9856242 !! Test set Gini
Gini(y_pred=pred$pred,y_true=y)
##[1] 0.9950813 !! Train set Gini
##Unbelivable Gini :)

##importance matrix
imp_mat <- xgb.importance (feature_names = colnames(train_set[-c(1,30)]),model = xgb3)
xgb.plot.importance(importance_matrix = imp_mat[1:20])

imp_mat<-as.data.frame(imp_mat)
View(imp_mat)
write.csv(imp_mat,"E:/Tooks/RBL_70_30/feature_gain.csv",row.names=FALSE)
