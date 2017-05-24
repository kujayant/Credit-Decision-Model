# Credit-Decision-Model
Predict likely DPD using a highly unbalanced dataset
Use processed_dummified to get test and train data using data_part variable. 
Label var is "bad_flag_worst6".
Originally,
If bad_flag_worst6 value is 0 – it means customer has good credit history
If bad_flag_worst6 value is 1 –it means customer has falls into 30 DPD + bucket
For modeling purposes, I have switched it.

##Features###
You can see the gain statistics of top 50 features in "feature_gain.csv"
In total there are 820 features.
Account_features.R creates features using the raw_account_xx files.
Enquiry_features.R creates features using the raw_enquiry_xx files.
merge_raw.R merges accounts and enquiry features with raw_data_xx file.
The subsequent cleaning and dummification of vars is contained in merge_raw.R file.

##Model##
A single xgboost model seems to have outperformed the existing benchmark. To be sure that my results are really correct,
I have used confusion matrix, gini and AUC to check the veracity of results. You can see the xgboost settings in Model.R.

##Results##
The confusion matrices of train and test set are below:
##Train Set##
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0   947   464
         1    56 22414
                                        
               Accuracy : 0.9782        
                 95% CI : (0.9763, 0.98)
    No Information Rate : 0.958         
    P-Value [Acc > NIR] : < 2.2e-16     
                                        
                  Kappa : 0.7735        
 Mcnemar's Test P-Value : < 2.2e-16     
                                        
            Sensitivity : 0.94417       
            Specificity : 0.97972       
         Pos Pred Value : 0.67116       
         Neg Pred Value : 0.99751       
             Prevalence : 0.04200       
         Detection Rate : 0.03965       
   Detection Prevalence : 0.05908       
      Balanced Accuracy : 0.96194       
                                        
       'Positive' Class : 0             
                                        
##Test Set##
Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0  399  215
         1   63 9554
                                          
               Accuracy : 0.9728          
                 95% CI : (0.9695, 0.9759)
    No Information Rate : 0.9548          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.7276          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.86364         
            Specificity : 0.97799         
         Pos Pred Value : 0.64984         
         Neg Pred Value : 0.99345         
             Prevalence : 0.04516         
         Detection Rate : 0.03900         
   Detection Prevalence : 0.06001         
      Balanced Accuracy : 0.92081         
                                          
       'Positive' Class : 0         
       
The model seems to be generalizing well. 

AUC
##Train Set##
Area under the curve: 0.9975
##Test Set##
Area under the curve: 0.9928

GINI
(Calculated using Gini function from MLmetrics library)
##Train Set##
0.9950813
##Test Set##
0.9856242



