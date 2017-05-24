# Credit-Decision-Model

Predict likely DPD using a highly unbalanced dataset
Use processed_dummified to get test and train data using data_part variable. 
Label var is "bad_flag_worst6".

Originally,

If bad_flag_worst6 value is 0 – it means customer has good credit history

If bad_flag_worst6 value is 1 –it means customer has falls into 30 DPD + bucket

For modeling purposes, I have switched it. Now my cut-off probability can be 0.5. Otherwise predicted probabilities were very small.

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

![alt text](https://github.com/kujayant/Credit-Decision-Model/blob/master/Train_Conf_mat.PNG)

    
##Test Set##

Confusion Matrix and Statistics

![alt text](https://github.com/kujayant/Credit-Decision-Model/blob/master/Test_conf_mat.PNG)

       
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



