
raw_data_70<-read.csv("E:/Tooks/RBL_70_30/raw_data_70.csv",header=T,stringsAsFactors = FALSE)

raw_data_30<-read.csv("E:/Tooks/RBL_70_30/raw_data_30.csv",header=T,stringsAsFactors = FALSE)

raw_data_70$ref_code<-ifelse(raw_data_70$ref_code=="",paste0("#",raw_data_70$apprefno),raw_data_70$ref_code)
raw_data_30$ref_code<-ifelse(raw_data_30$ref_code=="",paste0("#",raw_data_30$apprefno),raw_data_30$ref_code)


raw_data_70$ref_code<-substr(raw_data_70$ref_code,2,nchar(raw_data_70$ref_code))
raw_data_30$ref_code<-substr(raw_data_30$ref_code,2,nchar(raw_data_30$ref_code))
account_70$externalid<-substr(account_70$externalid,2,nchar(account_70$externalid))
account_30$externalid<-substr(account_30$externalid,2,nchar(account_30$externalid))
###merge account and raw data

merged_enq_data_70<-merge(raw_data_70,account_70,by.x="ref_code",by.y="externalid",all.x=TRUE)

merged_enq_data_30<-merge(raw_data_30,account_30,by.x="ref_code",by.y="externalid",all.x=TRUE)

###merge merged data and enquiry data

enq_70$externalid<-substr(enq_70$externalid,2,nchar(enq_70$externalid))
enq_30$externalid<-substr(enq_30$externalid,2,nchar(enq_30$externalid))

all_merged_70<-merge(merged_enq_data_70,enq_70,by.x="ref_code",by.y="externalid",all.x=TRUE)
all_merged_30<-merge(merged_enq_data_30,enq_30,by.x="ref_code",by.y="externalid",all.x=TRUE)

#####Merged dataset is prepared. More features could be created and more cleaning is required.

unique(all_merged_70$card_name)
all_merged_70$card_name[all_merged_70$card_name==""]<-"Other"
unique(all_merged_30$card_name)
all_merged_30$card_name[all_merged_30$card_name==""]<-"Other"

unique(all_merged_70$promo_code)
all_merged_70$promo_code[all_merged_70$promo_code==""]<-"Other"
unique(all_merged_30$promo_code)
all_merged_30$promo_code[all_merged_30$promo_code==""]<-"Other"

sum(is.na(all_merged_70$cibil_score))
summary(all_merged_70$cibil_score)
all_merged_70$cibil_score[all_merged_70$cibil_score<300 & !is.na(all_merged_70$cibil_score)]
summary(all_merged_70$cibil_score[all_merged_70$cibil_score>=300 & !is.na(all_merged_70$cibil_score)])
##Imputing NAs with 1st quartile value of 698
all_merged_70$cibil_score[all_merged_70$cibil_score<300]<-698
all_merged_70$cibil_score[is.na(all_merged_70$cibil_score)]<-698

sum(is.na(all_merged_30$cibil_score))
summary(all_merged_30$cibil_score)
all_merged_30$cibil_score[all_merged_30$cibil_score<300]<-698
all_merged_30$cibil_score[is.na(all_merged_30$cibil_score)]<-698

unique(all_merged_70$aip_status)
sum(is.na(all_merged_70$aip_status))
table(all_merged_70$aip_status)
##Imputing most frequent status
all_merged_70$aip_status[is.na(all_merged_70$aip_status)]<-3
table(all_merged_30$aip_status)
sum(is.na(all_merged_30$aip_status))
all_merged_30$aip_status[is.na(all_merged_30$aip_status)]<-3

##app_disposition - likely to be a NearzeroVar
unique(all_merged_70$app_disposition)
table(all_merged_70$app_disposition)
all_merged_70$app_disposition[all_merged_70$app_disposition==""]<-"Other"

unique(all_merged_30$app_disposition)
table(all_merged_30$app_disposition)
all_merged_30$app_disposition[all_merged_30$app_disposition==""]<-"Other"

#Status Type also a likely candidate for nearZeroVar
unique(all_merged_70$status_type)
table(all_merged_70$status_type)
all_merged_70$status_type[is.na(all_merged_70$status_type)]<-15

unique(all_merged_30$status_type)
table(all_merged_30$status_type)
all_merged_30$status_type[is.na(all_merged_30$status_type)]<-15

##approved credit limit
summary(all_merged_70$approved_credit_limit)
all_merged_70$approved_credit_limit[all_merged_70$approved_credit_limit==0]<-72000
all_merged_70$approved_credit_limit[is.na(all_merged_70$approved_credit_limit)]<-72000

summary(all_merged_30$approved_credit_limit)
all_merged_30$approved_credit_limit[all_merged_30$approved_credit_limit==0]<-72000
all_merged_30$approved_credit_limit[is.na(all_merged_30$approved_credit_limit)]<-72000

##reject reason code and desc
unique(all_merged_70$reject_reason_code)
table(all_merged_70$reject_reason_code)
all_merged_70$reject_reason_code[all_merged_70$reject_reason_code==""]<-"Other"

unique(all_merged_30$reject_reason_code)
table(all_merged_30$reject_reason_code)
all_merged_30$reject_reason_code[all_merged_30$reject_reason_code==""]<-"Other"

##intl_trn
unique(all_merged_70$intl_trn)
table(all_merged_70$intl_trn)
all_merged_70$intl_trn[all_merged_70$intl_trn==""]<-"N"

unique(all_merged_30$intl_trn)
table(all_merged_30$intl_trn)
all_merged_30$intl_trn[all_merged_30$intl_trn==""]<-"N"

#fee_code
unique(all_merged_70$fee_code)
table(all_merged_70$fee_code)
all_merged_70$fee_code[all_merged_70$fee_code==""]<-"PD1"

unique(all_merged_30$fee_code)
table(all_merged_30$fee_code)
all_merged_30$fee_code[all_merged_30$fee_code==""]<-"PD1"

#override fee code
unique(all_merged_70$override_fee_code)
table(all_merged_70$override_fee_code)
all_merged_70$override_fee_code[all_merged_70$override_fee_code==""]<-"OT"

unique(all_merged_30$override_fee_code)
table(all_merged_30$override_fee_code)
all_merged_30$override_fee_code[all_merged_30$override_fee_code==""]<-"OT"

##override fee yes or no
all_merged_70$override_fee_binary<-ifelse(all_merged_70$override_fee_code=="OT","N","Y")
all_merged_30$override_fee_binary<-ifelse(all_merged_30$override_fee_code=="OT","N","Y")

##override months
summary(all_merged_70$override_months)
table(all_merged_70$override_months)
all_merged_70$override_months[is.na(all_merged_70$override_months)]<-0

summary(all_merged_30$override_months)
table(all_merged_30$override_months)
all_merged_30$override_months[is.na(all_merged_30$override_months)]<-0

#acq source -- too many levels. Maybe difficult to dummify
unique(all_merged_70$acq_source)
sum(is.na(all_merged_70$acq_source))
all_merged_70$acq_source[all_merged_70$acq_source==""]<-"OT"

unique(all_merged_30$acq_source)
sum(is.na(all_merged_30$acq_source))
all_merged_30$acq_source[all_merged_30$acq_source==""]<-"OT"

##se_code -- Too many levels. Maybe difficult to dummify
unique(all_merged_70$se_code)
sum(is.na(all_merged_70$se_code))
all_merged_70$se_code[all_merged_70$se_code==""]<-"OT"

unique(all_merged_30$se_code)
sum(is.na(all_merged_30$se_code))
all_merged_30$se_code[all_merged_30$se_code==""]<-"OT"

## Lead and Mktg Code
##Too many levels in Lead Code
unique(all_merged_70$lead_code)
sum(is.na(all_merged_70$lead_code))
all_merged_70$lead_code[all_merged_70$lead_code==""]<-"OT"

unique(all_merged_30$lead_code)
sum(is.na(all_merged_30$lead_code))
all_merged_30$lead_code[all_merged_30$lead_code==""]<-"OT"

##Mktg Code
unique(all_merged_70$mktg_code)
unique(all_merged_30$mktg_code)
##Deciding to ignore lead and marketing code

##app_gender
unique(all_merged_70$app_gender)
table(all_merged_70$app_gender)
sum(is.na(all_merged_70$app_gender))
all_merged_70$app_gender[is.na(all_merged_70$app_gender)]<-2

unique(all_merged_30$app_gender)
table(all_merged_30$app_gender)
sum(is.na(all_merged_30$app_gender))
all_merged_30$app_gender[is.na(all_merged_30$app_gender)]<-2

##app_pan Nothing to do here
sum(is.na(all_merged_70$app_pan))

##app_dob
sum(is.na(all_merged_70$app_dob))
class(all_merged_70$app_dob)
all_merged_70$app_dob<-as.Date(all_merged_70$app_dob,format="%d-%b-%y")

sum(is.na(all_merged_30$app_dob))
class(all_merged_30$app_dob)
all_merged_30$app_dob<-as.Date(all_merged_30$app_dob,format="%d-%b-%y")

#creating age var
today_70<-rep(as.Date("2017-05-24"),times=nrow(all_merged_70))
today_30<-rep(as.Date("2017-05-24"),times=nrow(all_merged_30))
all_merged_70$app_age<-as.numeric(difftime(today_70,all_merged_70$app_dob,units="weeks"))
all_merged_30$app_age<-as.numeric(difftime(today_30,all_merged_30$app_dob,units="weeks"))

##mob_verified
unique(all_merged_70$mob_verified)
table(all_merged_70$mob_verified)
all_merged_70$mob_verified[all_merged_70$mob_verified==""]<-"Y"

unique(all_merged_30$mob_verified)
table(all_merged_30$mob_verified)
all_merged_30$mob_verified[all_merged_30$mob_verified==""]<-"Y"

##email - doing nothing
unique(all_merged_70$email)

##marital_status
unique(all_merged_70$marital_status)
sum(is.na(all_merged_70$marital_status))
table(all_merged_70$marital_status)
all_merged_70$marital_status[is.na(all_merged_70$marital_status)]<-1

unique(all_merged_30$marital_status)
sum(is.na(all_merged_30$marital_status))
table(all_merged_30$marital_status)
all_merged_30$marital_status[is.na(all_merged_30$marital_status)]<-1

##num_dependents
summary(all_merged_70$num_dependents)
all_merged_70$num_dependents[is.na(all_merged_70$num_dependents)]<-0

summary(all_merged_30$num_dependents)
all_merged_30$num_dependents[is.na(all_merged_30$num_dependents)]<-0

##edu_qualification
unique(all_merged_70$edu_qualification)
unique(all_merged_30$edu_qualification)

all_merged_70$edu_qualification[all_merged_70$edu_qualification==""]<-"UnKn"
all_merged_30$edu_qualification[all_merged_30$edu_qualification==""]<-"UnKn"

##app_res_city

unique(all_merged_70$app_res_city)
sum(is.na(all_merged_70$app_res_city))
all_merged_70$app_res_city[all_merged_70$app_res_city==""]<-"OT City"


unique(all_merged_30$app_res_city)
sum(is.na(all_merged_30$app_res_city))
all_merged_30$app_res_city[all_merged_30$app_res_city==""]<-"OT City"

##res_from_year
summary(all_merged_70$res_from_yr)
all_merged_70$res_from_yr[is.na(all_merged_70$res_from_yr)]<-2005

summary(all_merged_30$res_from_yr)
all_merged_30$res_from_yr[is.na(all_merged_30$res_from_yr)]<-2005

##res_from_month
summary(all_merged_70$res_from_month)
all_merged_70$res_from_month[is.na(all_merged_70$res_from_month)]<-0

summary(all_merged_30$res_from_month)
all_merged_30$res_from_month[is.na(all_merged_30$res_from_month)]<-0

##res-type
unique(all_merged_70$res_type)
unique(all_merged_30$res_type)

table(all_merged_70$res_type)
table(all_merged_30$res_type)
all_merged_70$res_type[all_merged_70$res_type==""]<-"Self"
all_merged_30$res_type[all_merged_30$res_type==""]<-"Self"

all.equal(names(all_merged_70),names(all_merged_30))

###COmbining test and train for dumification and further cleaning and column selection

all_merged_70$data_part<-rep("Train",times=rep(nrow(all_merged_70)))
all_merged_30$data_part<-rep("Test",times=rep(nrow(all_merged_30)))

all_merged<-rbind(all_merged_70,all_merged_30)

all_merged_slct_cols<-all_merged[c(1,5,6,8,9,10,11,12,13,16,17,18,19,20,
                                   21,22,23,24,28,29,30,31,32,33,35,36,37,
                                   38,39,40,41,42,44:48,51,53,55,56,57,
                                   58,60:67,69:70,72:74,76:79,81,83,84:86,87:129)]

View(all_merged_slct_cols)

##Email binary
all_merged_slct_cols$email_binary<-ifelse(all_merged_slct_cols$email=="",0,1)

##net monthly income
all_merged_slct_cols$net_monthly_income[is.na(all_merged_slct_cols$net_monthly_income)]<-30000
length(all_merged_slct_cols$net_monthly_income[all_merged_slct_cols$net_monthly_income==1])

##Company Type
all_merged_slct_cols$company_type[all_merged_slct_cols$company_type==""]<-"UnKn"
all_merged_slct_cols$company_type[all_merged_slct_cols$company_type=="Partnership Co."]<-"Partnership"
all_merged_slct_cols$company_type[all_merged_slct_cols$company_type=="Public Ltd Co."]<-"PSU"

##industry type
all_merged_slct_cols$industry_type[all_merged_slct_cols$industry_type==""]<-"UnKn"

##month_joining
##removing NA rows from dataset. Most of the vars need imputation and not likely to yield any results.
all_merged_slct_cols_<-all_merged_slct_cols[!is.na(all_merged_slct_cols$month_joining),]

####
table(all_merged_slct_cols_$id_proof)
all_merged_slct_cols_$id_proof[all_merged_slct_cols_$id_proof==""]<-"PAN Card"

##existing bank
#HDFC Ltd.
all_merged_slct_cols_$existing_bank[all_merged_slct_cols_$existing_bank=="HDFC Ltd."]<-"HDFC Bank"
all_merged_slct_cols_$existing_bank[all_merged_slct_cols_$existing_bank==""]<-"UnKn"

##existing card issues
all_merged_slct_cols_$existing_card_issuer[all_merged_slct_cols_$existing_card_issuer==""]<-"No Bank"

##existing card age
all_merged_slct_cols_$existing_card_start_date<-as.Date(all_merged_slct_cols_$existing_card_start_date,format="%d-%b-%y")
todays<-rep(as.Date("2017-05-24"),times=nrow(all_merged_slct_cols_))
all_merged_slct_cols_$existing_card_age<-as.numeric(difftime(todays,all_merged_slct_cols_$existing_card_start_date,unit="weeks"))
summary(all_merged_slct_cols_$existing_card_age)
all_merged_slct_cols_$existing_card_age[is.na(all_merged_slct_cols_$existing_card_age)]<-0

##app_existing_other_loan_acc
all_merged_slct_cols_$app_existing_other_loan_cc[all_merged_slct_cols_$app_existing_other_loan_cc==""]<-"N"

##app coupon code
all_merged_slct_cols_$app_coupon_code[all_merged_slct_cols_$app_coupon_code==""]<-"No Coupon"

#app_go_green
all_merged_slct_cols_$app_go_green[all_merged_slct_cols_$app_go_green==""]<-"N"

#app_account_type
all_merged_slct_cols_$app_account_type[is.na(all_merged_slct_cols_$app_account_type)]<-0


##total_diff_lastpaymt_opened_dt
summary(all_merged_slct_cols_$total_diff_lastpaymt_opened_dt)
##Imputing median value
all_merged_slct_cols_$total_diff_lastpaymt_opened_dt[is.na(all_merged_slct_cols_$total_diff_lastpaymt_opened_dt)]<-2116

##mean_diff_lastpaymt_opened_dt
summary(all_merged_slct_cols_$mean_diff_lastpaymt_opened_dt)
all_merged_slct_cols_$mean_diff_lastpaymt_opened_dt[is.na(all_merged_slct_cols_$mean_diff_lastpaymt_opened_dt)]<-509.1

summary(all_merged_slct_cols_$sd_diff_lastpaymt_opened_dt)
all_merged_slct_cols_$sd_diff_lastpaymt_opened_dt[is.na(all_merged_slct_cols_$sd_diff_lastpaymt_opened_dt)]<-338


summary(all_merged_slct_cols_$sd_high_credit_amt)
all_merged_slct_cols_$sd_high_credit_amt[is.na(all_merged_slct_cols_$sd_high_credit_amt)]<-67170

summary(all_merged_slct_cols_$sd_cur_balance_amt)
all_merged_slct_cols_$sd_cur_balance_amt[is.na(all_merged_slct_cols_$sd_cur_balance_amt)]<-0

#utilisation trend
summary(all_merged_slct_cols_$utilisation_trend)
all_merged_slct_cols_$utilisation_trend[is.na(all_merged_slct_cols_$utilisation_trend)]<-1.195


all_merged_slct_cols_$mean_valueofcollateral[is.na(all_merged_slct_cols_$mean_valueofcollateral)]<-0
all_merged_slct_cols_$sd_valueofcollateral[is.na(all_merged_slct_cols_$sd_valueofcollateral)]<-0

all_merged_slct_cols_$avg_repayment_tenure[is.na(all_merged_slct_cols_$avg_repayment_tenure)]<-0

all_merged_slct_cols_$count_enquiry_recency_90_<-as.numeric(all_merged_slct_cols_$count_enquiry_recency_90_)
all_merged_slct_cols_$count_enquiry_recency_90_[is.na(all_merged_slct_cols_$count_enquiry_recency_90_)]<-0

all_merged_slct_cols_$count_enquiry_recency_365_<-as.numeric(all_merged_slct_cols_$count_enquiry_recency_365_)
summary(all_merged_slct_cols_$count_enquiry_recency_365_)
all_merged_slct_cols_$count_enquiry_recency_365_[is.na(all_merged_slct_cols_$count_enquiry_recency_365_)]<-0

all_merged_slct_cols_$total_enq_amt_<-as.numeric(all_merged_slct_cols_$total_enq_amt_)
summary(all_merged_slct_cols_$total_enq_amt_)
all_merged_slct_cols_$total_enq_amt_[is.na(all_merged_slct_cols_$total_enq_amt_)]<-0

all_merged_slct_cols_$mean_enq_amt_<-as.numeric(all_merged_slct_cols_$mean_enq_amt_)
all_merged_slct_cols_$mean_enq_amt_[is.na(all_merged_slct_cols_$mean_enq_amt_)]<-0

all_merged_slct_cols_$sd_enq_amt_<-as.numeric(all_merged_slct_cols_$sd_enq_amt_)
all_merged_slct_cols_$sd_enq_amt_[is.na(all_merged_slct_cols_$sd_enq_amt_)]<-0

all_merged_slct_cols_$mean_diff_open_enquiry_dt_<-as.numeric(all_merged_slct_cols_$mean_diff_open_enquiry_dt_)
all_merged_slct_cols_$mean_diff_open_enquiry_dt_[is.na(all_merged_slct_cols_$mean_diff_open_enquiry_dt_)]<-0

all_merged_slct_cols_$max_freq_enquiry<-as.numeric(all_merged_slct_cols_$max_freq_enquiry)
all_merged_slct_cols_$max_freq_enquiry[is.na(all_merged_slct_cols_$max_freq_enquiry)]<-0

all_merged_slct_cols_f<-all_merged_slct_cols_[-c(14,15,17,20,33,43)]
write.csv(all_merged_slct_cols_f,"E:/Tooks/RBL_70_30/processed_data.csv",row.names=FALSE)

library(caret)
dummies <- dummyVars(bad_flag_worst6~card_name+promo_code+reject_reason_code+intl_trn+
                                      fee_code+override_fee_code+lead_code+mob_verified+edu_qualification
                                      +app_res_city+res_type+permanent_same+company_type+industry_type+office_city+
                                      id_proof+existing_bank+app_has_card+existing_card_issuer+
                                      app_existing_other_loan_cc+app_existing_rbl_cust+app_international_transaction_fl
                                      +app_si_ecs_flg+app_coupon_code+app_resident_india+app_pref_mailing_addr+
                                      app_go_green+override_fee_binary+app_fcu_check_status, data = all_merged_slct_cols_f, fullRank=T)
d_data <- data.frame(predict(dummies, newdata = all_merged_slct_cols_f))

all_merged_slct_cols_ff<-cbind(all_merged_slct_cols_f,d_data)

all_merged_slct_cols_ff<-all_merged_slct_cols_ff[-c(2,3,6,9,10,11,12,14,16,19,20,23,24,27,28,32,33,34,35,36,40,41,42,43,44,45,52,53,57,100)]
ncol(all_merged_slct_cols_ff)

write.csv(all_merged_slct_cols_ff,"E:/Tooks/RBL_70_30/processed_dummified.csv",row.names=FALSE)

train_set<-all_merged_slct_cols_ff[all_merged_slct_cols_ff$data_part=="Train",]
test_set<-all_merged_slct_cols_ff[all_merged_slct_cols_ff$data_part=="Test",]

train_set<-train_set[-c(72)]
test_set<-test_set[-c(72)]
##removed data partition label

