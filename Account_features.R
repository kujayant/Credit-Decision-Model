library(plyr)
raw_account_70<-read.csv("E:/Tooks/RBL_70_30/raw_account_70.csv",header=T,stringsAsFactors = FALSE)
raw_account_30<-read.csv("E:/Tooks/RBL_70_30/raw_account_30.csv",header=T,stringsAsFactors = FALSE)
## To summarize raw account data and create features out of it.


### Function to count ownership indicator
owner_1<-function(x){
  return(length(x$owner_indic[x$owner_indic==1]))
}

owner_2<-function(x){
  return(length(x$owner_indic[x$owner_indic==2]))
}

owner_3<-function(x){
  return(length(x$owner_indic[x$owner_indic==3]))
}

owner_4<-function(x){
  return(length(x$owner_indic[x$owner_indic==4]))
}

##Total duration between last payment date and account open date
class(raw_account_70$last_paymt_dt)
class(raw_account_30$last_paymt_dt)
class(raw_account_70$opened_dt)
class(raw_account_30$opened_dt)
raw_account_70$last_paymt_dt<-as.Date(raw_account_70$last_paymt_dt,format="%d-%b-%y")
raw_account_30$last_paymt_dt<-as.Date(raw_account_30$last_paymt_dt,format="%d-%b-%y")
raw_account_70$opened_dt<-as.Date(raw_account_70$opened_dt,format="%d-%b-%y")
raw_account_30$opened_dt<-as.Date(raw_account_30$opened_dt,format="%d-%b-%y")

library(lubridate)

total_diff_lastpaymt_opened_dt<-function(x) {
  return(sum(as.numeric(difftime(x$last_paymt_dt,x$opened_dt,unit="days"),na.rm=TRUE)))
}

mean_diff_lastpaymt_opened_dt<-function(x) {
  return(mean(as.numeric(difftime(x$last_paymt_dt,x$opened_dt,unit="days"),na.rm=TRUE)))
}

sd_diff_lastpaymt_opened_dt<-function(x) {
  return(sd(as.numeric(difftime(x$last_paymt_dt,x$opened_dt,unit="days"),na.rm=TRUE)))
}

## Credit Limit Features

sum(is.na(raw_account_70$high_credit_amt))

sum(is.na(raw_account_30$high_credit_amt))

summary(raw_account_70$high_credit_amt)

summary(raw_account_30$high_credit_amt)

plot(hist(raw_account_70$high_credit_amt,na.rm=TRUE))
plot(hist(raw_account_30$high_credit_amt,na.rm=TRUE))
##median imputation in place of NA
raw_account_70$high_credit_amt[is.na(raw_account_70$high_credit_amt)]<-45220
raw_account_30$high_credit_amt[is.na(raw_account_30$high_credit_amt)]<-45220

total_high_credit_amt<-function(x) {
  return(sum(x$high_credit_amt))
}

mean_high_credit_amt<-function(x) {
  return(mean(x$high_credit_amt))
}

sd_high_credit_amt<-function(x) {
  return(sd(x$high_credit_amt))
}

####Current Balance Features
sum(is.na(raw_account_70$cur_balance_amt))
sum(is.na(raw_account_30$cur_balance_amt))

total_cur_balance_amt<-function(x) {
  return(sum(x$cur_balance_amt))
}

mean_cur_balance_amt<-function(x) {
  return(mean(x$cur_balance_amt))
}

sd_cur_balance_amt<-function(x) {
  return(sd(x$cur_balance_amt))
}

##Ratio_currbalance_creditlimit
Ratio_currbalance_creditlimit<-function(x){
  return(sum(x$cur_balance_amt)/sum(x$high_credit_amt))
}

##Utilisation trend

utilisation_trend<-function(x){
  s<-sum(x$cur_balance_amt)/sum(x$high_credit_amt)
  m_cashlimit<-mean(x$cashlimit,na.rm=TRUE)
  m<-mean(x$cur_balance_amt)/(mean(x$high_credit_amt)+m_cashlimit)
  return((s/m))
}

##written off or settled account

num_writ_off_acc<-function(x){
  return(sum(!is.na(x$writtenoffandsettled)))
}

types_of_writ_off<-function(x){
  return(length(unique(x$writtenoffandsettled[!is.na(x$writtenoffandsettled)])))
}

##Collateral features
collateral_1<-function(x){
  return(length(x$typeofcollateral[x$typeofcollateral==1]))
}

collateral_2<-function(x){
  return(length(x$typeofcollateral[x$typeofcollateral==2]))
}

collateral_3<-function(x){
  return(length(x$typeofcollateral[x$typeofcollateral==3]))
}

collateral_4<-function(x){
  return(length(x$typeofcollateral[x$typeofcollateral==4]))
}

collateral_0<-function(x){
  return(length(x$typeofcollateral[x$typeofcollateral==0 & !is.na(x$typeofcollateral)]))
}

total_valueofcollateral<-function(x) {
  return(sum(x$valueofcollateral,na.rm=TRUE))
}

mean_valueofcollateral<-function(x) {
  return(mean(x$valueofcollateral,na.rm=TRUE))
}

sd_valueofcollateral<-function(x) {
  return(sd(x$valueofcollateral,na.rm=TRUE))
}

raw_account_70$repaymenttenure<-as.numeric(raw_account_70$repaymenttenure)
raw_account_30$repaymenttenure<-as.numeric(raw_account_30$repaymenttenure)

avg_repayment_tenure<-function(x){
  return(mean(x$repaymenttenure,na.rm=TRUE))
}

total_writtn_off_amt<-function(x){
  return(sum(x$writtenoffamounttotal,na.rm=TRUE))
}

total_writtn_off_principal<-function(x){
  return(sum(x$writtenoffamountprincipal,na.rm=TRUE))
}

total_settlement_amt<-function(x){
  return(sum(x$settlementamount,na.rm=TRUE))
}

count_RBL_member<-function(x){
  rbl<-c("RATNAKAR BANK","RBL BANK","RBL","RBL Bank")
  return(length(x$member_short_name[x$member_short_name %in% rbl]))
}

count_other_member<-function(x){
  other<-c("NOT DISCLOSED")
  return(length(x$member_short_name[x$member_short_name %in% other]))
}

###Payment History Features
###From the documentation it feels that 3 characters from left form payment history of a month.
payment_history_count_STD_months<-function(x){
  master_count<-rep(0,times=nrow(x))
  for(i in 1:nrow(x)){
    hist_vect<-rep("",times=((nchar(x$paymenthistory1[i])-6)/3))
    start_index=0
    for(j in 1:(nchar(x$paymenthistory1[i])-6)){
      
      if(start_index==0){
        sub_index_start=j+3
        sub_index_end<-j+5
      } else {
        sub_index_start=start_index
        sub_index_end<-start_index+2
      }
      
      start_index=sub_index_end+1
      hist_vect[j]<-substr(x$paymenthistory1[i],sub_index_start,sub_index_end)
      
    }
    cut_off<-as.numeric(which(hist_vect=="\"\"\"",arr.ind=TRUE))
    cut_off1<-cut_off-1
    print(cut_off)
    print(cut_off1)
    new_vect<-hist_vect[c(1:cut_off1)]
    master_count[i]<-length(which(new_vect=="000" |new_vect=="STD", arr.ind=TRUE))
  }
  return(sum(master_count))
}

##
#hist_vect<-rep("",times=((nchar(raw_account_70$paymenthistory1[4711])-6)/3))
#start_index=0
#for(j in 1:(nchar(raw_account_70$paymenthistory1[4711])-6)){
#  
#  if(start_index==0){
#    sub_index_start=j+3
#    sub_index_end<-j+5
#  } else {
#    sub_index_start=start_index
#    sub_index_end<-start_index+2
#  }
#  
#  start_index=sub_index_end+1
#  hist_vect[j]<-substr(raw_account_70$paymenthistory1[4711],sub_index_start,sub_index_end)
#  
#}
#cut_off<-as.numeric(which(hist_vect=="\"\"\"",arr.ind=TRUE))
#cut_off1<-cut_off-1
#print(cut_off)
#print(cut_off1)
#new_vect<-hist_vect[c(1:cut_off1)]
##

payment_history_mean_length<-function(x){
  l1<-nchar(x$paymenthistory1)-6
  l2<-nchar(x$paymenthistory2)-6
  ll<-(l1+l2)/3
  return(mean(ll,na.rm=TRUE))
}



account_70<-ddply(raw_account_70,.(externalid),function(x) 
                            c(owner_1=owner_1(x),
                              owner_2=owner_2(x),
                              owner_3=owner_3(x),
                              owner_4=owner_4(x),
                              total_diff_lastpaymt_opened_dt=total_diff_lastpaymt_opened_dt(x),
                              mean_diff_lastpaymt_opened_dt=mean_diff_lastpaymt_opened_dt(x),
                              sd_diff_lastpaymt_opened_dt=sd_diff_lastpaymt_opened_dt(x),
                              total_high_credit_amt=total_high_credit_amt(x),
                              mean_high_credit_amt=mean_high_credit_amt(x),
                              sd_high_credit_amt=sd_high_credit_amt(x),
                              total_cur_balance_amt=total_cur_balance_amt(x),
                              mean_cur_balance_amt=mean_cur_balance_amt(x),
                              sd_cur_balance_amt=sd_cur_balance_amt(x),
                              Ratio_currbalance_creditlimit=Ratio_currbalance_creditlimit(x),
                              utilisation_trend=utilisation_trend(x),
                              num_writ_off_acc=num_writ_off_acc(x),
                              types_of_writ_off=types_of_writ_off(x),
                              collateral_1=collateral_1(x),
                              collateral_2=collateral_2(x),
                              collateral_3=collateral_3(x),
                              collateral_4=collateral_4(x),
                              collateral_0=collateral_0(x),
                              total_valueofcollateral=total_valueofcollateral(x),
                              mean_valueofcollateral=mean_valueofcollateral(x),
                              sd_valueofcollateral=sd_valueofcollateral(x),
                              avg_repayment_tenure=avg_repayment_tenure(x),
                              total_writtn_off_amt=total_writtn_off_amt(x),
                              total_writtn_off_principal=total_writtn_off_principal(x),
                              total_settlement_amt=total_settlement_amt(x),
                              count_RBL_member=count_RBL_member(x),
                              count_other_member=count_other_member(x),
                              payment_history_count_STD_months=payment_history_count_STD_months(x),
                              payment_history_mean_length=payment_history_mean_length(x)))


account_30<-ddply(raw_account_30,.(externalid),function(x) 
  c(owner_1=owner_1(x),
    owner_2=owner_2(x),
    owner_3=owner_3(x),
    owner_4=owner_4(x),
    total_diff_lastpaymt_opened_dt=total_diff_lastpaymt_opened_dt(x),
    mean_diff_lastpaymt_opened_dt=mean_diff_lastpaymt_opened_dt(x),
    sd_diff_lastpaymt_opened_dt=sd_diff_lastpaymt_opened_dt(x),
    total_high_credit_amt=total_high_credit_amt(x),
    mean_high_credit_amt=mean_high_credit_amt(x),
    sd_high_credit_amt=sd_high_credit_amt(x),
    total_cur_balance_amt=total_cur_balance_amt(x),
    mean_cur_balance_amt=mean_cur_balance_amt(x),
    sd_cur_balance_amt=sd_cur_balance_amt(x),
    Ratio_currbalance_creditlimit=Ratio_currbalance_creditlimit(x),
    utilisation_trend=utilisation_trend(x),
    num_writ_off_acc=num_writ_off_acc(x),
    types_of_writ_off=types_of_writ_off(x),
    collateral_1=collateral_1(x),
    collateral_2=collateral_2(x),
    collateral_3=collateral_3(x),
    collateral_4=collateral_4(x),
    collateral_0=collateral_0(x),
    total_valueofcollateral=total_valueofcollateral(x),
    mean_valueofcollateral=mean_valueofcollateral(x),
    sd_valueofcollateral=sd_valueofcollateral(x),
    avg_repayment_tenure=avg_repayment_tenure(x),
    total_writtn_off_amt=total_writtn_off_amt(x),
    total_writtn_off_principal=total_writtn_off_principal(x),
    total_settlement_amt=total_settlement_amt(x),
    count_RBL_member=count_RBL_member(x),
    count_other_member=count_other_member(x),
    payment_history_count_STD_months=payment_history_count_STD_months(x),
    payment_history_mean_length=payment_history_mean_length(x)))

