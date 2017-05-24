library(lubridate)

raw_enquiry_70<-read.csv("E:/Tooks/RBL_70_30/raw_enquiry_70.csv",header=T,stringsAsFactors = FALSE)
raw_enquiry_70<-read.csv("E:/Tooks/RBL_70_30/raw_enquiry_30.csv",header=T,stringsAsFactors = FALSE)


class(raw_enquiry_70$enquiry_dt)
class(raw_enquiry_30$enquiry_dt)


raw_enquiry_70$enquiry_dt<-as.Date(raw_enquiry_70$enquiry_dt,format="%d-%b-%y")
raw_enquiry_30$enquiry_dt<-as.Date(raw_enquiry_30$enquiry_dt,format="%d-%b-%y")

##90 days recency enquiry from the last enquiry made
count_enquiry_recency_90<-function(x){
  order_dates<-rev(sort(x$enquiry_dt))
  for_diff_date<-rep(order_dates[1],times=length(order_dates))
  diff_in_days<-as.numeric(difftime(for_diff_date,order_dates,units="days"))
  return(length(diff_in_days[diff_in_days<=90]))
}

##365 days recency enquiry from the last enquiry made
count_enquiry_recency_365<-function(x){
  order_dates<-rev(sort(x$enquiry_dt))
  for_diff_date<-rep(order_dates[1],times=length(order_dates))
  diff_in_days<-as.numeric(difftime(for_diff_date,order_dates,units="days"))
  return(length(diff_in_days[diff_in_days<=365]))
}

##total,mean and std dev enq_amt
total_enq_amt<-function(x){
  return(sum(as.numeric(x$enq_amt),na.rm=TRUE))
}

mean_enq_amt<-function(x){
  return(mean(as.numeric(x$enq_amt),na.rm=TRUE))
}

sd_enq_amt<-function(x){
  return(sd(as.numeric(x$enq_amt),na.rm=TRUE))
}

##mean_diff_open_enquiry_dt
class(raw_enquiry_70$dt_opened)
class(raw_enquiry_30$dt_opened)

raw_enquiry_70$dt_opened<-as.Date(raw_enquiry_70$dt_opened,format="%d-%b-%y")
raw_enquiry_30$dt_opened<-as.Date(raw_enquiry_30$dt_opened,format="%d-%b-%y")

mean_diff_open_enquiry_dt<-function(x){
  diff_enq<-abs(as.numeric(difftime(x$enquiry_dt,x$dt_opened,units="days")))
  return(mean(diff_enq))
}

##max_freq_enquiry
max_freq_enquiry<-function(x){
  freq_table<-as.data.frame(table(x$enq_purpose))
  freq_table<-freq_table[order(freq_table$Freq,na.last=TRUE,decreasing = TRUE),]
  return(as.character(freq_table$Var1[1]))
}

#raw_enquiry_70$externalid<-ifelse(raw_enquiry_70$externalid=="",paste("#",raw_enquiry_70$apprefno),raw_enquiry_70$externalid)

library(plyr)

sum(is.na(raw_enquiry_70$externalid))

raw_enquiry_70_<-raw_enquiry_70[nchar(raw_enquiry_70$externalid)>1,]

raw_enquiry_30_<-raw_enquiry_30[nchar(raw_enquiry_30$externalid)>1,]

enq_70<-ddply(raw_enquiry_70_,.(externalid),function(x) c(
  count_enquiry_recency_90_=count_enquiry_recency_90(x),
  count_enquiry_recency_365_=count_enquiry_recency_365(x),
  total_enq_amt_=total_enq_amt(x),
  mean_enq_amt_=mean_enq_amt(x),
  sd_enq_amt_=sd_enq_amt(x),
  mean_diff_open_enquiry_dt_=mean_diff_open_enquiry_dt(x),
  max_freq_enquiry=max_freq_enquiry(x))
)


enq_30<-ddply(raw_enquiry_30_,.(externalid),function(x) c(
  count_enquiry_recency_90_=count_enquiry_recency_90(x),
  count_enquiry_recency_365_=count_enquiry_recency_365(x),
  total_enq_amt_=total_enq_amt(x),
  mean_enq_amt_=mean_enq_amt(x),
  sd_enq_amt_=sd_enq_amt(x),
  mean_diff_open_enquiry_dt_=mean_diff_open_enquiry_dt(x),
  max_freq_enquiry=max_freq_enquiry(x))
)

