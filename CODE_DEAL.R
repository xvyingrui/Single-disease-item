

#导入数据

data1=fread("H://data/ybsingled/11_17dataset/new1.csv",sep = ",",na.strings = "")

data2=fread("H://data/ybsingled/11_17dataset/data2.csv",sep = ",",na.strings = "")


#合并数据
data1.1=data1%>%rename(disease1=DISEASE,type=shushi)%>%select(-totalfee1,-operfee1,-VAR57)

data2.1=data2%>%select(-DISEASE)


data12=rbind(data1.1,data2.1)

#数据的日期处理
data12$time=format(as.Date(data12$PAYTIME),format="%Y-%m-%d")

data12$year=substr(data12$time,1,4)

data12$month=substr(data12$time,6,7)



#贴现至2017，贴现率为0.02

tx=function(a,b,n=2017,c=0.02){
  a=a*(1+c)^(n-b)
  return(a)
}

data12$year=as.numeric(data12$year)

data12[,17:34]=tx(data12[,TOTALFEE:SPEEXFEE],data12$year)



operfee=data12%>%
  group_by(year,month)%>%
  summarise(sum_operfee=sum(OPERFEE),number=n())%>%
  mutate(sin_operfee=sum_operfee/number)


##按照年月份计算例均手术费、例均总费用

  

##按照年月份计算例均医保报销费、例均报销费用

ybpay=data12%>%
  group_by(year,month)%>%
  summarise(sum_ybpay=sum(VAR55),number=n())%>%
  mutate(sin_ybpay=sum_ybpay/number)%>%
  filter(!year==2011)%>%
  filter(number>2)

#时间序列分析ARIMA模型拟合

#1、生成时序
library(forecast)

ts1=ts(ybpay$sum_ybpay,start = c(2012,1),frequency = 12)


fit=auto.arima(ts1)

fit

plot(forecast(fit,15),xlab = "year",ylab = "number")


#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。
#################################################
####################################################
#2017年6-8月份数据

library(readxl)

X678_qc_0926 <- read_excel("H:/data/ybsingled/6_8dataset/678_qc_0926.xlsx")

X6789qc1016 <- read_excel("H:/data/ybsingled/6789qc1016.xlsx")			

X678_qc_0926=X6789qc1016
#数据的日期处理
X678_qc_0926$time=format(as.Date(X678_qc_0926$PAYTIME),format="%Y-%m-%d")

X678_qc_0926$year=substr(X678_qc_0926$time,1,4)

X678_qc_0926$month=substr(X678_qc_0926$time,6,7)

#按照原先报销政策下的医保支付费用
##按照年月份计算例均医保报销费、例均报销费用

ybpay1=X678_qc_0926%>%
  group_by(year,month)%>%
  summarise(sum_ybpay=sum(TOTALFEE),number=n())%>%
  mutate(sin_ybpay=sum_ybpay/number)%>%
  filter(!year==2011)%>%
  filter(number>2)

#时间序列分析ARIMA模型拟合

#1、生成时序
library(forecast)

ts2=ts(ybpay1$sum_ybpay,start = c(2017,6),frequency = 12)


fit=auto.arima(ts2)

fit

plot(forecast(fit,3),xlab = "year",ylab = "number")



























































