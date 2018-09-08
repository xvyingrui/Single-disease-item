

#按照医疗机构编码，年月进行分组汇总、合并数据。。。。。。。。。。。。。。。。。。。。。。。。表1
library(data.table)
library(tidyverse)


data1=fread("C:/Users/xuyingrui/Desktop/11/1707.csv",sep = ",")



data2=data1%>%filter(医疗机构名称=="")     ###data2为分割数据


data3=full_join(data1,data2,by=c("医疗机构编码","年月"))



data4=data3%>%filter(人次.y!="NA")%>%mutate(人次.x=人次.x+人次.y,
                                            定额人次.x=定额人次.x+定额人次.y,
                                            人数.x=人数.x+人数.y,
                                            大额.x=大额.x+大额.y,
                                            本次就诊总费用.x=本次就诊总费用.x+本次就诊总费用.y,
                                            总记账金额.x=总记账金额.x+总记账金额.y,
                                            基本医疗费用.x=基本医疗费用.x+基本医疗费用.y,
                                            清算基本医疗费用.x=清算基本医疗费用.x+清算基本医疗费用.y)


data4.1=data3[is.na(data3$人次.y),]
data5=rbind(data4,data4.1)                     
data5=data5[,-c(17:30)]
data5$year=substr(data5$年月,5,6)

data5.1=data5%>%filter(year==15|year==16|year==17)%>%filter(医疗机构名称.x!="")     

write.table(data5.1,file = "城乡汇总数据1707取下半月加聚类加季度（加城居生育）_处理1（医疗机构—年月）.csv",sep=",",row.names=F)


data5.1_zh=data5.1%>%select(年月,人数.x,医疗机构编码)%>%spread(key = 医疗机构编码,value = 人数.x)


write.table(data5.1_zh,file = "城乡汇总数据1707取下半月加聚类加季度（加城居生育）_年月_医疗机构_人数.csv",sep=",",row.names=F)



#.......................................................医院机构编码和年季分组


#按照医疗机构编码，年季进行分组汇总、合并数据。。。。。。。。。。。。。。。。。。。。。。表2-年季度数据
#data5.2为。。。。。。。。。。。。年季+机构+汇总数据
#data5.2_zh为。。。。。。。。。。。。年季+机构编码+人数的数据


data5.2=data5.1%>%group_by(医疗机构编码,年季.x)%>%summarise(人次=sum(人次.x),
                                              定额人次=sum(定额人次.x),
                                              人数=sum(人数.x),
                                              大额=sum(大额.x),
                                              本次就诊总费用=sum(本次就诊总费用.x),
                                              总记账金额=sum(总记账金额.x),
                                              基本医疗费用=sum(基本医疗费用.x),
                                              清算基本医疗费用=sum(清算基本医疗费用.x))


write.table(data5.2,file = "城乡汇总数据1707取下半月加聚类加季度（加城居生育）_处理2（医疗机构—年季）.csv",sep=",",row.names=F)


data5.2_zh=data5.2%>%select(年季.x,人数,医疗机构编码)%>%spread(key = 医疗机构编码,value = 人数)


write.table(data5.2_zh,file = "城乡汇总数据1707取下半月加聚类加季度（加城居生育）_年季_医疗机构_人数.csv",sep=",",row.names=F)




#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。年月+人数+机构编码
#。。。。。。。。。。年月格式处理

x1=data5.1_zh

data5.1_zh$year=substr(data5.1_zh$年月,5,6)
data5.1_zh$month=substr(data5.1_zh$年月,1,3)
data5.1_zh$年季=ifelse(data5.1_zh$month=="Jan",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","01"),
                     ifelse(data5.1_zh$month=="Feb",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","02"),
                            ifelse(data5.1_zh$month=="Mar",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","03"),
                                   ifelse(data5.1_zh$month=="Apr",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","04"),
                                          ifelse(data5.1_zh$month=="May",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","05"),
                                                 ifelse(data5.1_zh$month=="Jun",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","06"),
                                                        ifelse(data5.1_zh$month=="Jul",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","07"),
                                                               ifelse(data5.1_zh$month=="Aug",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","08"),
                                                                      ifelse(data5.1_zh$month=="Sep",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","09"),
                                                                             ifelse(data5.1_zh$month=="Oct",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","10"),
                                                                                    ifelse(data5.1_zh$month=="Nov",data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","11"),
                                                                                           data5.1_zh$time<-paste0("20",data5.1_zh$year,"/","12"))))))))))))
                   



data5.1_zh$年月=data5.1_zh$年季
data5.1_zh=data5.1_zh[,-c(305:308)]



###############################################################################################...
###############################################################.................................年月+人数+时序预测


library(missForest) 
library(tseries)
library(forecast)
setwd('G:/yingyu/定点管理处161024/2017/18年结算标准170719/住院数据170731/2018处理数据（以此为准）/分析数据171030')
renshu=read.csv('人数.csv',head=TRUE)
#renshu1=t(renshu)

renshu=data5.1_zh

renshu2=renshu[,-1]
renshu3=missForest(renshu2,verbose=TRUE)
renshu4=renshu3$ximp #提取修补后的数据
renshu5=t(renshu4)
renshu6=as.data.frame(renshu5)

renshuRE=data.frame('医疗机构编码'=0,'x15.01'=0,'x15.02'=0,'x15.03'=0,
                    'x15.04'=0,'x15.05'=0,'x15.06'=0,'x15.07'=0,'x15.08'=0,'x15.09'=0,
                    'x15.10'=0,'x15.11'=0,'x15.12'=0,'x16.01'=0,'x16.02'=0,'x16.03'=0,
                    'x16.04'=0,'x16.05'=0,'x16.06'=0,'x16.07'=0,'x16.08'=0,'x16.09'=0,
                    'x16.10'=0,'x16.11'=0,'x16.12'=0,'x17.01'=0,'x17.02'=0,'x17.03'=0,
                    'x17.04'=0,'x17.05'=0,'x17.06'=0,'x17.07'=0,'x17.08'=0,'x17.09'=0,
                    'x^17.10'=0,'x^17.11'=0,'x^17.12'=0,'x^18.01'=0,'x^18.02'=0,'x^18.03'=0,
                    'x^18.04'=0,'x^18.05'=0,'x^18.06'=0,'x^18.07'=0,'x^18.08'=0,'x^18.09'=0,
                    'x^18.10'=0,'x^18.11'=0,'x^18.12'=0,'残差检验p值'=0,'均方根误差'=0)
for(i in 1:nrow(renshu6)) {
  data<-renshu6[i,]
  name<-rownames(data)
  rs<-as.numeric(data)
  model<-auto.arima(rs)
  presid0<-Box.test(model$residuals,type='Ljung-Box')
  presid<-presid0$p.value
  pred<-forecast(model,h=15,level=c(99.5))
  err<-accuracy(model)
  REMSE<-err[1,'RMSE']
  renshuRE[i,]=c(name,rs,pred$mean,presid,REMSE)
}
write.csv(renshuRE,'城职2018人数时间序列预测（年月+人数）.csv',row.names=F)



###############################################################################################...
###############################################################.................................年月+人数+时序预测


#data5.2_zh  为年季+机构+人数数据
#。。。。。。。


library(missForest) 
library(tseries)
library(forecast)
setwd('G:/yingyu/定点管理处161024/2017/18年结算标准170719/住院数据170731/2018处理数据（以此为准）/分析数据171030')
renshu=read.csv('人数.csv',head=TRUE)
#renshu1=t(renshu)

renshu=data5.2_zh

renshu2=renshu[,-1]
renshu2=as.data.frame(renshu2)
renshu3=missForest(renshu2,verbose=TRUE)
renshu4=renshu3$ximp #提取修补后的数据
renshu5=t(renshu4)
renshu6=as.data.frame(renshu5)

renshuRE=data.frame('医疗机构编码'=0,'x15.01'=0,'x15.02'=0,'x15.03'=0,
                    'x15.04'=0,'x16.01'=0,'x16.02'=0,'x16.03'=0,
                    'x16.04'=0,'x17.01'=0,'x17.02'=0,'x17.03'=0,
                    'x17.04'=0,'x^18.01'=0,'x^18.02'=0,'x^18.03'=0,
                    'x^18.04'=0,'残差检验p值'=0,'均方根误差'=0)
for(i in 1:nrow(renshu6)) {
  data<-renshu6[i,]
  name<-rownames(data)
  rs<-as.numeric(data)
  model<-auto.arima(rs)
  presid0<-Box.test(model$residuals,type='Ljung-Box')
  presid<-presid0$p.value
  pred<-forecast(model,h=5,level=c(99.5))
  err<-accuracy(model)
  REMSE<-err[1,'RMSE']
  renshuRE[i,]=c(name,rs,pred$mean,presid,REMSE)
}
write.csv(renshuRE,'城职2018人数时间序列预测（年季+人数）.csv',row.names=F)


#》》》》》》》》》》》》》》》》》》。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。人次人头比预测


#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。时序2018年人次人头比计算

#数据：年月-data5.1
#数据：年季-data5.2


data6=data5.1%>%
  group_by(医疗机构编码,年.x)%>%
  summarise(年总人次=sum(人次.x),年总定额人次=sum(定额人次.x),年总人数=sum(人数.x))%>%
  mutate(人次人头比=年总定额人次/年总人数)

data6.1=data6%>%filter(年.x==15)%>%select(医疗机构编码,年.x,人次人头比)%>%rename(人次人头比15年=人次人头比)
data6.2=data6%>%filter(年.x==16)%>%select(医疗机构编码,年.x,人次人头比)%>%rename(人次人头比16年=人次人头比)
data6.3=data6%>%filter(年.x==17)%>%select(医疗机构编码,年.x,人次人头比)%>%rename(人次人头比17年=人次人头比)


data6.4=full_join(data6.1,data6.2,by="医疗机构编码")

data6.5=full_join(data6.4,data6.3,by="医疗机构编码")

data6.5[is.na(data6.5)]<-999

data6.5$人次人头比18年=ifelse(data6.5$人次人头比15年<=data6.5$人次人头比17年,
                        ifelse(data6.5$人次人头比15年<999,data6.5$人次人头比15年,data6.5$人次人头比16年),
                        ifelse(data6.5$人次人头比17年<999,data6.5$人次人头比17年,data6.5$人次人头比16年))


write.table(data6.5,file = "时序定额人次人头比.csv",sep=",",row.names=F)



#》》》》》》》》》》》》》》》》》》。。。。。。。。。。。。。。。。。。。。。。。。。。。。。非时序2018年人次人头比预测


data7=fread("C:/Users/xuyingrui/Desktop/11/2015年-2017年人次人头比计算结果已有.csv",sep = ",")

data7.1=data7%>%filter(年==2015&险种类型=="城乡")%>%select(医疗机构编码,年,年度定额人次人头比)%>%rename(年度定额人次人头比15年=年度定额人次人头比)
data7.2=data7%>%filter(年==2016&险种类型=="城乡")%>%select(医疗机构编码,年,年度定额人次人头比)%>%rename(年度定额人次人头比16年=年度定额人次人头比)
data7.3=data7%>%filter(年==2017&险种类型=="城乡")%>%select(医疗机构编码,年,年度定额人次人头比)%>%rename(年度定额人次人头比17年=年度定额人次人头比)


data7.4=full_join(data7.1,data7.2,by="医疗机构编码")

data7.5=full_join(data7.4,data7.3,by="医疗机构编码")


data7.5[is.na(data7.5)]<-999

data7.5$人次人头比18年=ifelse(data7.5$年度定额人次人头比15年<=data7.5$年度定额人次人头比17年,
                        ifelse(data7.5$年度定额人次人头比15年<999,data7.5$年度定额人次人头比15年,data7.5$年度定额人次人头比16年),
                        ifelse(data7.5$年度定额人次人头比17年<999,data7.5$年度定额人次人头比17年,data7.5$年度定额人次人头比16年))

write.table(data7.5,file = "非时序2018年人次人头比.csv",sep=",",row.names=F)

#data5.1数据。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。进行年份汇总分类

data8=data5.1%>%
  group_by(医疗机构编码,年.x)%>%
  summarise(年总人次=sum(人次.x),年总定额人次=sum(定额人次.x),年总人数=sum(人数.x))%>%
  mutate(人次人头比=年总定额人次/年总人数)


data5.1$year=substr(data5.1$年月,5,6)
data5.1$month=substr(data5.1$年月,1,3)

data5.1$年月份=ifelse(data5.1$month=="Jan",data5.1$time<-paste0("20",data5.1$year,"/","01"),
                  ifelse(data5.1$month=="Feb",data5.1$time<-paste0("20",data5.1$year,"/","02"),
                         ifelse(data5.1$month=="Mar",data5.1$time<-paste0("20",data5.1$year,"/","03"),
                                ifelse(data5.1$month=="Apr",data5.1$time<-paste0("20",data5.1$year,"/","04"),
                                       ifelse(data5.1$month=="May",data5.1$time<-paste0("20",data5.1$year,"/","05"),
                                              ifelse(data5.1$month=="Jun",data5.1$time<-paste0("20",data5.1$year,"/","06"),
                                                     ifelse(data5.1$month=="Jul",data5.1$time<-paste0("20",data5.1$year,"/","07"),
                                                            ifelse(data5.1$month=="Aug",data5.1$time<-paste0("20",data5.1$year,"/","08"),
                                                                   ifelse(data5.1$month=="Sep",data5.1$time<-paste0("20",data5.1$year,"/","09"),
                                                                          ifelse(data5.1$month=="Oct",data5.1$time<-paste0("20",data5.1$year,"/","10"),
                                                                                 ifelse(data5.1$month=="Nov",data5.1$time<-paste0("20",data5.1$year,"/","11"),
                                                                                        data5.1$time<-paste0("20",data5.1$year,"/","12"))))))))))))

data5.1$month=substr(data5.1$年月份,6,7)

data5.1$month=as.numeric(data5.1$month)



data8=data5.1%>%
  filter(month<=9)%>%
  group_by(医疗机构编码,年.x)%>%
  summarise(年总人次=sum(人次.x),年总定额人次=sum(定额人次.x),年总人数=sum(人数.x))%>%
  mutate(人次人头比=年总定额人次/年总人数)


#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。.......2017年预计值计算



data8_12=data5.1%>%
  group_by(医疗机构编码,年.x)%>%
  summarise(年总人次12=sum(人次.x),年总定额人次12=sum(定额人次.x),年总人数12=sum(人数.x))%>%
  mutate(人次人头比12=年总定额人次12/年总人数12)


data8_8_12=full_join(data8,data8_12,by=c("医疗机构编码","年.x"))


#15年人数计算
data8_8_12.1=data8_8_12%>%
  filter(年.x==15)%>%
  select(医疗机构编码,年.x,年总人数,年总人数12)%>%
  mutate(年总人数15_9_12=年总人数/年总人数12)%>%
  select(-年总人数,-年总人数12)
data8_8_12.2=data8_8_12%>%
  filter(年.x==16)%>%
  select(医疗机构编码,年.x,年总人数,年总人数12)%>%
  mutate(年总人数16_9_12=年总人数/年总人数12)%>%
  select(-年总人数,-年总人数12)
data8_8_12.3=data8_8_12%>%
  filter(年.x==17)%>%
  select(医疗机构编码,年.x,年总人数,年总人数12)%>%
  mutate(年总人数17_9_12="未计算")



data8_8_12.4=full_join(data8_8_12.1,data8_8_12.2,by="医疗机构编码")

data8_8_12.5=full_join(data8_8_12.4,data8_8_12.3,by="医疗机构编码")


data17_yj=data8_8_12.5%>%
  mutate(年总人数17_9比值=(年总人数15_9_12+年总人数16_9_12)/2)%>%
  mutate(年总人数预计值17=年总人数/年总人数17_9比值)






############################。。。。。。。。。。。。。。。。计算2018年增长率

data8.1=data8%>%filter(年.x==15)%>%select(医疗机构编码,年.x,年总人数)%>%rename(年总人数15年=年总人数)
data8.2=data8%>%filter(年.x==16)%>%select(医疗机构编码,年.x,年总人数)%>%rename(年总人数16年=年总人数)
data8.3=data8%>%filter(年.x==17)%>%select(医疗机构编码,年.x,年总人数)%>%rename(年总人数17年=年总人数)



data8.4=full_join(data8.1,data8.2,by="医疗机构编码")

data8.5=full_join(data8.4,data8.3,by="医疗机构编码")

data8.5=data8.5%>%
  mutate(增长率15_16=(年总人数16年-年总人数15年)/年总人数15年,增长率16_17=(年总人数17年-年总人数16年)/年总人数16年)%>%
  mutate(年增长率18=0.3*增长率15_16+0.7*增长率16_17)


#..............。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。非时序2018年预计值测算
#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。联合2017预计值+2018年增长率=计算2018年预计值


####data17_yj为2017年预计值表

####data8.5为2018年增长率计算表

data18_yj=full_join(data17_yj,data8.5,by="医疗机构编码")

data18_yj_1=data18_yj%>%
  mutate(年总人数预计值18=年总人数预计值17*(1+年增长率18))%>%
  select(医疗机构编码,年总人数预计值17,年增长率18,年总人数预计值18)

write.table(data18_yj_1,file = "非时序2018年预计值测算.csv",sep=",",row.names=F)


#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。非时序2018年预计值*2018年度人次人头比

#data18_yj_2为2018年预计值测算结果

#data7.5为2018年非时序人次人头比测算结果

library(readxl)
 data18_yj_2 <- read_excel("C:/Users/xuyingrui/Desktop/ybjwj/2018_非时序_预计值_编码处理.xlsx")


data18_yj_rcb=full_join(data18_yj_2,data7.5,by="医疗机构编码")
data18_yj_rcb$年总人数预计值18=as.numeric(data18_yj_rcb$年总人数预计值18)

data18_result1=data18_yj_rcb%>%
  mutate(预计值_年度人次人头比_18=年总人数预计值18*人次人头比18年)%>%
  select(医疗机构编码,年总人数预计值18,人次人头比18年,预计值_年度人次人头比_18)


write.table(data18_result1,file = "非时序2018年预计值_人次人头比.csv",sep=",",row.names=F)

#................................................................................。。非时序计算到此结束





