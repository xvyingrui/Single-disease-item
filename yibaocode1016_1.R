
##########单病种数据6789。。。。。。。。。。。20171016


x1_6=fread("h:/data/ybsingled/yibaoshuju_6月/1_xg.csv",sep=",")
x1_7=fread("h:/data/ybsingled/yibaoshuju_7月/1_xg.csv",sep=",")
x1_8=fread("h:/data/ybsingled/2017年8月数据/TJ_0613_单病种结果0926.csv",sep=",")
x1_9=fread("h:/data/ybsingled/按病种2017年9月数据/0559单病种结果.csv",sep=",")




x1_67=rbind(x1_6,x1_7,x1_8,x1_9 )

x1_678_qc=x1_67%>%group_by(就医序列号,ICD10,ID,OUTDATE,TOTALFEE,INDATE)%>%
  filter(row_number()==1)%>%
  ungroup()

write.table(x1_678_qc,file = "6789qc1016.csv",sep = ",",row.names = F)

library(readxl)
x1_6789 <- read_excel("H:/data/ybsingled/6789qc1016.xlsx")

#。。。。。。。。。。。。。。。。。。。。。。。。。。。。

x1_67_qc1=x1_6789

x1_67_qc1$HOSGRADE=as.character(x1_67_qc1$HOSGRADE)


x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="相当三级"]<-"三级"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="二级"]<-"1-2级"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="一级"]<-"1-2级"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="相当一级"]<-"1-2级"



#日期处理为月-日

bzss=x1_67_qc1

bzss$time=format(as.Date(bzss$PAYTIME),format="%m-%d")

bzss$month=substr(bzss$time,1,2)

##..........................................................................,,.......指单病种付费

bzss1=bzss%>%filter(待遇类别=="按病种付费")

#三级医院按照标准付费
result1=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="三级")%>%
  filter(TOTALFEE>=三级医院底限)%>%
  filter(TOTALFEE<=三级医院高限)%>%
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(例数=n(),三级医院标准支付总额=sum(三级医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=三级医院标准支付总额*记账比率)%>%
  mutate(付费类别="按病种付费")


#三级医院按原先报销方式付费

result2=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="三级")%>%
  filter(TOTALFEE<三级医院底限|TOTALFEE>三级医院高限)%>%
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(例数=n(),三级医院标准支付总额=sum(三级医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=实际记账金额)%>%
  mutate(付费类别="按原先报销方式付费")


result=rbind(result1,result2)


#其他医院按标准付费

result3=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="1-2级")%>%
  filter(TOTALFEE>=其他医院底限)%>%
  filter(TOTALFEE<=其他医院高限)%>%
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(例数=n(),其他医院标准支付总额=sum(其他医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=其他医院标准支付总额*记账比率)%>%
  mutate(付费类别="按病种付费")



result4=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="1-2级")%>%
  filter(TOTALFEE<其他医院底限|TOTALFEE>其他医院高限)%>%
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(例数=n(),其他医院标准支付总额=sum(其他医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=实际记账金额)%>%
  mutate(付费类别="按原先报销方式付费")


result_2=rbind(result3,result4)

result=rename(result,支付标准总额=三级医院标准支付总额)

result_2=rename(result_2,支付标准总额=其他医院标准支付总额)

result_12=rbind(result,result_2)%>%mutate(医保实际负担比例=医保支付费用_标准/总费用)

write.table(result_12,file = "按病种付费1016.csv",sep=",",row.names=F)

#..........................................................................,,.......指定手术付费

#按照指定手术付费
bzss2=bzss%>%filter(待遇类别=="指定手术单病种")



#。。。。。。。。。。。。。。。。。。。。。。。。。。。。

x1_67_qc1=bzss2

x1_67_qc1$HOSGRADE=as.character(x1_67_qc1$HOSGRADE)


x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="相当三级"]<-"三级"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="二级"]<-"1-2级"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="一级"]<-"1-2级"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="相当一级"]<-"1-2级"



#日期处理为月-日

bzss2=x1_67_qc1

bzss2$time=format(as.Date(bzss2$PAYTIME),format="%m-%d")

bzss2$month=substr(bzss2$time,1,2)


#三级医院按照标准付费
result1=bzss2%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="三级")%>%
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(例数=n(),三级医院标准支付总额=sum(三级医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=三级医院标准支付总额*记账比率)%>%
  mutate(医保实际负担比例=医保支付费用_标准/总费用)%>%
  rename(支付标准总额=三级医院标准支付总额)


#其他医院按标准付费

result2=bzss2%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="1-2级")%>%
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(例数=n(),其他医院标准支付总额=sum(其他医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=其他医院标准支付总额*记账比率)%>%
  mutate(医保实际负担比例=医保支付费用_标准/总费用)%>%
  rename(支付标准总额=其他医院标准支付总额)

result12=rbind(result1,result2)


write.table(result12,file = "按指定手术付费1016.csv",sep=",",row.names=F)






