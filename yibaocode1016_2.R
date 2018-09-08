
#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。1016
#。。。。。。。。。。。。。。。。。。。。。。。。。诊断加术式


bzss=x1_6789

#


bzss$HOSGRADE[bzss$HOSGRADE=="相当三级"]<-"三级"
bzss$HOSGRADE[bzss$HOSGRADE=="二级"]<-"1-2级"
bzss$HOSGRADE[bzss$HOSGRADE=="一级"]<-"1-2级"
bzss$HOSGRADE[bzss$HOSGRADE=="相当一级"]<-"1-2级"



#日期处理为月-日

bzss$time=format(as.Date(bzss$PAYTIME),format="%m-%d")

bzss$month=substr(bzss$time,1,2)

#三级医院按照标准付费
#单病种类型

result1_1=bzss%>%
  filter(待遇类别=="按病种付费")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="三级")%>%
  filter(TOTALFEE>=三级医院底限)%>%
  filter(TOTALFEE<=三级医院高限)%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(例数=n(),三级医院标准支付总额=sum(三级医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=三级医院标准支付总额*记账比率)%>%
  mutate(付费类型="按单病种标准付费")

#按照日间手术付费
result1_2=bzss%>%
  filter(待遇类别=="指定手术单病种")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="三级")%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(例数=n(),三级医院标准支付总额=sum(三级医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=三级医院标准支付总额*记账比率)%>%
  mutate(付费类型="按指定手术单病种标准付费")

result1=rbind(result1_1,result1_2)

#三级医院按原先报销方式付费

result2=bzss%>%
  filter(待遇类别=="按病种付费")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="三级")%>%
  filter(TOTALFEE<三级医院底限|TOTALFEE>三级医院高限)%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(例数=n(),三级医院标准支付总额=sum(三级医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=实际记账金额)%>%
  mutate(付费类型="按原先报销方式付费")


result=rbind(result1,result2)



#其他医院按标准付费
#单病种类型

result3_1=bzss%>%
  filter(待遇类别=="按病种付费")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="1-2级")%>%
  filter(TOTALFEE>=其他医院底限)%>%
  filter(TOTALFEE<=其他医院高限)%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(例数=n(),其他医院标准支付总额=sum(其他医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=其他医院标准支付总额*记账比率)%>%
  mutate(付费类型="按单病种标准付费")

#按照日间手术付费
result3_2=bzss%>%
  filter(待遇类别=="指定手术单病种")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="1-2级")%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(例数=n(),其他医院标准支付总额=sum(其他医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=其他医院标准支付总额*记账比率)%>%
  mutate(付费类型="按指定手术单病种标准付费")

result3=rbind(result3_1,result3_2)

#其他医院按照原先报销方式报销
result4=bzss%>%
  filter(待遇类别=="按病种付费")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(三级医院底限=0.5*三级医院标准,三级医院高限=2*三级医院标准,
               其他医院底限=0.5*其他医院标准,其他医院高限=2*其他医院标准)%>%
  filter(HOSGRADE=="1-2级")%>%
  filter(TOTALFEE<其他医院底限|TOTALFEE>其他医院高限)%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(例数=n(),其他医院标准支付总额=sum(其他医院标准),实际记账金额=sum(总记账金额),总费用=sum(TOTALFEE))%>%
  mutate(记账比率=实际记账金额/总费用)%>%
  mutate(医保支付费用_标准=实际记账金额)%>%
  mutate(付费类型="按原先报销方式付费")



result_2=rbind(result3,result4)


write.table(result_2,file = "其他医院标准1016.csv",sep = ",",row.names = F)

write.table(result,file = "三级医院标准1016.csv",sep = ",",row.names = F)


