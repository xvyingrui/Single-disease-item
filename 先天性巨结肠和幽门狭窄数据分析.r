

library(readxl)

#导入excel数据
xiantian <- read_excel("E:/data/xiantian.xlsx")

#将原表中的变量名重命名，方便识别和处理（TOTALEE=本次就诊总费用，PAYTIME为支付时间，disease515为病种名称，STATUS为医保类型，HOSGRADE为医院等级）
xiantian=rename(xiantian,TOTALEE=var38,PAYTIME=var27,disease515=var34,HOSGRADE=var17,STATUS=var11)

#选取需要处理的变量作为子集
xiantian1=select(xiantian,TOTALEE,PAYTIME,disease515,HOSGRADE,STATUS)

#将缺失值编码为数字0
xiantian1[is.na(xiantian1)]<-0


#处理日期PAYTIME为year（只有年份的类型）
xiantian1=mutate(xiantian1,year=substr(PAYTIME,1,4))

                 
               
#将医院等级的编码统一
xiantian1$HOSGRADE[xiantian1$HOSGRADE=="三级"]<-"3级"
xiantian1$HOSGRADE[xiantian1$HOSGRADE=="三级甲等"]<-"3级"
xiantian1$HOSGRADE[xiantian1$HOSGRADE=="三级乙等"]<-"3级"
xiantian1$HOSGRADE[xiantian1$HOSGRADE=="二级甲等"]<-"2级"
xiantian1$HOSGRADE[xiantian1$HOSGRADE=="二级"]<-"2级"
xiantian1$HOSGRADE[xiantian1$HOSGRADE==0]<-"等级不明"

#将医保类型的编码统一
xiantian1$STATUS[xiantian1$STATUS=="城乡居民医疗保险"]<-"城乡" 
xiantian1$STATUS[xiantian1$STATUS=="城镇职工医疗保险"]<-"城职"

#统一病种术式
xiantian1$disease515[xiantian1$disease515=="先天性长段型巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性短段型巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性超短段型巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性常见型巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠(长段型)"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠（赫希施斯普龙病）(未分型)"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠(赫希施普隆氏病)"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠(全结肠型)"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠(普通型、常见型)"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性超短段型巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠(短段型)"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性巨结肠类源病"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性全结肠型巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天无神经节性巨结肠"]<-"先天性巨结肠"
xiantian1$disease515[xiantian1$disease515=="先天性肥大性幽门梗阻(狭窄)"]<-"先天性幽门狭窄11"
xiantian1$disease515[xiantian1$disease515=="先天性幽门狭窄"]<-"先天性幽门狭窄11"
xiantian1$disease515[xiantian1$disease515=="先天性肥大性幽门狭窄"]<-"先天性幽门狭窄11"
xiantian1$disease515[xiantian1$disease515=="幽门狭窄"]<-"无"







#贴现2016
#将总费用转换为数值型变量——方便计算
xiantian1$TOTALEE=as.numeric(xiantian1$TOTALEE)

xiantian160=xiantian1%>%filter(year==2012)%>%mutate(TOTAL1=TOTALEE*((1+0.02)^4))%>%select(-TOTALEE,-PAYTIME)
xiantian161=xiantian1%>%filter(year==2013)%>%mutate(TOTAL1=TOTALEE*((1+0.02)^3))%>%select(-TOTALEE,-PAYTIME)
xiantian162=xiantian1%>%filter(year==2014)%>%mutate(TOTAL1=TOTALEE*((1+0.02)^2))%>%select(-TOTALEE,-PAYTIME)
xiantian163=xiantian1%>%filter(year==2015)%>%mutate(TOTAL1=TOTALEE*((1+0.02)^1))%>%select(-TOTALEE,-PAYTIME)
xiantian164=xiantian1%>%filter(year==2016)%>%mutate(TOTAL1=TOTALEE*((1+0.02)^0))%>%select(-TOTALEE,-PAYTIME)

xiantian16=rbind(xiantian160,xiantian161,xiantian162,
                  xiantian163,xiantian164)
xiantian16=rename(xiantian16,TOTAL=TOTAL1)


#筛选病种名相符合的病种
xiantian22=filter(xiantian16,disease515=="先天性巨结肠"|disease515=="先天性幽门狭窄11")

#分组计算（按照病种和医院等级进行分组，计算均值，标准差，最小值，P5，P25，P50，P75，P95，最大值）
xt1=xiantian22%>%group_by(disease515,HOSGRADE)%>%
  summarise(例数=n(),费用均值=mean(TOTAL),标准差=sd(TOTAL),最小值=min(TOTAL),
                P5=quantile(TOTAL,probs=c(0.05)),P25=quantile(TOTAL,probs=c(0.25)),
                P50=quantile(TOTAL,probs=c(0.50)),P75=quantile(TOTAL,probs=c(0.75)),
                P95=quantile(TOTAL,probs=c(0.95)),最大值=max(TOTAL))

#选取医院等级为2级的子集且病种相符合的子集进行掐头去尾

xiantian121=xiantian22%>%filter(disease515=="先天性巨结肠"&HOSGRADE=="2级"&TOTAL>=quantile(TOTAL,probs = c(0.05))&TOTAL<=quantile(TOTAL,probs = c(0.95)))
xiantian122=xiantian22%>%filter(disease515=="先天性幽门狭窄11"&HOSGRADE=="2级"&TOTAL>=quantile(TOTAL,probs = c(0.05))&TOTAL<=quantile(TOTAL,probs = c(0.95)))



xiantian1223=rbind(xiantian121,xiantian122)

#选取医院等级为3级和病种相符合的子集进行掐头去尾


xiantian121=xiantian22%>%filter(disease515=="先天性巨结肠"&HOSGRADE=="3级"&TOTAL>=quantile(TOTAL,probs = c(0.05))&TOTAL<=quantile(TOTAL,probs = c(0.95)))
xiantian122=xiantian22%>%filter(disease515=="先天性幽门狭窄11"&HOSGRADE=="3级"&TOTAL>=quantile(TOTAL,probs = c(0.05))&TOTAL<=quantile(TOTAL,probs = c(0.95)))


xiantian1224=rbind(xiantian121,xiantian122)


#选取医院等级为等级不明和病种相符合的子集进行掐头去尾

xiantian121=xiantian22%>%filter(disease515=="先天性巨结肠"&HOSGRADE=="等级不明"&TOTAL>=quantile(TOTAL,probs = c(0.05))&TOTAL<=quantile(TOTAL,probs = c(0.95)))
xiantian122=xiantian22%>%filter(disease515=="先天性幽门狭窄11"&HOSGRADE=="等级不明"&TOTAL>=quantile(TOTAL,probs = c(0.05))&TOTAL<=quantile(TOTAL,probs = c(0.95)))

xiantian1225=rbind(xiantian121,xiantian122)

#合并掐头去尾的子集为一个数据集
xiantian1223=rbind(xiantian1223,xiantian1224,xiantian1225)


#计算掐头去尾后的均值和标准差

xiantian1224=xiantian1223%>%group_by(disease515,HOSGRADE)%>%
  summarise(掐头去尾均值=mean(TOTAL),掐头去尾标准差=sd(TOTAL))

#把均值和标准差和掐头去尾的均值标准差数据合并
xt=left_join(xt1,xiantian1224,by=c("disease515","HOSGRADE"))

xt1=mutate(xt,P5除掐头去尾均值=P5/掐头去尾均值,P95除掐头去尾均值=P95/掐头去尾均值,cankao=掐头去尾均值*(1+0.02))


write.table(xt1,file = "先天性.csv",sep = ",")

