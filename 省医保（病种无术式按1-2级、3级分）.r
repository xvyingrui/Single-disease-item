
#省医保（无术式按3级、3级分）

library(ggplot2)
library(lubridate)
library(data.table)
library(dplyr)

#读数据
diyi=fread("di1.csv",sep = ",",na.strings = "")

#去掉重复
diyi513=diyi%>%group_by(ID,OUTDATE,TOTALFEE)%>%filter(row_number()==1)%>%ungroup()


#选子集
diyisx=select(diyi,contains("FEE"),contains("TIME"),HOSGRADE,disease1,type)

#替换缺失值为0

diyisx[is.na(diyisx)]<-0

diyisx2=as.data.frame(diyisx)

#将一级和二级医院编码为1-2级
diyisx2$HOSGRADE[diyisx2$HOSGRADE=="一级"]<-"1-2级"
diyisx2$HOSGRADE[diyisx2$HOSGRADE=="二级"]<-"1-2级"
diyisx2$HOSGRADE[diyisx2$HOSGRADE=="三级"]<-"3级"
diyisx2$HOSGRADE[diyisx2$HOSGRADE==0]<-"等级不明"

#日期处理为年份
diyisx2$PAYTIME=year(diyisx2$PAYTIME)

#贴现率调整_2016

diyisx60=diyisx2%>%filter(PAYTIME==2012)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^4))%>%select(-TOTALFEE,-PAYTIME)
diyisx61=diyisx2%>%filter(PAYTIME==2013)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^3))%>%select(-TOTALFEE,-PAYTIME)
diyisx62=diyisx2%>%filter(PAYTIME==2014)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^2))%>%select(-TOTALFEE,-PAYTIME)
diyisx63=diyisx2%>%filter(PAYTIME==2015)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^1))%>%select(-TOTALFEE,-PAYTIME)
diyisx64=diyisx2%>%filter(PAYTIME==2016)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^0))%>%select(-TOTALFEE,-PAYTIME)

diyisx6=rbind(diyisx60,diyisx61,diyisx62,
                  diyisx63,diyisx64)
diyisx6=rename(diyisx6,TOTALFEE=TOTAL1)




#选取病种相符合的子集
diyisx222=diyisx6%>%filter(disease1=="腹股沟疝"&type!="单侧腹股沟疝修补术"|
disease1=="腹股沟疝"&type!="经腹腔镜单侧腹股沟疝修补术"|
disease1=="下肢静脉曲张"&type!="大隐静脉高位结扎＋剥脱术"|
disease1=="下肢静脉曲张"&type!="大隐静脉腔内激光闭合术"|
disease1=="腰椎间盘突出"&type!="前路腰椎间盘切除人工椎间盘置换术"|
disease1=="腰椎间盘突出"&type!="腰椎间盘摘除术"|
disease1=="肾结石"&type!="经皮肾镜超声碎石取石术"|
disease1=="肾结石"&type!="肾切开取石术"|
disease1=="单侧精索静脉"&type!="精索静脉曲张高位结扎术"|
disease1=="垂体腺瘤"|disease1=="卵巢良性肿瘤"&type!="经腹单侧卵巢囊肿剥除术"|
disease1=="卵巢良性肿瘤"&type!="经腹单侧卵巢切除术"|
disease1=="卵巢良性肿瘤"&type!="经腹腔镜单侧卵巢切除术"|
disease1=="宫颈癌"&type!="经腹广泛性子宫切除术"|
disease1=="宫颈癌"&type!="经腹腔镜根治性全子宫切除+腹膜后淋巴结切除术"|
disease1=="子宫平滑肌瘤"&type!="经阴道全子宫切除术"|
disease1=="子宫平滑肌瘤"&type!="经腹子宫次全切除术"|
disease1=="子宫平滑肌瘤"&type!="腹腔镜联合阴式全子宫切除术"|
disease1=="子宫平滑肌瘤"&type!="经宫腔镜黏膜下肌瘤切除术"|
disease1=="自然临产阴道"|
disease1=="计划性剖宫产"|
disease1=="结肠癌"&type!="根治性结肠癌切除术"|
disease1=="结肠癌"&type!="扩大根治性结肠癌切除术"|
disease1=="胃癌"&type!="胃部分切除术"|
disease1=="胃癌"&type!="根治性近端胃大部切除术"|
disease1=="胃癌"&type!="扩大根治性远端胃大部切除术"|
disease1=="乳腺癌"&type!="乳腺肿物切除术"|
disease1=="腭裂"&type!="腭裂修复术"|
disease1=="急性单纯性阑")


diyisx4=diyisx222%>%group_by(disease1,HOSGRADE)%>%
  summarise(例数=n(),费用均值=mean(TOTALFEE),标准差=sd(TOTALFEE),最小值=min(TOTALFEE),
                P5=quantile(TOTALFEE,probs=c(0.05)),P25=quantile(TOTALFEE,probs=c(0.25)),
                P50=quantile(TOTALFEE,probs=c(0.50)),P75=quantile(TOTALFEE,probs=c(0.75)),
                P95=quantile(TOTALFEE,probs=c(0.95)),最大值=max(TOTALFEE))


#选取医院等级为1-2级的子集
xiaoyu31=diyisx222%>%filter(disease1=="腹股沟疝"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diyisx222%>%filter(disease1=="下肢静脉曲张"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diyisx222%>%filter(disease1=="腰椎间盘突出"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diyisx222%>%filter(disease1=="肾结石"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diyisx222%>%filter(disease1=="单侧精索静脉"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diyisx222%>%filter(disease1=="垂体腺瘤"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diyisx222%>%filter(disease1=="卵巢良性肿瘤"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diyisx222%>%filter(disease1=="宫颈癌"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diyisx222%>%filter(disease1=="子宫平滑肌瘤"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diyisx222%>%filter(disease1=="自然临产阴道"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diyisx222%>%filter(disease1=="计划性剖宫产"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diyisx222%>%filter(disease1=="结肠癌"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diyisx222%>%filter(disease1=="胃癌"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diyisx222%>%filter(disease1=="乳腺癌"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diyisx222%>%filter(disease1=="腭裂"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diyisx222%>%filter(disease1=="急性单纯性阑"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))


#融合数据
diyisx223=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316)


#选取医院等级为3级子集
xiaoyu31=diyisx222%>%filter(disease1=="腹股沟疝"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diyisx222%>%filter(disease1=="下肢静脉曲张"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diyisx222%>%filter(disease1=="腰椎间盘突出"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diyisx222%>%filter(disease1=="肾结石"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diyisx222%>%filter(disease1=="单侧精索静脉"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diyisx222%>%filter(disease1=="垂体腺瘤"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diyisx222%>%filter(disease1=="卵巢良性肿瘤"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diyisx222%>%filter(disease1=="宫颈癌"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diyisx222%>%filter(disease1=="子宫平滑肌瘤"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diyisx222%>%filter(disease1=="自然临产阴道"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diyisx222%>%filter(disease1=="计划性剖宫产"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diyisx222%>%filter(disease1=="结肠癌"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diyisx222%>%filter(disease1=="胃癌"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diyisx222%>%filter(disease1=="乳腺癌"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diyisx222%>%filter(disease1=="腭裂"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diyisx222%>%filter(disease1=="急性单纯性阑"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))


#融合数据
diyisx224=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316)





#选取医院等级为等级不明的子集
xiaoyu31=diyisx222%>%filter(disease1=="腹股沟疝"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diyisx222%>%filter(disease1=="下肢静脉曲张"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diyisx222%>%filter(disease1=="腰椎间盘突出"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diyisx222%>%filter(disease1=="肾结石"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diyisx222%>%filter(disease1=="单侧精索静脉"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diyisx222%>%filter(disease1=="垂体腺瘤"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diyisx222%>%filter(disease1=="卵巢良性肿瘤"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diyisx222%>%filter(disease1=="宫颈癌"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diyisx222%>%filter(disease1=="子宫平滑肌瘤"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diyisx222%>%filter(disease1=="自然临产阴道"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diyisx222%>%filter(disease1=="计划性剖宫产"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diyisx222%>%filter(disease1=="结肠癌"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diyisx222%>%filter(disease1=="胃癌"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diyisx222%>%filter(disease1=="乳腺癌"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diyisx222%>%filter(disease1=="腭裂"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diyisx222%>%filter(disease1=="急性单纯性阑"&HOSGRADE=="等级不明")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))


#融合数据
diyisx225=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316)

diyisx226=rbind(diyisx223,diyisx224,diyisx225)				


#计算掐头去尾后的均值和标准差

diyisx227=diyisx226%>%group_by(disease1,HOSGRADE)%>%
  summarise(掐头去尾均值=mean(TOTALFEE),掐头去尾标准差=sd(TOTALFEE))

#把均值和标准差和掐头去尾的均值标准差数据合并
diyipi1=left_join(diyisx4,diyisx227,by=c("disease1","HOSGRADE"))

diyipi2=mutate(diyipi1,P5除掐头去尾均值=P5/掐头去尾均值,P95除掐头去尾均值=P95/掐头去尾均值,cankao=掐头去尾均值*(1+0.02))


#导出
write.table(diyipi2,file = "省医保无术式.csv",sep = ",")
