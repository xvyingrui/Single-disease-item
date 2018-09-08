#DBGs

library(ggplot2)
library(lubridate)
library(data.table)
library(dplyr)

#第一批分3级非3级比较
#读数据
diyi=fread("di1.csv",sep = ",",na.strings = "")


#去除重复值（按照ID、出院日期、总费用相同的为重复）
diyi513=diyi%>%group_by(ID,OUTDATE,TOTALFEE)%>%filter(row_number()==1)%>%ungroup()

#选子集
diyisx=select(diyi513,contains("FEE"),contains("TIME"),HOSGRADE,disease1,type)

#无去重
diyisx=select(diyi,contains("FEE"),contains("TIME"),HOSGRADE,disease1,type)

#去掉缺失行
diyisx1=na.omit(diyisx)

#OPERFEE>0，选取手术费大于0的数据
diyisx2=filter(diyisx1,OPERFEE>1)


#将一级和二级医院编码为1-2级
diyisx2$HOSGRADE[diyisx2$HOSGRADE=="一级"]<-"1-2级"
diyisx2$HOSGRADE[diyisx2$HOSGRADE=="二级"]<-"1-2级"
diyisx2$HOSGRADE[diyisx2$HOSGRADE=="三级"]<-"3级"

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

#选取子集
diyisx222=diyisx6%>%filter(disease1=="包皮过长"&type=="包皮环切术"|
                             disease1=="腱鞘囊肿"&type=="腱鞘囊肿切除术"|
                             disease1=="慢性扁桃体炎"&type=="扁桃体切除术"|
                             disease1=="头、面和颈软"&type=="浅表肿物切除术"|
                             disease1=="皮肤和皮下组"&type=="浅表肿物切除术"|
                             disease1=="头部肿物"&type=="浅表肿物切除术"|
                             disease1=="面部肿物"&type=="浅表肿物切除术"|
                             disease1=="颈部肿物"&type=="浅表肿物切除术"|
                             disease1=="肢体软组织肿"&type=="浅表肿物切除术"|
                             disease1=="上肢（肩）软"&type=="浅表肿物切除术"|
                             disease1=="下肢（髋）软"&type=="浅表肿物切除术"|
                             disease1=="乳腺良性肿瘤"&type=="乳腺肿物切除术"|
                             disease1=="乳房皮肤和皮"&type=="乳腺肿物切除术"|
                             disease1=="宫颈炎性疾病"&type=="宫颈环形电切术"|
                             disease1=="子宫颈息肉"&type=="宫颈息肉切除术"|
                             disease1=="结肠息肉"&type=="息肉摘除术"|
                             disease1=="直肠息肉"&type=="息肉摘除术"|
                             disease1=="尿道狭窄"&type=="尿道狭窄扩张术"|
                             disease1=="内痔/混合痔"&type=="痔疮切除术"|
                             disease1=="外痔"&type=="痔疮切除术"|
                             disease1=="痔疮"&type=="痔疮切除术"|
                             disease1=="肛裂"&type=="肛裂切除括约肌松解术"|
                             disease1=="肛瘘"&type=="肛瘘切除术"|
                             disease1=="翼状胬肉"&type=="翼状胬肉切除术"|
                             disease1=="先天性睑内翻"&type=="睑内翻矫正术"|
                             disease1=="鼓膜穿孔"&type=="鼓膜修补术"|
                             disease1=="外耳道肿物"&type=="外耳道良性肿物切除术"|
                             disease1=="取除骨折内固"&type=="骨折内固定装置取出术")

#重新编码不规范的病种名							 
diyisx222$disease1[diyisx222$disease1=="头、面和颈软"]<-"头面颈部皮肤和皮下组"
diyisx222$disease1[diyisx222$disease1=="皮肤和皮下组"]<-"头面颈部皮肤和皮下组"
							 
							 

#分组计算（按照病种、术式、医院等级进行分组计算均值等各个统计学指标）
diyisx4=diyisx222%>%group_by(disease1,type,HOSGRADE)%>%
  summarise(lishu1=n(),费用均值=mean(TOTALFEE),标准差=sd(TOTALFEE),最小值=min(TOTALFEE),
                P5=quantile(TOTALFEE,probs=c(0.05)),P25=quantile(TOTALFEE,probs=c(0.25)),
                P50=quantile(TOTALFEE,probs=c(0.50)),P75=quantile(TOTALFEE,probs=c(0.75)),
                P95=quantile(TOTALFEE,probs=c(0.95)),最大值=max(TOTALFEE))



				
				
#选取医院等级为1-2级的子集
xiaoyu31=diyisx222%>%filter(disease1=="包皮过长"&type=="包皮环切术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diyisx222%>%filter(disease1=="腱鞘囊肿"&type=="腱鞘囊肿切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diyisx222%>%filter(disease1=="慢性扁桃体炎"&type=="扁桃体切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diyisx222%>%filter(disease1=="头面颈部皮肤和皮下组"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diyisx222%>%filter(disease1=="头部肿物"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diyisx222%>%filter(disease1=="面部肿物"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diyisx222%>%filter(disease1=="颈部肿物"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diyisx222%>%filter(disease1=="肢体软组织肿"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diyisx222%>%filter(disease1=="上肢（肩）软"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diyisx222%>%filter(disease1=="下肢（髋）软"&type=="浅表肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diyisx222%>%filter(disease1=="乳腺良性肿瘤"&type=="乳腺肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diyisx222%>%filter(disease1=="乳房皮肤和皮"&type=="乳腺肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diyisx222%>%filter(disease1=="宫颈炎性疾病"&type=="宫颈环形电切术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diyisx222%>%filter(disease1=="子宫颈息肉"&type=="宫颈息肉切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diyisx222%>%filter(disease1=="结肠息肉"&type=="息肉摘除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diyisx222%>%filter(disease1=="直肠息肉"&type=="息肉摘除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu317=diyisx222%>%filter(disease1=="尿道狭窄"&type=="尿道狭窄扩张术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu318=diyisx222%>%filter(disease1=="内痔/混合痔"&type=="痔疮切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu319=diyisx222%>%filter(disease1=="外痔"&type=="痔疮切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu320=diyisx222%>%filter(disease1=="痔疮"&type=="痔疮切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu321=diyisx222%>%filter(disease1=="肛裂"&type=="肛裂切除括约肌松解术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu322=diyisx222%>%filter(disease1=="肛瘘"&type=="肛瘘切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu323=diyisx222%>%filter(disease1=="翼状胬肉"&type=="翼状胬肉切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu324=diyisx222%>%filter(disease1=="先天性睑内翻"&type=="睑内翻矫正术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu325=diyisx222%>%filter(disease1=="鼓膜穿孔"&type=="鼓膜修补术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu326=diyisx222%>%filter(disease1=="外耳道肿物"&type=="外耳道良性肿物切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu327=diyisx222%>%filter(disease1=="取除骨折内固"&type=="骨折内固定装置取出术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))


diyisx223=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316,xiaoyu317,xiaoyu318,xiaoyu319,xiaoyu320,xiaoyu321,xiaoyu322,xiaoyu323,xiaoyu324,xiaoyu325,xiaoyu326,xiaoyu327)



#选取医院等级为3级子集
xiaoyu31=diyisx222%>%filter(disease1=="包皮过长"&type=="包皮环切术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diyisx222%>%filter(disease1=="腱鞘囊肿"&type=="腱鞘囊肿切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diyisx222%>%filter(disease1=="慢性扁桃体炎"&type=="扁桃体切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diyisx222%>%filter(disease1=="头面颈部皮肤和皮下组"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diyisx222%>%filter(disease1=="头部肿物"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diyisx222%>%filter(disease1=="面部肿物"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diyisx222%>%filter(disease1=="颈部肿物"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diyisx222%>%filter(disease1=="肢体软组织肿"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diyisx222%>%filter(disease1=="上肢（肩）软"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diyisx222%>%filter(disease1=="下肢（髋）软"&type=="浅表肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diyisx222%>%filter(disease1=="乳腺良性肿瘤"&type=="乳腺肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diyisx222%>%filter(disease1=="乳房皮肤和皮"&type=="乳腺肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diyisx222%>%filter(disease1=="宫颈炎性疾病"&type=="宫颈环形电切术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diyisx222%>%filter(disease1=="子宫颈息肉"&type=="宫颈息肉切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diyisx222%>%filter(disease1=="结肠息肉"&type=="息肉摘除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diyisx222%>%filter(disease1=="直肠息肉"&type=="息肉摘除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu317=diyisx222%>%filter(disease1=="尿道狭窄"&type=="尿道狭窄扩张术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu318=diyisx222%>%filter(disease1=="内痔/混合痔"&type=="痔疮切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu319=diyisx222%>%filter(disease1=="外痔"&type=="痔疮切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu320=diyisx222%>%filter(disease1=="痔疮"&type=="痔疮切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu321=diyisx222%>%filter(disease1=="肛裂"&type=="肛裂切除括约肌松解术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu322=diyisx222%>%filter(disease1=="肛瘘"&type=="肛瘘切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu323=diyisx222%>%filter(disease1=="翼状胬肉"&type=="翼状胬肉切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu324=diyisx222%>%filter(disease1=="先天性睑内翻"&type=="睑内翻矫正术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu325=diyisx222%>%filter(disease1=="鼓膜穿孔"&type=="鼓膜修补术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu326=diyisx222%>%filter(disease1=="外耳道肿物"&type=="外耳道良性肿物切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu327=diyisx222%>%filter(disease1=="取除骨折内固"&type=="骨折内固定装置取出术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))

diyisx224=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316,xiaoyu317,xiaoyu318,xiaoyu319,xiaoyu320,xiaoyu321,xiaoyu322,xiaoyu323,xiaoyu324,xiaoyu325,xiaoyu326,xiaoyu327)



#合并已掐头去尾后的数据子集
diyisx225=rbind(diyisx223,diyisx224)


#计算掐头去尾后的均值和标准差

diyisx226=diyisx225%>%group_by(disease1,type,HOSGRADE)%>%
  summarise(掐头去尾均值=mean(TOTALFEE),掐头去尾标准差=sd(TOTALFEE))

#把均值和标准差和掐头去尾的均值标准差数据合并
diyipi1=left_join(diyisx4,diyisx226,by=c("disease1","type","HOSGRADE"))  


diyipi1=mutate(diyipi1,P5除掐头去尾均值=P5/掐头去尾均值,P95除掐头去尾均值=P95/掐头去尾均值,cankao=掐头去尾均值*(1+0.02))

#把均值和标准差和掐头去尾的均值标准差数据合并

#导出
write.table(diyipi1,file = "第一批病种加术式3级和非3级（无去重）.csv",sep = ",")
