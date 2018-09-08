#dierpi

dier=fread("单病种第一次处理/di2.csv",sep = ",",na.strings = "")

#去重
dier513=dier%>%group_by(ID,OUTDATE,TOTALFEE)%>%filter(row_number()==1)%>%ungroup()


#筛选子集
diersx=select(dier513,contains("FEE"),contains("TIME"),HOSGRADE,DISEASE,shushi)

#删除缺失行
diersx1=na.omit(diersx)

#选取手术费大于0
diersx2=filter(diersx1,OPERFEE>1)

#将一级和二级医院编码为1-2级
diersx2$HOSGRADE[diersx2$HOSGRADE=="一级"]<-"1-2级"
diersx2$HOSGRADE[diersx2$HOSGRADE=="二级"]<-"1-2级"
diersx2$HOSGRADE[diersx2$HOSGRADE=="相当一级"]<-"1-2级"
diersx2$HOSGRADE[diersx2$HOSGRADE=="相当二级"]<-"1-2级"
diersx2$HOSGRADE[diersx2$HOSGRADE=="三级"]<-"3级"
diersx2$HOSGRADE[diersx2$HOSGRADE=="相当三级"]<-"3级"

#日期处理为年份
diersx2$PAYTIME=year(diersx2$PAYTIME)

#贴现率调整_2016


diersx60=diersx2%>%filter(PAYTIME==2012)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^4))%>%select(-TOTALFEE,-PAYTIME)
diersx61=diersx2%>%filter(PAYTIME==2013)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^3))%>%select(-TOTALFEE,-PAYTIME)
diersx62=diersx2%>%filter(PAYTIME==2014)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^2))%>%select(-TOTALFEE,-PAYTIME)
diersx63=diersx2%>%filter(PAYTIME==2015)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^1))%>%select(-TOTALFEE,-PAYTIME)
diersx64=diersx2%>%filter(PAYTIME==2016)%>%mutate(TOTAL1=TOTALFEE*((1+0.02)^0))%>%select(-TOTALFEE,-PAYTIME)

diersx6=rbind(diersx60,diersx61,diersx62,diersx63,diersx64)
diersx6=rename(diersx6,TOTALFEE=TOTAL1)


#选取术式和病种相符合的子集
diersx222=diersx6%>%filter(DISEASE=="冠状动脉粥样硬化性心脏病"&shushi=="冠状动脉球囊扩张及支架植入"|
                             DISEASE=="结节性甲状腺肿"&shushi=="甲状腺全切术"|
                             DISEASE=="结节性甲状腺肿"&shushi=="甲状腺次全切除术"|
                             DISEASE=="结节性甲状腺肿"&shushi=="甲状腺部分切除术"|
                             DISEASE=="胆囊结石"&shushi=="经腹腔镜胆囊切除术"|
                             DISEASE=="胆囊结石"&shushi=="胆囊切除术"|
                             DISEASE=="支气管肺癌"&shushi=="全肺切除术"|
                             DISEASE=="支气管肺癌"&shushi=="经胸腔镜肺段切除术"|
                             DISEASE=="支气管肺癌"&shushi=="肺段切除术"|
                             DISEASE=="支气管肺癌"&shushi=="经胸腔镜肺叶切除术"|
                             DISEASE=="支气管肺癌"&shushi=="肺叶切除术"|
                             DISEASE=="股骨头坏死"&shushi=="股骨头假体置换术"|
                             DISEASE=="膝关节病"&shushi=="全膝关节置换术"|
                             DISEASE=="髋关节病"&shushi=="全髋关节置换术"|
                             DISEASE=="肾癌"&shushi=="经腹腔镜根治性肾切除术"|
                             DISEASE=="肾癌"&shushi=="根治性肾切除术"|
                             DISEASE=="脑膜瘤"&shushi=="伽玛刀治疗"|
                             DISEASE=="膀胱肿瘤"&shushi=="膀胱部分切除术"|
                             DISEASE=="膀胱肿物"&shushi=="经尿道膀胱肿瘤电切术"|
                             DISEASE=="膀胱恶性肿瘤"&shushi=="经尿道膀胱肿瘤电切术"|
                             DISEASE=="输尿管结石"&shushi=="经输尿管镜碎石取石术"|
                             DISEASE=="肾功能衰竭"&shushi=="异体肾移植术"|
							 DISEASE=="动脉导管未闭"&shushi=="经皮穿刺动脉导管未闭封堵术"|
							 DISEASE=="房间隔缺损"&shushi=="房间隔缺损补片修补术"|
							 DISEASE=="风湿性心脏病二尖瓣病变"&shushi=="二尖瓣置换术"|
							 DISEASE=="垂体瘤"&shushi=="伽玛刀治疗"|
							 DISEASE=="肩袖损伤"&shushi=="经关节镜下肩袖损伤修复术"|
							 DISEASE=="原发性急性闭角型青光眼"&shushi=="小梁切除术"|
							 DISEASE=="子宫腺肌病"&shushi=="经腹腔镜下子宫切除术"|
							 DISEASE=="子宫腺肌病"&shushi=="经腹全子宫切除术"|
							 DISEASE=="复发性肩关节脱位"&shushi=="经关节镜肩关节脱位修复术"|
							 DISEASE=="慢性鼻窦炎"&shushi=="行鼻内窥镜手术"|
							 DISEASE=="脑动静脉畸形"&shushi=="伽玛刀治疗")



#分组计算

diersx4=diersx222%>%group_by(DISEASE,shushi,HOSGRADE)%>%
  summarise(例数=n(),费用均值=mean(TOTALFEE),标准差=sd(TOTALFEE),最小值=min(TOTALFEE),
                P5=quantile(TOTALFEE,probs=c(0.05)),P25=quantile(TOTALFEE,probs=c(0.25)),
                P50=quantile(TOTALFEE,probs=c(0.50)),P75=quantile(TOTALFEE,probs=c(0.75)),
                P95=quantile(TOTALFEE,probs=c(0.95)),最大值=max(TOTALFEE))





#选取医院等级为1-2级的子集

xiaoyu31=diersx222%>%filter(DISEASE=="冠状动脉粥样硬化性心脏病"&shushi=="冠状动脉球囊扩张及支架植入"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diersx222%>%filter(DISEASE=="结节性甲状腺肿"&shushi=="甲状腺全切术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diersx222%>%filter(DISEASE=="结节性甲状腺肿"&shushi=="甲状腺次全切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diersx222%>%filter(DISEASE=="结节性甲状腺肿"&shushi=="甲状腺部分切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diersx222%>%filter(DISEASE=="胆囊结石"&shushi=="经腹腔镜胆囊切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diersx222%>%filter(DISEASE=="胆囊结石"&shushi=="胆囊切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="全肺切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="经胸腔镜肺段切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="肺段切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="经胸腔镜肺叶切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="肺叶切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diersx222%>%filter(DISEASE=="股骨头坏死"&shushi=="股骨头假体置换术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diersx222%>%filter(DISEASE=="膝关节病"&shushi=="全膝关节置换术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diersx222%>%filter(DISEASE=="髋关节病"&shushi=="全髋关节置换术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diersx222%>%filter(DISEASE=="肾癌"&shushi=="经腹腔镜根治性肾切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diersx222%>%filter(DISEASE=="肾癌"&shushi=="根治性肾切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu317=diersx222%>%filter(DISEASE=="脑膜瘤"&shushi=="伽玛刀治疗"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu318=diersx222%>%filter(DISEASE=="膀胱肿瘤"&shushi=="膀胱部分切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu319=diersx222%>%filter(DISEASE=="膀胱肿物"&shushi=="经尿道膀胱肿瘤电切术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu320=diersx222%>%filter(DISEASE=="膀胱恶性肿瘤"&shushi=="经尿道膀胱肿瘤电切术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu321=diersx222%>%filter(DISEASE=="输尿管结石"&shushi=="经输尿管镜碎石取石术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu322=diersx222%>%filter(DISEASE=="肾功能衰竭"&shushi=="异体肾移植术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu323=diersx222%>%filter(DISEASE=="动脉导管未闭"&shushi=="经皮穿刺动脉导管未闭封堵术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu324=diersx222%>%filter(DISEASE=="房间隔缺损"&shushi=="房间隔缺损补片修补术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu325=diersx222%>%filter(DISEASE=="风湿性心脏病二尖瓣病变"&shushi=="二尖瓣置换术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu326=diersx222%>%filter(DISEASE=="垂体瘤"&shushi=="伽玛刀治疗"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu327=diersx222%>%filter(DISEASE=="肩袖损伤"&shushi=="经关节镜下肩袖损伤修复术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu328=diersx222%>%filter(DISEASE=="原发性急性闭角型青光眼"&shushi=="小梁切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu329=diersx222%>%filter(DISEASE=="子宫腺肌病"&shushi=="经腹腔镜下子宫切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu330=diersx222%>%filter(DISEASE=="子宫腺肌病"&shushi=="经腹全子宫切除术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu331=diersx222%>%filter(DISEASE=="复发性肩关节脱位"&shushi=="经关节镜肩关节脱位修复术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu332=diersx222%>%filter(DISEASE=="慢性鼻窦炎"&shushi=="行鼻内窥镜手术"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu333=diersx222%>%filter(DISEASE=="脑动静脉畸形"&shushi=="伽玛刀治疗"&HOSGRADE=="1-2级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))

#融合数据
diersx223=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316,xiaoyu317,xiaoyu318,xiaoyu319,xiaoyu320,xiaoyu321,xiaoyu322,xiaoyu323,xiaoyu324,xiaoyu325,xiaoyu326,xiaoyu327,xiaoyu328,xiaoyu329,xiaoyu330,xiaoyu331,xiaoyu332,xiaoyu333)





#选取医院等级为3级子集
#选取术式和病种相符合的子集
xiaoyu31=diersx222%>%filter(DISEASE=="冠状动脉粥样硬化性心脏病"&shushi=="冠状动脉球囊扩张及支架植入"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu32=diersx222%>%filter(DISEASE=="结节性甲状腺肿"&shushi=="甲状腺全切术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu33=diersx222%>%filter(DISEASE=="结节性甲状腺肿"&shushi=="甲状腺次全切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu34=diersx222%>%filter(DISEASE=="结节性甲状腺肿"&shushi=="甲状腺部分切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu35=diersx222%>%filter(DISEASE=="胆囊结石"&shushi=="经腹腔镜胆囊切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu36=diersx222%>%filter(DISEASE=="胆囊结石"&shushi=="胆囊切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu37=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="全肺切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu38=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="经胸腔镜肺段切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu39=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="肺段切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu310=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="经胸腔镜肺叶切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu311=diersx222%>%filter(DISEASE=="支气管肺癌"&shushi=="肺叶切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu312=diersx222%>%filter(DISEASE=="股骨头坏死"&shushi=="股骨头假体置换术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu313=diersx222%>%filter(DISEASE=="膝关节病"&shushi=="全膝关节置换术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu314=diersx222%>%filter(DISEASE=="髋关节病"&shushi=="全髋关节置换术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu315=diersx222%>%filter(DISEASE=="肾癌"&shushi=="经腹腔镜根治性肾切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu316=diersx222%>%filter(DISEASE=="肾癌"&shushi=="根治性肾切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu317=diersx222%>%filter(DISEASE=="脑膜瘤"&shushi=="伽玛刀治疗"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu318=diersx222%>%filter(DISEASE=="膀胱肿瘤"&shushi=="膀胱部分切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu319=diersx222%>%filter(DISEASE=="膀胱肿物"&shushi=="经尿道膀胱肿瘤电切术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu320=diersx222%>%filter(DISEASE=="膀胱恶性肿瘤"&shushi=="经尿道膀胱肿瘤电切术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu321=diersx222%>%filter(DISEASE=="输尿管结石"&shushi=="经输尿管镜碎石取石术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu322=diersx222%>%filter(DISEASE=="肾功能衰竭"&shushi=="异体肾移植术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu323=diersx222%>%filter(DISEASE=="动脉导管未闭"&shushi=="经皮穿刺动脉导管未闭封堵术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu324=diersx222%>%filter(DISEASE=="房间隔缺损"&shushi=="房间隔缺损补片修补术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu325=diersx222%>%filter(DISEASE=="风湿性心脏病二尖瓣病变"&shushi=="二尖瓣置换术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu326=diersx222%>%filter(DISEASE=="垂体瘤"&shushi=="伽玛刀治疗"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu327=diersx222%>%filter(DISEASE=="肩袖损伤"&shushi=="经关节镜下肩袖损伤修复术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu328=diersx222%>%filter(DISEASE=="原发性急性闭角型青光眼"&shushi=="小梁切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu329=diersx222%>%filter(DISEASE=="子宫腺肌病"&shushi=="经腹腔镜下子宫切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu330=diersx222%>%filter(DISEASE=="子宫腺肌病"&shushi=="经腹全子宫切除术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu331=diersx222%>%filter(DISEASE=="复发性肩关节脱位"&shushi=="经关节镜肩关节脱位修复术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu332=diersx222%>%filter(DISEASE=="慢性鼻窦炎"&shushi=="行鼻内窥镜手术"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))
xiaoyu333=diersx222%>%filter(DISEASE=="脑动静脉畸形"&shushi=="伽玛刀治疗"&HOSGRADE=="3级")%>%filter(TOTALFEE>quantile(TOTALFEE,probs = c(0.05))&TOTALFEE<quantile(TOTALFEE,probs = c(0.95)))

#融合数据
diersx224=rbind(xiaoyu31,xiaoyu32,xiaoyu33,xiaoyu34,xiaoyu35,xiaoyu36,xiaoyu37,xiaoyu38,xiaoyu39,xiaoyu310,xiaoyu311,xiaoyu312,xiaoyu313,xiaoyu314,xiaoyu315,xiaoyu316,xiaoyu317,xiaoyu318,xiaoyu319,xiaoyu320,xiaoyu321,xiaoyu322,xiaoyu323,xiaoyu324,xiaoyu325,xiaoyu326,xiaoyu327,xiaoyu328,xiaoyu329,xiaoyu330,xiaoyu331,xiaoyu332,xiaoyu333)


diersx225=rbind(diersx223,diersx224)				


#计算掐头去尾后的均值和标准差

diersx226=diersx225%>%group_by(DISEASE,shushi,HOSGRADE)%>%
  summarise(掐头去尾均值=mean(TOTALFEE),掐头去尾标准差=sd(TOTALFEE))

#把均值和标准差和掐头去尾的均值标准差数据合并
dierpi1=left_join(diersx4,diersx226,by=c("DISEASE","shushi","HOSGRADE"))


dierpi2=mutate(dierpi1,P5除掐头去尾均值=P5/掐头去尾均值,P95除掐头去尾均值=P95/掐头去尾均值,cankao=掐头去尾均值*(1+0.02))



#导出
write.table(dierpi2,file = "第二批病种加术式3级非3级.csv",sep = ",")




