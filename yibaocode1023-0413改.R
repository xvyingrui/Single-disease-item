


library(tidyverse)
library(readxl)			

#住院结果筛选			
X6789zy1021 <- read_excel("H:/data/ybsingled/6789zy1021.xlsx")			

#去重			
x1=X6789zy1021%>%group_by(ID,OUTDATE,TOTALFEE)%>%filter(row_number()==1)%>%ungroup()	

#单病种结果			
X6789qc1016 <- read_excel("H:/data/ybsingled/6789qc1016.xlsx")			


x2=X6789qc1016			

data1=rbind(x1,x2)		

#医院等级编码			
data1$HOSGRADE[data1$HOSGRADE=="一级"]<-"1-2级"			
data1$HOSGRADE[data1$HOSGRADE=="相当一级"]<-"1-2级"			
data1$HOSGRADE[data1$HOSGRADE=="二级"]<-"1-2级"			
data1$HOSGRADE[data1$HOSGRADE=="相当三级"]<-"三级"			


#分组计算			
result1=data1%>%			
  rename(zhenduan=诊断加术式,daiyu=待遇类别)%>%		
  group_by(zhenduan,HOSGRADE)%>%	
  summarise(lishu1=n(),费用均值=mean(TOTALFEE),费用mean=mean(TOTALFEE,trim = 0.1),标准差=sd(TOTALFEE),最小值=min(TOTALFEE),
            P5=quantile(TOTALFEE,	probs=c(0.05)),	P25=quantile(TOTALFEE,	probs=c(0.25)),
            P50=quantile(TOTALFEE	,probs=c(0.50)),	P75=quantile(TOTALFEE,	probs=c(0.75)),
            P95=quantile(TOTALFEE,	probs=c(0.95)),	最大值=max(TOTALFEE))	





#掐头去尾			
#1-2级医院			



data11=data1%>%filter(HOSGRADE=="1-2级")
data11$诊断加术式=as.factor(data11$诊断加术式)


DATA11=plyr::ddply(data11,"诊断加术式",function(x){
  x <- x[x$TOTALFEE > quantile(x$TOTALFEE, prob=0.05) & x$TOTALFEE < quantile(x$TOTALFEE, prob=0.95), ]
})




#3级医院			

data11=data1%>%filter(HOSGRADE=="三级")
data11$诊断加术式=as.factor(data11$诊断加术式)

DATA12=plyr::ddply(data11,"诊断加术式",function(x){
  x <- x[x$TOTALFEE > quantile(x$TOTALFEE, prob=0.05) & x$TOTALFEE < quantile(x$TOTALFEE, prob=0.95), ]
})


#合并掐头去尾的数据			

data2=rbind(DATA11,DATA12)		


#计算掐头去尾后的均值和标准差		


library(dplyr)

result2=data2%>%rename(zhenduan=诊断加术式)%>%
  group_by(zhenduan,HOSGRADE)%>%		
  summarise(掐头去尾均值=mean(TOTALFEE),掐头去尾标准差=sd(TOTALFEE))		


#合并两个结果表			
result1_2=left_join(result1,result2,by=c("zhenduan","HOSGRADE"))


write.table(result1_2	,file = "2018参考值计算1027.csv",sep=",",row.names=F)



#食管癌。。。。。。。。。。。。。。。。。。。。。。。。

sga_1=x2%>%filter(诊断加术式=="食管癌行食管癌切除胃代食管胸内吻合术治疗")%>%select(诊断加术式,TOTALFEE,HOSPITAL,ID)%>%
  rename(病种=诊断加术式,总费用=TOTALFEE,医院=HOSPITAL,病人=ID)

sga1027=rbind(sga_1,sga1026)

#分组计算			
result1=sga1027%>%			
  rename(zhenduan=病种,TOTALFEE=总费用)%>%		
  group_by(zhenduan)%>%	
  summarise(lishu1=n(),费用均值=mean(TOTALFEE),标准差=sd(TOTALFEE),最小值=min(TOTALFEE),
            P5=quantile(TOTALFEE,	probs=c(0.05)),	P25=quantile(TOTALFEE,	probs=c(0.25)),
            P50=quantile(TOTALFEE	,probs=c(0.50)),	P75=quantile(TOTALFEE,	probs=c(0.75)),
            P95=quantile(TOTALFEE,	probs=c(0.95)),	最大值=max(TOTALFEE))	





#掐头去尾			



DATA11=plyr::ddply(sga1027,"病种",function(x){
  x <- x[x$总费用 > quantile(x$总费用, prob=0.05) & x$总费用 < quantile(x$总费用, prob=0.95), ]
})






#计算掐头去尾后的均值和标准差		


library(dplyr)

result2=DATA11%>%rename(zhenduan=病种,TOTALFEE=总费用)%>%
  group_by(zhenduan)%>%		
  summarise(掐头去尾均值=mean(TOTALFEE),掐头去尾标准差=sd(TOTALFEE))		


#合并两个结果表			
result1_2=left_join(result1,result2,by=c("zhenduan"))


write.table(result1_2	,file = "食管癌.csv",sep=",",row.names=F)









