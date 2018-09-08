
##########����������6789����������������������20171016


x1_6=fread("h:/data/ybsingled/yibaoshuju_6��/1_xg.csv",sep=",")
x1_7=fread("h:/data/ybsingled/yibaoshuju_7��/1_xg.csv",sep=",")
x1_8=fread("h:/data/ybsingled/2017��8������/TJ_0613_�����ֽ��0926.csv",sep=",")
x1_9=fread("h:/data/ybsingled/������2017��9������/0559�����ֽ��.csv",sep=",")




x1_67=rbind(x1_6,x1_7,x1_8,x1_9 )

x1_678_qc=x1_67%>%group_by(��ҽ���к�,ICD10,ID,OUTDATE,TOTALFEE,INDATE)%>%
  filter(row_number()==1)%>%
  ungroup()

write.table(x1_678_qc,file = "6789qc1016.csv",sep = ",",row.names = F)

library(readxl)
x1_6789 <- read_excel("H:/data/ybsingled/6789qc1016.xlsx")

#��������������������������������������������������������

x1_67_qc1=x1_6789

x1_67_qc1$HOSGRADE=as.character(x1_67_qc1$HOSGRADE)


x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="�൱����"]<-"����"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="����"]<-"1-2��"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="һ��"]<-"1-2��"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="�൱һ��"]<-"1-2��"



#���ڴ���Ϊ��-��

bzss=x1_67_qc1

bzss$time=format(as.Date(bzss$PAYTIME),format="%m-%d")

bzss$month=substr(bzss$time,1,2)

##..........................................................................,,.......ָ�����ָ���

bzss1=bzss%>%filter(�������=="�����ָ���")

#����ҽԺ���ձ�׼����
result1=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="����")%>%
  filter(TOTALFEE>=����ҽԺ����)%>%
  filter(TOTALFEE<=����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ,daiyu=�������)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(�������="�����ָ���")


#����ҽԺ��ԭ�ȱ�����ʽ����

result2=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="����")%>%
  filter(TOTALFEE<����ҽԺ����|TOTALFEE>����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ,daiyu=�������)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=ʵ�ʼ��˽��)%>%
  mutate(�������="��ԭ�ȱ�����ʽ����")


result=rbind(result1,result2)


#����ҽԺ����׼����

result3=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="1-2��")%>%
  filter(TOTALFEE>=����ҽԺ����)%>%
  filter(TOTALFEE<=����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ,daiyu=�������)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(�������="�����ָ���")



result4=bzss1%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="1-2��")%>%
  filter(TOTALFEE<����ҽԺ����|TOTALFEE>����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ,daiyu=�������)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=ʵ�ʼ��˽��)%>%
  mutate(�������="��ԭ�ȱ�����ʽ����")


result_2=rbind(result3,result4)

result=rename(result,֧����׼�ܶ�=����ҽԺ��׼֧���ܶ�)

result_2=rename(result_2,֧����׼�ܶ�=����ҽԺ��׼֧���ܶ�)

result_12=rbind(result,result_2)%>%mutate(ҽ��ʵ�ʸ�������=ҽ��֧������_��׼/�ܷ���)

write.table(result_12,file = "�����ָ���1016.csv",sep=",",row.names=F)

#..........................................................................,,.......ָ����������

#����ָ����������
bzss2=bzss%>%filter(�������=="ָ������������")



#��������������������������������������������������������

x1_67_qc1=bzss2

x1_67_qc1$HOSGRADE=as.character(x1_67_qc1$HOSGRADE)


x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="�൱����"]<-"����"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="����"]<-"1-2��"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="һ��"]<-"1-2��"
x1_67_qc1$HOSGRADE[x1_67_qc1$HOSGRADE=="�൱һ��"]<-"1-2��"



#���ڴ���Ϊ��-��

bzss2=x1_67_qc1

bzss2$time=format(as.Date(bzss2$PAYTIME),format="%m-%d")

bzss2$month=substr(bzss2$time,1,2)


#����ҽԺ���ձ�׼����
result1=bzss2%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="����")%>%
  rename(zhenduan=��ϼ���ʽ,daiyu=�������)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(ҽ��ʵ�ʸ�������=ҽ��֧������_��׼/�ܷ���)%>%
  rename(֧����׼�ܶ�=����ҽԺ��׼֧���ܶ�)


#����ҽԺ����׼����

result2=bzss2%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="1-2��")%>%
  rename(zhenduan=��ϼ���ʽ,daiyu=�������)%>%
  group_by(zhenduan,HOSGRADE,daiyu)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(ҽ��ʵ�ʸ�������=ҽ��֧������_��׼/�ܷ���)%>%
  rename(֧����׼�ܶ�=����ҽԺ��׼֧���ܶ�)

result12=rbind(result1,result2)


write.table(result12,file = "��ָ����������1016.csv",sep=",",row.names=F)





