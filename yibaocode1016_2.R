
#��������������������������������������������������������������������������������������������������������������1016
#����������������������������������������������������ϼ���ʽ


bzss=x1_6789

#


bzss$HOSGRADE[bzss$HOSGRADE=="�൱����"]<-"����"
bzss$HOSGRADE[bzss$HOSGRADE=="����"]<-"1-2��"
bzss$HOSGRADE[bzss$HOSGRADE=="һ��"]<-"1-2��"
bzss$HOSGRADE[bzss$HOSGRADE=="�൱һ��"]<-"1-2��"



#���ڴ���Ϊ��-��

bzss$time=format(as.Date(bzss$PAYTIME),format="%m-%d")

bzss$month=substr(bzss$time,1,2)

#����ҽԺ���ձ�׼����
#����������

result1_1=bzss%>%
  filter(�������=="�����ָ���")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="����")%>%
  filter(TOTALFEE>=����ҽԺ����)%>%
  filter(TOTALFEE<=����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(��������="�������ֱ�׼����")

#�����ռ���������
result1_2=bzss%>%
  filter(�������=="ָ������������")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="����")%>%
  rename(zhenduan=��ϼ���ʽ)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(��������="��ָ�����������ֱ�׼����")

result1=rbind(result1_1,result1_2)

#����ҽԺ��ԭ�ȱ�����ʽ����

result2=bzss%>%
  filter(�������=="�����ָ���")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="����")%>%
  filter(TOTALFEE<����ҽԺ����|TOTALFEE>����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=ʵ�ʼ��˽��)%>%
  mutate(��������="��ԭ�ȱ�����ʽ����")


result=rbind(result1,result2)



#����ҽԺ����׼����
#����������

result3_1=bzss%>%
  filter(�������=="�����ָ���")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="1-2��")%>%
  filter(TOTALFEE>=����ҽԺ����)%>%
  filter(TOTALFEE<=����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(��������="�������ֱ�׼����")

#�����ռ���������
result3_2=bzss%>%
  filter(�������=="ָ������������")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  filter(HOSGRADE=="1-2��")%>%
  rename(zhenduan=��ϼ���ʽ)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=����ҽԺ��׼֧���ܶ�*���˱���)%>%
  mutate(��������="��ָ�����������ֱ�׼����")

result3=rbind(result3_1,result3_2)

#����ҽԺ����ԭ�ȱ�����ʽ����
result4=bzss%>%
  filter(�������=="�����ָ���")%>%
  filter(month=="06"|month=="07"|month=="08"|month=="09")%>%
  mutate(����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼,
               ����ҽԺ����=0.5*����ҽԺ��׼,����ҽԺ����=2*����ҽԺ��׼)%>%
  filter(HOSGRADE=="1-2��")%>%
  filter(TOTALFEE<����ҽԺ����|TOTALFEE>����ҽԺ����)%>%
  rename(zhenduan=��ϼ���ʽ)%>%
  group_by(zhenduan,HOSPITAL)%>%
  summarise(����=n(),����ҽԺ��׼֧���ܶ�=sum(����ҽԺ��׼),ʵ�ʼ��˽��=sum(�ܼ��˽��),�ܷ���=sum(TOTALFEE))%>%
  mutate(���˱���=ʵ�ʼ��˽��/�ܷ���)%>%
  mutate(ҽ��֧������_��׼=ʵ�ʼ��˽��)%>%
  mutate(��������="��ԭ�ȱ�����ʽ����")



result_2=rbind(result3,result4)


write.table(result_2,file = "����ҽԺ��׼1016.csv",sep = ",",row.names = F)

write.table(result,file = "����ҽԺ��׼1016.csv",sep = ",",row.names = F)

