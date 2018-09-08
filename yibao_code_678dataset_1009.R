


#...................................6\7\8月份数据分析

x_678=X678_qc_0926


x_678[is.na(x_678)]<-0

x_678$time=format(as.Date(x_678$INDATE),format="%m-%d")

x_678$month=substr(x_678$time,1,2)

x_678$day=substr(x_678$time,4,5)




#将数据的7月15号之前与之后重新编码为time=7为7月15号之后的；time=6为7月15号之前
x_678$day=as.numeric(x_678$day)
x_678$month=as.numeric(x_678$month)

x_678_1=x_678%>%filter(month>=6)

x_678_1$time=cut(x_678_1$INDATE,breaks = "week")


############按照诊断和时间分组

x1=x_678_1%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(time,zhenduan)%>%
  filter(zhenduan!=0)%>%
  summarise(总费用=sum(TOTALFEE),例数=n(),                   ######总费用的计算
               总计账金额总费用=sum(总记账金额),床位费=sum(BEDFEE),中药费=sum(CMEDFEE),
               中草药费=sum(HERBFEE),治疗费=sum(TREATFEE),手术费=sum(OPERFEE),输血费=sum(BLOODFEE),
               其他费=sum(OTHERFEE),材料费=sum(MATERFEE),特殊治疗费=sum(SPETRFEE),西药费=sum(WESTFEE),
               中成药费=sum(CPATFEE),检查费=sum(EXAMFEE),放射费=sum(RADIAFEE),化验费=sum(LABFEE),
               输氧费=sum(OXYGFEE),麻醉费=sum(ANESTFEE),特殊检查费=sum(SPEEXFEE))%>%
  mutate(每例总费用=总费用/例数,每例总计账金额总费用=总计账金额总费用/例数,床位费占比=床位费/总费用,中药费占比=中药费/总费用,
              中草药费占比=中草药费/总费用,治疗费占比=治疗费/总费用,手术费占比=手术费/总费用,
              输血费占比=输血费/总费用,其他费占比=其他费/总费用,材料费占比=材料费/总费用,特殊治疗费占比=特殊治疗费/总费用,
              西药费占比=西药费/总费用,中成药费占比=中成药费/总费用,检查费占比=检查费/总费用,放射费占比=放射费/总费用,
              化验费占比=化验费/总费用,输氧费占比=输氧费/总费用,麻醉费占比=麻醉费/总费用,特殊检查费占比=特殊检查费/总费用,
              例均药品费用=(中草药费+中药费+西药费+中成药费)/例数,例均检查费用=(检查费+放射费+化验费+特殊检查费)/例数,
              例均手术及治疗费=(治疗费+手术费+特殊治疗费+输氧费+麻醉费+输血费)/例数,
              例均材料及其他费=(床位费+其他费+材料费)/例数,        ###例均费用的计算
              药品费用占比=(中草药费+中药费+西药费+中成药费)/总费用,检查费用占比=(检查费+放射费+化验费+特殊检查费)/总费用,
              手术及治疗费占比=(治疗费+手术费+特殊治疗费+输氧费+麻醉费+输血费)/总费用,
              材料及其他费占比=(床位费+其他费+材料费)/总费用)          ###费用占比的计算

#############按照时间分组

x2=x_678_1%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(time)%>%
  filter(zhenduan!=0)%>%
  summarise(总费用=sum(TOTALFEE),例数=n(),                   ######总费用的计算
               总计账金额总费用=sum(总记账金额),床位费=sum(BEDFEE),中药费=sum(CMEDFEE),
               中草药费=sum(HERBFEE),治疗费=sum(TREATFEE),手术费=sum(OPERFEE),输血费=sum(BLOODFEE),
               其他费=sum(OTHERFEE),材料费=sum(MATERFEE),特殊治疗费=sum(SPETRFEE),西药费=sum(WESTFEE),
               中成药费=sum(CPATFEE),检查费=sum(EXAMFEE),放射费=sum(RADIAFEE),化验费=sum(LABFEE),
               输氧费=sum(OXYGFEE),麻醉费=sum(ANESTFEE),特殊检查费=sum(SPEEXFEE))%>%
  mutate(每例总费用=总费用/例数,每例总计账金额总费用=总计账金额总费用/例数,床位费占比=床位费/总费用,中药费占比=中药费/总费用,
              中草药费占比=中草药费/总费用,治疗费占比=治疗费/总费用,手术费占比=手术费/总费用,
              输血费占比=输血费/总费用,其他费占比=其他费/总费用,材料费占比=材料费/总费用,特殊治疗费占比=特殊治疗费/总费用,
              西药费占比=西药费/总费用,中成药费占比=中成药费/总费用,检查费占比=检查费/总费用,放射费占比=放射费/总费用,
              化验费占比=化验费/总费用,输氧费占比=输氧费/总费用,麻醉费占比=麻醉费/总费用,特殊检查费占比=特殊检查费/总费用,
              例均药品费用=(中草药费+中药费+西药费+中成药费)/例数,例均检查费用=(检查费+放射费+化验费+特殊检查费)/例数,
              例均手术及治疗费=(治疗费+手术费+特殊治疗费+输氧费+麻醉费+输血费)/例数,
              例均材料及其他费=(床位费+其他费+材料费)/例数,        ###例均费用的计算
              药品费用占比=(中草药费+中药费+西药费+中成药费)/总费用,检查费用占比=(检查费+放射费+化验费+特殊检查费)/总费用,
              手术及治疗费占比=(治疗费+手术费+特殊治疗费+输氧费+麻醉费+输血费)/总费用,
              材料及其他费占比=(床位费+其他费+材料费)/总费用)          ###费用占比的计算


######................构图

ggplot(x2, aes(x=time, y=每例总费用, group=1)) + 
  geom_line(linetype="solid",colour="red",size=1)+
  geom_point(size=4,shape=20)+
  geom_text(data=x2,aes(x=time,y=每例总费用,label=每例总费用),position = "dodge")


ggplot(x2, aes(x=time, y=每例总费用, group=1)) + 
  geom_line(linetype="solid",colour="red",size=0.5)+
  geom_point(size=4,shape=20)+
  geom_text(data=x2,aes(x=time,y=每例总费用,label=每例总费用),position = "dodge")






















###########################################################################

x1_67_qc_7=x_678%>%filter(month==7&day>15|month==8)%>%mutate(time="7月15号之后")
x1_67_qc_6=x_678%>%filter(month==7&day<=15|month==6)%>%mutate(time="7月15号之前")

x1_67_qc_67=rbind(x1_67_qc_6,x1_67_qc_7)





x1=x1_67_qc_67%>%
  rename(zhenduan=诊断加术式)%>%
  group_by(time,zhenduan)%>%
  summarise(sum_treatfee=sum(TREATFEE),sum_materfee=sum(MATERFEE),
            sum_westfee=sum(WESTFEE),sum_totalfee=sum(TOTALFEE),number=n())%>%
  mutate(sin_materfee=sum_materfee/number,sin_totalfee=sum_totalfee/number,
         sin_westfee=sum_westfee/number,sin_treatfee=sum_treatfee/number)




































