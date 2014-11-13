rm(list=ls())
fdat<-read.csv("c:/Classes/AppliedBiostat/Project2/frmgham.csv")
library(reshape2)#for manipulating data 
library(rms)#includes convenience functions for evaluating non-linear relationships
library(BDSS)#personal package for convenience functions and table builder
library(ResourceSelection)#includes hosmer lemeshow test
#Must reshape the data to fit simple logistic model
mdat<-melt(fdat,id=c("RANDID","PERIOD"))
#Create new variable for each time period for each measured variable
ndat<-dcast(mdat,RANDID~variable+PERIOD)
rm(fdat,mdat)
#limit to only cases without prevalent AP or MI
include<-ndat$PREVAP_1==0 & ndat$PREVMI_1==0# & ndat$HEARTRTE_2<200#& ndat$TOTCHOL_2<450
ndat<-ndat[include,]
ndat$sex<-factor(ifelse(ndat$SEX_2==1,"M",ifelse(ndat$SEX_2==2,"F",NA)))
#Define event as someone who had no AP or MI in period 1 and had AP or MI in period 2
ndat$event<-ifelse((ndat$PREVAP_1==0 & ndat$PREVAP_2==1 )|(ndat$PREVMI_1==0 & ndat$PREVMI_2==1 ),1,0)
ndat<-ndat[!is.na(ndat$event),]
ndat$Event<-factor(ifelse(ndat$event,"CVD","No CVD"))

# sasdat<-ndat
# sasdat[is.na(sasdat)]<--999
# write.csv(sasdat,"C:/Classes/AppliedBiostat/Project2/ReshapedFarmData.csv")
m1dat<-subset(ndat, select=c(TOTCHOL_2,sex,AGE_2,CIGPDAY_2,BMI_2,BPMEDS_2,SYSBP_2,HEARTRTE_2,DIABETES_2,event))
m1dat<-m1dat[complete.cases(m1dat),]
#148 events
#Fit model for question 1
model1<-glm(event~TOTCHOL_2+sex*AGE_2+CIGPDAY_2+BMI_2+factor(BPMEDS_2)*SYSBP_2+rcspline.eval(HEARTRTE_2)+factor(DIABETES_2),data=m1dat,family=binomial(link='logit'),na.actio="na.fail")
m1<-lrm(event~TOTCHOL_2+sex*AGE_2+CIGPDAY_2+BMI_2+factor(BPMEDS_2)*SYSBP_2+HEARTRTE_2+factor(DIABETES_2),data=m1dat,x=T,y=T)
summary(model1)
#check for non-linear relationships
rcspline.plot(m1dat$TOTCHOL_2,m1dat$event,model="logistic",xlab="Total Cholesterol")
rcspline.plot(m1dat$AGE_2,m1dat$event,model="logistic",xlab="Age")
rcspline.plot(m1dat$CIGPDAY_2,m1dat$event,model="logistic",xlab="Cigarettes/day")
rcspline.plot(m1dat$BMI_2,m1dat$event,model="logistic",xlab="BMI")
rcspline.plot(m1dat$SYSBP_2,m1dat$event,model="logistic",xlab="Systolic BP")
rcspline.plot(m1dat$HEARTRTE_2,m1dat$event,model="logistic",xlab="Heart Rate")
#Conduct hosmer lemeshow GOF test

m1preds<-exp(predict(model1))/(1+exp(predict(model1)))
hoslem.test(m1dat$event,m1preds,g=10)

#create quiting variable if case smoked in period 1 and did not in period 2
ndat$quit<-ifelse(ndat$CURSMOKE_1==1 & ndat$CURSMOKE_2==0,1,0)
#create event variable for question 2 using those that had an event between period 2 and 3.
for(i in 1:dim(ndat)[1]){
  ndat$minTimeCVD[i]<-min(ndat$TIMEAP_1[i], ndat$TIMEMI_1[i])
}
ndat$event2<-ifelse((ndat$PREVAP_1==0 & ndat$PREVAP_2==0 & ndat$PREVAP_3==1 )|(ndat$PREVMI_1==0 & ndat$PREVMI_2==0 & ndat$PREVMI_3==1),1,0)
ndat$Event2<-factor(ifelse((ndat$PREVAP_1==0 & ndat$PREVAP_2==0 & ndat$PREVAP_3==1 )|(ndat$PREVMI_1==0 & ndat$PREVMI_2==0 & ndat$PREVMI_3==1),"CVD","No CVD")) 
include2<-ndat$PREVAP_1==0 & ndat$PREVMI_1==0 & ndat$PREVAP_2==0 & ndat$PREVMI_2==0 & ndat$HEARTRTE_2<200
#create data set for question 2 with only those cases that smoked in the first period
m2dat<-ndat[include2,]
m2dat<-subset(m2dat, select=c(TOTCHOL_3,sex,AGE_3,CIGPDAY_3,BMI_3,BPMEDS_3,SYSBP_3,HEARTRTE_3,DIABETES_3,quit,CURSMOKE_3,event2,Event2,minTimeCVD))
#Fit model for question 2
m2dat1<-m2dat[complete.cases(m2dat),]
model2<-glm(event2~TOTCHOL_3+sex*AGE_3+CIGPDAY_3+BMI_3+factor(BPMEDS_3)*SYSBP_3+rcspline.eval(HEARTRTE_3)+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
summary(model2)
#check if confounders can be removed
#check CIGPDAY
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+BMI_3+factor(BPMEDS_3)*SYSBP_3+rcspline.eval(HEARTRTE_3)+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#check BMI
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+factor(BPMEDS_3)*SYSBP_3+rcspline.eval(HEARTRTE_3)+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#check BPmed SysBP interaction
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+factor(BPMEDS_3)+SYSBP_3+rcspline.eval(HEARTRTE_3)+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#check BP Meds Main effect
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+SYSBP_3+rcspline.eval(HEARTRTE_3)+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#test HR spline
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+SYSBP_3+HEARTRTE_3+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#test HR main effect
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+SYSBP_3+factor(DIABETES_3)+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#test diabetes
model2r<-glm(event2~TOTCHOL_3+sex*AGE_3+SYSBP_3+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#test age by sex interaction
model2r<-glm(event2~TOTCHOL_3+sex+AGE_3+SYSBP_3+quit,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1
#test CURSMOKE
model2r<-glm(event2~TOTCHOL_3+sex+AGE_3+SYSBP_3+quit+CURSMOKE_3,data=m2dat1,family=binomial(link='logit'))
abs(exp(summary(model2)$coef["quit",1])-exp(summary(model2r)$coef["quit",1])) /exp(summary(model2)$coef["quit",1]) >= .1

#final model (added rcspline HR back into the model since it is a known risk factor according to model 1, also added current smoking status into the model so that ORs can be compared between a person that quits vs continues to smoke) 
model2r<-glm(event2~TOTCHOL_3+sex+AGE_3+SYSBP_3+rcspline.eval(HEARTRTE_3)+quit+CIGPDAY_3,data=m2dat1,family=binomial(link='logit'),na.action="na.fail")
#The only variable that had any effect was cigs per day but the effect was less than 10%

#hosmer-lemeshow for 2nd model
m2preds<-exp(predict(model2r))/(1+exp(predict(model2r)))
hoslem.test(as.numeric(as.character(m2dat1$event2)),m2preds,g=10)

#summary table of variables used in analysis
t1<-SummaryTable(ndat,rowvars=c("BPMEDS_2","DIABETES_2","CURSMOKE_2","sex"),row.names=c("BP Meds","Diabetes","Smoking Status","Sex"),colvar="Event",cont.vars=c("TOTCHOL_2","AGE_2","CIGPDAY_2","BMI_2","SYSBP_2","HEARTRTE_2"),output=c("matrix","rtf","latex"),outfile="c:/Classes/AppliedBiostat/Project2/SummaryTableM1.rtf")
#write summary table to CSV for conveneience
write.csv(t1,"c:/Classes/AppliedBiostat/Project2/Table1.csv")

t2<-SummaryTable(m2dat1,rowvars=c("BPMEDS_3","DIABETES_3","CURSMOKE_3","quit","sex"),row.names=c("BP Meds","Diabetes","Smoking Status","Quit Smoking","Sex"),colvar="Event2",cont.vars=c("TOTCHOL_3","AGE_3","CIGPDAY_3","BMI_3","SYSBP_3","HEARTRTE_3"),output=c("matrix","rtf","latex"),outfile="c:/Classes/AppliedBiostat/Project2/SummaryTableM2.rtf")
#write summary table to CSV for conveneience
write.csv(t2,"c:/Classes/AppliedBiostat/Project2/Table2.csv")
#model summaries for inclusion in report
sm1<-summary(model1)
m1or<-exp(sm1$coef[,1]) #calculate ORs
m1lci<-exp(sm1$coef[,1]-1.96*sm1$coef[,2])#calculate CLs
m1uci<-exp(sm1$coef[,1]+1.96*sm1$coef[,2])
m1mat<-matrix(c(m1or,m1lci,m1uci,sm1$coef[,4]),length(sm1$coef[,1]),4)
row.names(m1mat)<-c("(Intercept","Total Cholesterol","Males","Age","Cig./day","BMI","BP Medicine","Systolic BP","spline(Heart rate)1","spline(Heart rate)2","spline(Heart rate)3","Diabetes", "Males*Age","BP meds*Systolic BP")
colnames(m1mat)<-c("OR","Lower CI","Upper CI", "p-value")
xtable(m1mat) #write to latex for report
write.csv(m1mat,"c:/Classes/AppliedBiostat/Project2/Model1ORs.csv")

sm2<-summary(model2r)
m2or<-exp(sm2$coef[,1])
m2lci<-exp(sm2$coef[,1]-1.96*sm2$coef[,2])
m2uci<-exp(sm2$coef[,1]+1.96*sm2$coef[,2])
m2mat<-matrix(c(m2or,m2lci,m2uci,sm2$coef[,4]),length(sm2$coef[,1]),4)
row.names(m2mat)<-c("(Intercept","Total Cholesterol","Males","Age","Systolic BP","spline(Heart rate)1","spline(Heart rate)2","spline(Heart rate)3","Smoking Cessation", "Cig./ day")
colnames(m2mat)<-c("OR","Lower CI","Upper CI", "p-value")
xtable(m2mat)
write.csv(m2mat,"c:/Classes/AppliedBiostat/Project2/Model2ORs.csv")



#Sensitivity Analyses
#model averaging
library(MuMIn)
d1<-dredge(model1)#fit all possible models from variables in global model
avg1<-model.avg(d1,beta=T)# average parameters and calculate variable weights
#combine results of model average and final model
avg1.mat<-exp(avg1$avg.model[,c(1,4,5)])
comb1.mat<-cbind(m1mat[,1:3],avg1.mat)
xtable(comb1.mat)
avg1$importance#variable weights

# d2<-dredge(model2r)
# avg2<-model.avg(d2,beta=T)
# #combine results of model average and final model
# avg2.mat<-exp(avg2$avg.model[,c(1,4,5)])
# comb2.mat<-cbind(m2mat[,1:3],avg2.mat)
# xtable(comb2.mat)
# avg2$importance

model2ropt<-glm(event2~TOTCHOL_3+sex+AGE_3+SYSBP_3+quit+CURSMOKE_3,data=m2dat1,family=binomial(link='logit'))
sm2<-summary(model2ropt)
m2or<-exp(sm2$coef[,1])
m2lci<-exp(sm2$coef[,1]-1.96*sm2$coef[,2])
m2uci<-exp(sm2$coef[,1]+1.96*sm2$coef[,2])
m2mat<-matrix(c(m2or,m2lci,m2uci,sm2$coef[,4]),length(sm2$coef[,1]),4)
row.names(m2mat)<-c("(Intercept","Total Cholesterol","Smoking Status","Males","Age","Systolic BP","Smoking Cessation", "Males*Age")
colnames(m2mat)<-c("OR","Lower CI","Upper CI", "p-value")
xtable(m2mat)

#evaluate age*sex interaction
#refit model without HR curve for simplicity
mint<-glm(event~TOTCHOL_2+sex*AGE_2+CIGPDAY_2+BMI_2+factor(BPMEDS_2)*SYSBP_2+HEARTRTE_2+factor(DIABETES_2),data=m1dat,family=binomial(link='logit'),na.actio="na.fail")
sm1b<-summary(mint)
ageint<-function(x,sex){ #calulate probability of event holding other variables at mean values and no BP meds or diabetes
  lp<-sm1b$coef[1] + sm1b$coef[2]*mean(m1dat$TOTCHOL_2) + sm1b$coef[3]*sex +sm1b$coef[4]*x +sm1b$coef[5]*mean(m1dat$CIGPDAY_2) +sm1b$coef[6]*mean(m1dat$BMI_2) +sm1b$coef[8]*mean(m1dat$SYSBP_2) +sm1b$coef[9]*mean(m1dat$HEARTRTE_2)+ sm1b$coef[11]*x*sex
  prob<-exp(lp)/(1+exp(lp))
  return(lp)#using the linear predictor more clearly shows the interaction
}
curve(ageint(x,sex=1),xlim=c(39,76),ylim=c(-5.5,-2.1),xlab="Age (years)",ylab="Log(Odds) of CVD")
curve(ageint(x,sex=0),xlim=c(39,76),add=T,col="red")
