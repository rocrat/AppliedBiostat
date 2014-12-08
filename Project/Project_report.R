
## ----ReadIn,echo=FALSE,message=FALSE,warning=FALSE-----------------------
#read in and check data 
rm(list=ls())
library(car)
library(ggplot2)
library(ggmap)
library(MASS)
longdat<-read.csv("C:/Classes/AppliedBiostat/Project/Domconv_918.csv")[1:17823,]
longdat$site<-Recode(longdat$site,"c('High','HIGH')='High'")
longdat$Villcodef<-as.factor(longdat$Villcode)
longdat$Housef<-as.factor(longdat$House)
#create a variable which combines subcode, villcode, and house
longdat$household<-factor(with(longdat, paste(Subcode, Villcode, House, sep="_")))
longdat$latlon<-factor(with(longdat, paste(Lat, Long, sep="_")))
# There are more unique housholds than latlons!
# strsplit(unique(dat$latlon),split="_") #get unique lat lons in a list
longdat$Age<-as.numeric(as.character(longdat$Age))#introducing NAs on purpose
longdat$HHhead<-Recode(longdat$HHhead,"c('F','M','N/A')='N'")
longdat$Relate<-as.numeric(as.character(Recode(longdat$Relate,"'N/A'=NA")))
longdat$HHspray<-Recode(longdat$HHspray,"c('','4','N/A')=NA")
longdat$NetHH<-Recode(longdat$NetHH,"'N/A'=NA")
longdat$NetHH<-droplevels(longdat$NetHH)


## ----eliminateDuplicates,echo=FALSE,eval=FALSE---------------------------
## #find duplicates entries within households
houses<-factor(unique(longdat$household))
N<-length(houses)
#loop through households and find entires that have unique age sex combos
longdat$age_sex<-factor(paste(longdat$Age,longdat$Sex,sep="_"))
subdat<-list(length=N)
for( i in 1:N){
  #limit to 1 household
  housedat<-longdat[longdat$household==houses[i],]
  include<-which(!duplicated(housedat$age_sex))
  subdat[[i]]<-housedat[include,]
}
datunique<-do.call(rbind,subdat)
rm(subdat)


## ----summarizeHouseHolds, echo=FALSE, eval=FALSE-------------------------
## #This chunk is to create a new data set which is based on the household
numocc<-numFem<-numMale<-oldest<-youngest<-headsex<-headage<-net<-under5<-under1<-under18<-over65<-periph<-lat<-lon<-hspray<-site<-elev<-lang<-rep(0,length=N)
## #loop through each unique household and calculate summary variables
for (i in 1:N){
  numocc[i]<-length(which(datunique$household==houses[i]))
  numFem[i]<-sum(datunique$household==houses[i] & datunique$Sex=="F",na.rm=T)
  numMale[i]<-sum(datunique$household==houses[i] & datunique$Sex=="M",na.rm=T)
  oldest[i]<-max(datunique[datunique$household==houses[i],]$Age)
  youngest[i]<-min(datunique[datunique$household==houses[i],]$Age)
  if(length(datunique[(datunique$household==houses[i] & datunique$HHhead=="Y"),]$Sex)==1){
    headsex[i]<-as.character(datunique[(datunique$household==houses[i] & datunique$HHhead=="Y"),]$Sex)
  }else{headsex[i]<-NA}
  if(length(datunique[datunique$household==houses[i]& datunique$HHhead=="Y",]$Age)==1){
    headage[i]<-datunique[datunique$household==houses[i]& datunique$HHhead=="Y",]$Age
  }else{headage[i]<-NA}
  netans<-as.character(datunique[datunique$household==houses[i],]$NetHH)
  net[i]<-sum(netans=="Y",na.rm=T)
  under5[i]<-sum(datunique[datunique$household==houses[i],]$Age>1 & datunique[datunique$household==houses[i],]$Age<=5,na.rm=T)#children >1 and less than 5
  under1[i]<-sum(datunique[datunique$household==houses[i],]$Age<=1,na.rm=T)
  under18[i]<-sum(datunique[datunique$household==houses[i],]$Age<18,na.rm=T)
  over65[i]<-sum(datunique[datunique$household==houses[i],]$Age>=65,na.rm=T)
  family<-datunique[datunique$household==houses[i],]$Relate
  periph[i]<-any(family>9)#classify any one more distant than grandchild as periphery
  lat[i]<-datunique[datunique$household==houses[i],]$Lat[1]
  lon[i]<-datunique[datunique$household==houses[i],]$Long[1]
  spray<-datunique[datunique$household==houses[i],]$HHspray
  hspray[i]<-sum(spray=="Y",na.rm=T)
  site[i]<-datunique[datunique$household==houses[i],]$site[1]
  elev[i]<-datunique[datunique$household==houses[i],]$Elevation[1]
  lang[i]<-datunique[datunique$household==houses[i],]$Lang[1]

}
#create data frame from new data vectors
hdat<-data.frame(houses,numocc,numFem,numMale,oldest,youngest,headsex,headage,net,under5,under1,under18,over65,periph,lat,lon,hspray,site,elev,lang)
#save new data to csv file so I don't have to run this costly loop again
write.csv(hdat,"C:/Classes/AppliedBiostat/Project/housedata.csv")


## ----importNewData,echo=FALSE,message=FALSE,results='asis'---------------
rm(list=ls())
library(BDSS)
library(pander)
hdat<-read.csv("C:/Classes/AppliedBiostat/Project/housedata.csv")
sumdat<-hdat
sumdat$net<-factor(ifelse(is.na(sumdat$net),NA,ifelse(sumdat$net==0,"No","Yes")))
sumdat$under5f<-factor(ifelse(is.na(sumdat$under5),NA,ifelse(sumdat$under5>=1,"Yes","No")))
sumdat$under1f<-factor(ifelse(is.na(sumdat$under1),NA,ifelse(sumdat$under1>=1,"Yes","No")))
sumdat$under18f<-factor(ifelse(is.na(sumdat$under18),NA,ifelse(sumdat$under18>=1,"Yes","No")))
sumdat$over65f<-factor(ifelse(is.na(sumdat$under18),NA,ifelse(sumdat$over65>=1,"Yes","No")))
sumdat$periph<-factor(ifelse(is.na(sumdat$periph),NA,ifelse(sumdat$periph==1,"Yes","No")))
sumdat$hspray<-factor(ifelse(is.na(sumdat$hspray),NA,ifelse(sumdat$hspray==0,"No","Yes")))
sumdat$site<-factor(ifelse(is.na(sumdat$site),NA,ifelse(sumdat$site==1,"High","Low")))
sumdat[which(sumdat$site=="Low" & sumdat$lat>0),]$site<-"High"#correct mis-labelled site


## ----HouseSummary,echo=FALSE,results='asis',eval=FALSE-------------------
SummaryTable(data=sumdat,rowvars=c("under5f","under1f","under18f","over65f","periph","hspray","site","headsex"),row.names=c("Under 5","Under 1","Under 18","Peripheral Family","House Sprayed","Site","House Head Sex"),colvar="net",cont.vars=c("numocc","numFem","numMale","oldest","youngest","headage","elev"),output="rmarkdown")


## ----CalcAgeRisk,echo=FALSE----------------------------------------------
sumdat$arisk<-with(sumdat,(2*under1)+under5+over65)
sumdat$ariskstd<-scale(log(sumdat$arisk+1),center=F)
#for sensitivity analysis exclude elderly group
sumdat$arisks<-with(sumdat,(2*under1)+under5)
sumdat$ariskstds<-scale(log(sumdat$arisks+1),center=F)


## ----getDEM,echo=FALSE,message=FALSE,message=FALSE,results='hide'--------
library(raster,quietly=TRUE,warn.conflicts=FALSE)
library(ggplot2)
demlow<-getData(name='SRTM',download=FALSE,path="C:/Classes/AppliedBiostat/Project",lon=34.98,lat=-.08)
demlow2<-getData(name='SRTM',download=FALSE,path="C:/Classes/AppliedBiostat/Project",lon=35.98,lat=-.08)
#plot(dem)
demhigh<-getData(name='SRTM',download=FALSE,path="C:/Classes/AppliedBiostat/Project",lon=34.965,lat=.2)
lowel<-readGDAL("C:/Classes/AppliedBiostat/Project/srtm_43_13.tif")
lowel2<-readGDAL("C:/Classes/AppliedBiostat/Project/srtm_44_13.tif")
highel<-readGDAL("C:/Classes/AppliedBiostat/Project/srtm_43_12.tif")
#shrink area
lowext<-extent(34.950000,35.00042,-0.110000,-0.050000)
lowel<-crop(demlow,lowext,"C:/Classes/AppliedBiostat/Project/LowSiteData",overwrite=TRUE)
lowext2<-extent(35.000000,35.020000,-0.110000,-0.050000)
lowel2<-crop(demlow2,lowext2,"C:/Classes/AppliedBiostat/Project/LowSiteData2",overwrite=TRUE)
# image(lowel)
highext<-extent(34.900000,35.000417,0.150000,0.250000)
highel<-crop(demhigh,highext,"C:/Classes/AppliedBiostat/Project/HighSiteData",overwrite=TRUE)

lowelcomp<-merge(lowel,lowel2)
writeGDAL(as(lowelcomp,"SpatialPixelsDataFrame"),fname="LowDEM.tif")
writeGDAL(as(highel,"SpatialPixelsDataFrame"),fname="HighDEM.tif")

lowsite<-sumdat[sumdat$site=="Low",]
lowp<-data.frame(rasterToPoints(lowelcomp))
ggplot(data=lowp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1200, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=hspray),data=lowsite[-565,],alpha=0.4)+scale_colour_manual("Spray",values=c("red","blue"))

highsite<-sumdat[sumdat$site=="High",]
highp<-data.frame(rasterToPoints(highel))
ggplot(data=highp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1714, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=hspray),data=highsite,alpha=0.4)+scale_colour_manual("Spray",values=c("red","blue"))


## ----computeSWI,echo=FALSE,message=FALSE,warning=FALSE,results='hide'----

library(SDMTools)
#calculate elevation variance
#first get diff from mean to deterime if it is a dip or a peak
#create 3 different windows for calculating dips, this will account for drainage area to some extent
meanmat3<-matrix(1/9,3,3)
meanmat7<-matrix(1/49,7,7)
meanmat11<-matrix(1/121,11,11)
#find dips at these three different windows
meanel3<-focal(lowelcomp,w=meanmat3)#,pad=T,pad.value=mean(values(lowelcomp)))
meandiff3<-lowelcomp-meanel3
values(meandiff3)<-ifelse(values(meandiff3)<0,values(meandiff3),0)

meanel7<-focal(lowelcomp,w=meanmat7)#,pad=T,pad.value=mean(values(lowelcomp)))
meandiff7<-lowelcomp-meanel7
values(meandiff7)<-ifelse(values(meandiff7)<0,values(meandiff7),0)

meanel11<-focal(lowelcomp,w=meanmat11)#,pad=T,pad.value=mean(values(lowelcomp)))
meandiff11<-lowelcomp-meanel11
values(meandiff11)<-ifelse(values(meandiff11)<0,values(meandiff11),0)

lowdips<-meandiff3+meandiff7+meandiff11

#calculate aspect of each cell
lowasp<-aspect(lowelcomp,latlon=TRUE)
lowaspc<-lowasp
#categorize into quadrants
values(lowaspc)<-ifelse(values(lowasp)>45 & values(lowasp) <= 135,1,
                        ifelse(values(lowasp)>135 & values(lowasp) <= 225,2,
                               ifelse(values(lowasp)>225 & values(lowasp) <= 315,3,
                                      ifelse(values(lowasp)>315 & values(lowasp) <=360,4,
                                             ifelse(values(lowasp)>0 & values(lowasp) <= 45,4,NA)))))
#Calculate aspect variance
aspmat3<-matrix(1,3,3)
aspmat7<-matrix(1,7,7)
aspmat11<-matrix(1,11,11)
aspvar3<-focal(lowaspc,aspmat3,fun=var,na.rm=T)
aspvar7<-focal(lowaspc,aspmat7,fun=var,na.rm=T)
aspvar11<-focal(lowaspc,aspmat11,fun=var,na.rm=T)

lowaspvar<-aspvar3+aspvar7+aspvar11
lowtwi<-lowaspvar*lowdips*-1

#repeat for High site
meanel3<-focal(highel,w=meanmat3)#,pad=T,pad.value=mean(values(highel)))
meandiff3<-highel-meanel3
values(meandiff3)<-ifelse(values(meandiff3)<0,values(meandiff3),0)

meanel7<-focal(highel,w=meanmat7)#,pad=T,pad.value=mean(values(highel)))
meandiff7<-highel-meanel7
values(meandiff7)<-ifelse(values(meandiff7)<0,values(meandiff7),0)

meanel11<-focal(highel,w=meanmat11)#,pad=T,pad.value=mean(values(highel)))
meandiff11<-highel-meanel11
values(meandiff11)<-ifelse(values(meandiff11)<0,values(meandiff11),0)

highdips<-meandiff3+meandiff7+meandiff11

#Repeat for High site
highasp<-aspect(highel,latlon=TRUE)
highaspc<-highasp
#categorize into quadrants
values(highaspc)<-ifelse(values(highasp)>45 & values(highasp) <= 135,1,
                        ifelse(values(highasp)>135 & values(highasp) <= 225,2,
                               ifelse(values(highasp)>225 & values(highasp) <= 315,3,
                                      ifelse(values(highasp)>315 & values(highasp) <=360,4,
                                             ifelse(values(highasp)>0 & values(highasp) <= 45,4,NA)))))
#Calculate aspect variance
aspmat3<-matrix(1,3,3)
aspmat7<-matrix(1,7,7)
aspmat11<-matrix(1,11,11)
aspvar3<-focal(highaspc,aspmat3,fun=var,na.rm=T)
aspvar7<-focal(highaspc,aspmat7,fun=var,na.rm=T)
aspvar11<-focal(highaspc,aspmat11,fun=var,na.rm=T)

highaspvar<-aspvar3+aspvar7+aspvar11
hightwi<-highaspvar*highdips*-1
#plot restricted TWI
htwi<-as.data.frame(hightwi,xy=TRUE)
ltwi<-as.data.frame(lowtwi,xy=TRUE)
ggplot(data=ltwi[complete.cases(ltwi),],aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Wetness",low = "green", mid = "yellow", high = "red",  midpoint =15, space = "rgb", na.value = "grey50", guide = "colourbar")
# 
ggplot(data=htwi[complete.cases(htwi),],aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Wetness",low = "green", mid = "yellow", high = "red",  midpoint =max(htwi$layer,na.rm=TRUE)/2, space = "rgb", na.value = "grey50", guide = "colourbar")


lowTWI<-readGDAL("C:/Classes/AppliedBiostat/Project/LowTWI.tif")#TWI calculated by SAGA
highTWI<-readGDAL("C:/Classes/AppliedBiostat/Project/HighTWI.tif")
lowt<-data.frame(lowTWI)
ggplot(data=lowt,aes(x=x,y=y))+geom_raster(aes(fill=band1))+scale_fill_gradient2("Wetness",low = "green", mid = "yellow", high = "red",  midpoint =-8, space = "rgb", na.value = "grey50", guide = "colourbar")#+ geom_point(aes(x=lon, y=lat,color=hspray),data=lowsite[-565,])+scale_colour_manual("Spray",values=c("green","blue"))

hight<-data.frame(highTWI)
ggplot(data=hight,aes(x=x,y=y))+geom_raster(aes(fill=band1))+scale_fill_gradient2("Wetness",low = "green", mid = "yellow", high = "red",  midpoint =-5, space = "rgb", na.value = "grey50", guide = "colourbar")#+ geom_point(aes(x=lon, y=lat,color=hspray),data=highsite)+scale_colour_manual("Spray",values=c("red","blue"))


## ----smoothTWI,echo=FALSE------------------------------------------------
#use focal to create moving window with gaussian weights for assiging household risk
#create filter for focal window
r<-raster(ncols=36,nrows=36,xmn=0)
#convert TWI to raster
lowtr<-raster(lowTWI,layer="band1")
hightr<-raster(highTWI,layer="band1")
fmat<-focalWeight(r,d=10,type='Gauss')
mriskLow<-focal(lowtr,fmat,filename='C:/Classes/AppliedBiostat/Project/smoothLowTWI',pad=T,padValue=median(lowt$band1),overwrite=T)
mriskHigh<-focal(hightr,fmat,filename='C:/Classes/AppliedBiostat/Project/smoothHighTWI',pad=T,padValue=median(hight$band1),overwrite=T)

lowtrp<-as.data.frame(mriskLow,xy=T)
hightrp<-as.data.frame(mriskHigh,xy=T)
ggplot(data=lowtrp[complete.cases(lowtrp),],aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Mosquito Risk",low = "green", mid = "yellow", high = "red",  midpoint = -12, space = "rgb", na.value = "grey50", guide = "colourbar")+theme(legend.position='bottom')+ geom_point(aes(x=lon, y=lat),data=lowsite[-565,],alpha=0.9)
ggplot(data=hightrp[complete.cases(hightrp),],aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Mosquito Risk",low = "green", mid = "yellow", high = "red",  midpoint = median(hightrp$layer,na.rm=T), space = "rgb", na.value = "grey50", guide = "colourbar")+theme(legend.position='bottom')+ geom_point(aes(x=lon, y=lat),data=highsite,alpha=0.8)


## ----assignRisktoHouse,echo=FALSE----------------------------------------
lowsite$mrisk<-extract(mriskLow,y=cbind(lowsite$lon,lowsite$lat))
highsite$mrisk<-extract(mriskHigh,y=cbind(highsite$lon,highsite$lat))
#Standardize the risk scores
lowsite$mriskpos<-lowsite$mrisk+abs(min(lowsite$mrisk,na.rm=T))
highsite$mriskpos<-highsite$mrisk+abs(min(highsite$mrisk,na.rm=T))
lowsite$mriskstd<-scale(log(lowsite$mriskpos+1),center=F)
highsite$mriskstd<-scale(log(highsite$mriskpos+1),center=F)


## ----plotMrisk,echo=FALSE,eval=FALSE-------------------------------------
ggplot(data=lowp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1200, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=mriskstd),data=lowsite[-565,],alpha=0.9)+scale_colour_gradient("Mosquito Risk",low="blue",high="red")

ggplot(data=highp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1714, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=mriskstd),data=highsite,alpha=0.8)+scale_colour_gradient("Mosquito Risk",low="blue",high="red")


## ----plotArisk,echo=FALSE,eval=FALSE-------------------------------------
ggplot(data=lowp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1200, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=arisk),data=lowsite[-565,],alpha=0.9)+scale_colour_gradient("Age Risk",low="blue",high="red")

ggplot(data=highp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1714, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=arisk),data=highsite,alpha=0.8)+scale_colour_gradient("Age Risk",low="blue",high="red")


## ----CombinedRisk,echo=FALSE---------------------------------------------
lowsite$comrsk<-lowsite$mriskstd+lowsite$arisk
highsite$comrsk<-highsite$mriskstd+highsite$arisk


## ----plotCombrisk,echo=FALSE,fig.height=3,fig.width=3,eval=FALSE---------
ggplot(data=lowp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1200, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=comrsk),data=lowsite[-565,],alpha=0.9)+scale_colour_gradient("Combined Risk",low="blue",high="red")+theme(legend.position='bottom',legend.box='horizontal')

ggplot(data=highp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1714, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=comrsk),data=highsite,alpha=0.8)+scale_colour_gradient("Combined Risk",low="blue",high="red")+theme(legend.position='bottom',legend.box='horizontal')


## ----checkLinearityAssumption,echo=FALSE,results='hide',eval=FALSE-------
lowsite$hspray<-ifelse(lowsite$hspray=="No",0,1)
lowsite$net<-ifelse(lowsite$net=="No",0,1)
highsite$hspray<-ifelse(highsite$hspray=="No",0,1)
highsite$net<-ifelse(highsite$net=="No",0,1)

rcspline.plot(lowsite$comrsk,lowsite$net,model="logistic",xlab="Combined Risk",ylab="Log Odds Net Use")#evidence of non-linearity (u-shape)
rcspline.plot(highsite$comrsk,highsite$net,model="logistic",xlab="Combined Risk")
rcspline.plot(lowsite$comrsk,lowsite$hspray,model="logistic")
rcspline.plot(highsite$comrsk,highsite$hspray,model="logistic")

rcspline.plot(lowsite$arisk,lowsite$net,model="logistic")
rcspline.plot(highsite$arisk,highsite$net,model="logistic")

rcspline.plot(lowsite$arisk,lowsite$hspray,model="logistic")
rcspline.plot(highsite$arisk,highsite$hspray,model="logistic")

rcspline.plot(lowsite$mrisk,lowsite$net,model="logistic")
rcspline.plot(highsite$mrisk,highsite$net,model="logistic",xlab="Mosquito Risk")#evidence of non-linearity increasing, leveling off then slight decrease
rcspline.plot(lowsite$mrisk,lowsite$hspray,model="logistic")
rcspline.plot(highsite$mrisk,highsite$hspray,model="logistic",xlab="Mosquito Risk")#evidence of non-linearity (increasing then leveling off)


## ----model1,echo=FALSE---------------------------------------------------
m1ls<-glm(hspray~comrsk,data=lowsite,family="binomial")
m1ls.est<-c(exp(summary(m1ls)$coef[2,1]),exp(summary(m1ls)$coef[2,1]-1.96*summary(m1ls)$coef[2,2]),exp(summary(m1ls)$coef[2,1]+1.96*summary(m1ls)$coef[2,2]))
m1ln<-glm(net~comrsk,data=lowsite,family="binomial")
m1ln.est<-c(exp(summary(m1ln)$coef[2,1]),exp(summary(m1ln)$coef[2,1]-1.96*summary(m1ln)$coef[2,2]),exp(summary(m1ln)$coef[2,1]+1.96*summary(m1ln)$coef[2,2]))
#adjust for non-linearity
lq1<-quantile(lowsite$comrsk,prob=.25)
lq2<-quantile(lowsite$comrsk,prob=.5)
lq3<-quantile(lowsite$comrsk,prob=.75)
lowsite$comrsk_q<-ifelse(lowsite$comrsk <= lq1,1,ifelse(lowsite$comrsk>lq1 & lowsite$comrsk<=lq2,2,ifelse(lowsite$comrsk>lq2 & lowsite$comrsk<=lq3,3,4)))
m1lnq<-glm(net~factor(comrsk_q),data=lowsite,family="binomial")
# summary(m1lnq)

m1hs<-glm(hspray~comrsk,data=highsite,family="binomial")
m1hs.est<-c(exp(summary(m1hs)$coef[2,1]),exp(summary(m1hs)$coef[2,1]-1.96*summary(m1hs)$coef[2,2]),exp(summary(m1hs)$coef[2,1]+1.96*summary(m1hs)$coef[2,2]))
m1hn<-glm(net~comrsk,data=highsite,family="binomial")
m1hn.est<-c(exp(summary(m1hn)$coef[2,1]),exp(summary(m1hn)$coef[2,1]-1.96*summary(m1hn)$coef[2,2]),exp(summary(m1hn)$coef[2,1]+1.96*summary(m1hn)$coef[2,2]))

Treatment<-rep(c("Net","Spray"),2)
Site<-c("High","","Low","")
outmat<-round(matrix(c(m1hn.est,m1hs.est,m1ln.est,m1ls.est),4,3,byrow=T),2)
colnames(outmat)<-c("OR","Lower 95% CI","Upper 95% CI")
obj1out<-cbind(Site,Treatment,outmat)


## ----Obj1Table,echo=FALSE,results='asis'---------------------------------
pandoc.table(obj1out,type="rmarkdown",split.tables=Inf)


## ----SepModels,echo=FALSE------------------------------------------------
#Separate model for each risk type
m2als<-glm(hspray~arisk,data=lowsite,family="binomial")
m2als.est<-c(exp(summary(m2als)$coef[2,1]),exp(summary(m2als)$coef[2,1]-1.96*summary(m2als)$coef[2,2]),exp(summary(m2als)$coef[2,1]+1.96*summary(m2als)$coef[2,2]))
m2aln<-glm(net~arisk,data=lowsite,family="binomial")
m2aln.est<-c(exp(summary(m2aln)$coef[2,1]),exp(summary(m2aln)$coef[2,1]-1.96*summary(m2aln)$coef[2,2]),exp(summary(m2aln)$coef[2,1]+1.96*summary(m2aln)$coef[2,2]))
m2mls<-glm(hspray~mrisk,data=lowsite,family="binomial")
m2mls.est<-c(exp(summary(m2mls)$coef[2,1]),exp(summary(m2mls)$coef[2,1]-1.96*summary(m2mls)$coef[2,2]),exp(summary(m2mls)$coef[2,1]+1.96*summary(m2mls)$coef[2,2]))
m2mln<-glm(net~mrisk,data=lowsite,family="binomial")
m2mln.est<-c(exp(summary(m2mln)$coef[2,1]),exp(summary(m2mln)$coef[2,1]-1.96*summary(m2mln)$coef[2,2]),exp(summary(m2mln)$coef[2,1]+1.96*summary(m2mln)$coef[2,2]))

m2ahs<-glm(hspray~arisk,data=highsite,family="binomial")
m2ahs.est<-c(exp(summary(m2ahs)$coef[2,1]),exp(summary(m2ahs)$coef[2,1]-1.96*summary(m2ahs)$coef[2,2]),exp(summary(m2ahs)$coef[2,1]+1.96*summary(m2ahs)$coef[2,2]))
m2ahn<-glm(net~arisk,data=highsite,family="binomial")
m2ahn.est<-c(exp(summary(m2ahn)$coef[2,1]),exp(summary(m2ahn)$coef[2,1]-1.96*summary(m2ahn)$coef[2,2]),exp(summary(m2ahn)$coef[2,1]+1.96*summary(m2ahn)$coef[2,2]))
m2mhs<-glm(hspray~mrisk,data=highsite,family="binomial")
m2mhs.est<-c(exp(summary(m2mhs)$coef[2,1]),exp(summary(m2mhs)$coef[2,1]-1.96*summary(m2mhs)$coef[2,2]),exp(summary(m2mhs)$coef[2,1]+1.96*summary(m2mhs)$coef[2,2]))
m2mhn<-glm(net~mrisk,data=highsite,family="binomial")
m2mhn.est<-c(exp(summary(m2mhn)$coef[2,1]),exp(summary(m2mhn)$coef[2,1]-1.96*summary(m2mhn)$coef[2,2]),exp(summary(m2mhn)$coef[2,1]+1.96*summary(m2mhn)$coef[2,2]))

sepmat<-round(matrix(c(m2ahn.est,m2mhn.est,m2ahs.est,m2mhs.est,m2aln.est,m2mln.est,m2als.est,m2mls.est),4,6,byrow=T),2)
colnames(sepmat)<-c(rep("Age Risk",3),rep("Mosquito Risk",3))
est<-c("","",rep(c("OR","Lower 95% CI","Upper 95% CI"),2))
sepout<-cbind(Site,Treatment,sepmat)
sepout<-rbind(est,sepout)
rownames(sepout)<-1:nrow(sepout)


## ----objective2Table,echo=FALSE,results='asis'---------------------------
pandoc.table(sepout,type="rmarkdown",split.tables=Inf)


## ----smoothTWI_sens,echo=FALSE-------------------------------------------
#use focal to create moving window with gaussian weights for assiging household risk
#create filter for focal window
r<-raster(ncols=36,nrows=36,xmn=0)
#convert TWI to raster
lowtr_s<-lowtwi
hightr_s<-hightwi
fmat<-focalWeight(r,d=10,type='Gauss')
mriskLows<-focal(lowtr_s,fmat,filename='smoothLowTWI_sens',pad=T,padValue=median(lowt$band1),overwrite=T)
mriskHighs<-focal(hightr_s,fmat,filename='smoothHighTWI_sens',pad=T,padValue=median(hight$band1),overwrite=T)

lowtrps<-as.data.frame(mriskLows,xy=T)
hightrps<-as.data.frame(mriskHighs,xy=T)
ggplot(data=lowtrps[complete.cases(lowtrps),],aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Mosquito Risk",low = "green", mid = "yellow", high = "red",  midpoint = median(lowtrps$layer,na.rm=T), space = "rgb", na.value = "grey50", guide = "colourbar")+theme(legend.position='bottom')+ geom_point(aes(x=lon, y=lat),data=lowsite[-565,],alpha=0.9)
ggplot(data=hightrps[complete.cases(hightrps),],aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Mosquito Risk",low = "green", mid = "yellow", high = "red",  midpoint = median(hightrps$layer,na.rm=T), space = "rgb", na.value = "grey50", guide = "colourbar")+theme(legend.position='bottom')+ geom_point(aes(x=lon, y=lat),data=highsite[-2537,],alpha=0.8)


## ----assignRisktoHouse_sens,echo=FALSE-----------------------------------
lowsite$mrisks<-extract(mriskLows,y=cbind(lowsite$lon,lowsite$lat))
highsite$mrisks<-extract(mriskHighs,y=cbind(highsite$lon,highsite$lat))
#Standardize the risk scores
lowsite$mriskstds<-scale(log(lowsite$mrisks+1),center=F)
highsite$mriskstds<-scale(log(highsite$mrisks+1),center=F)


## ----plotMrisk_sens,echo=FALSE,eval=FALSE--------------------------------
ggplot(data=lowp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1200, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=mrisks),data=lowsite,alpha=0.9)+scale_colour_gradient("Mosquito Risk",low="blue",high="red")

ggplot(data=highp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1714, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=mrisks),data=highsite,alpha=0.8)+scale_colour_gradient("Mosquito Risk",low="blue",high="red")


## ----CombinedRisk_sens,echo=FALSE----------------------------------------
lowsite$comrsks<-scale(log(lowsite$mrisks+1),center=F)+scale(log(lowsite$arisk+1),center=F)
highsite$comrsks<-scale(log(highsite$mrisks+1),center=F)+scale(log(highsite$arisk+1),center=F)


## ----plotCombrisk_sens,echo=FALSE,fig.height=7,fig.width=7,eval=FALSE----
ggplot(data=lowp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1200, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=comrsks),data=lowsite,alpha=0.9)+scale_colour_gradient("Combined Risk",low="blue",high="red")+theme(legend.position='bottom',legend.box='horizontal')

ggplot(data=highp,aes(x=x,y=y))+geom_raster(aes(fill=layer))+scale_fill_gradient2("Elevation",low = "brown", mid = "yellow", high = "green",  midpoint = 1714, space = "rgb", na.value = "grey50", guide = "colourbar")+ geom_point(aes(x=lon, y=lat,color=comrsks),data=highsite,alpha=0.8)+scale_colour_gradient("Combined Risk",low="blue",high="red")+theme(legend.position='bottom',legend.box='horizontal')


## ----checkLinearityAssumption_sens,echo=FALSE,results='hide',eval=FALSE----
rcspline.plot(lowsite$comrsks,lowsite$net,model="logistic",xlab="Combined Risk",ylab="Log Odds Net Use")#evidence of non-linearity (u-shape)
rcspline.plot(highsite$comrsks,highsite$net,model="logistic",xlab="Combined Risk")
rcspline.plot(lowsite$comrsks,lowsite$hspray,model="logistic")
rcspline.plot(highsite$comrsks,highsite$hspray,model="logistic")

rcspline.plot(lowsite$arisk,lowsite$net,model="logistic")
rcspline.plot(highsite$arisk,highsite$net,model="logistic")
rcspline.plot(lowsite$arisk,lowsite$hspray,model="logistic")
rcspline.plot(highsite$arisk,highsite$hspray,model="logistic")

rcspline.plot(lowsite$mrisks,lowsite$net,model="logistic")
rcspline.plot(highsite$mrisks,highsite$net,model="logistic",xlab="Mosquito Risk")
rcspline.plot(lowsite$mrisks,lowsite$hspray,model="logistic")
rcspline.plot(highsite$mrisks,highsite$hspray,model="logistic",xlab="Mosquito Risk")#evidence of non-linearity (increasing then leveling off)


## ----model1_sens,echo=FALSE----------------------------------------------
m1ls<-glm(hspray~comrsks,data=lowsite,family="binomial")
m1ls.est<-c(exp(summary(m1ls)$coef[2,1]),exp(summary(m1ls)$coef[2,1]-1.96*summary(m1ls)$coef[2,2]),exp(summary(m1ls)$coef[2,1]+1.96*summary(m1ls)$coef[2,2]))
m1ln<-glm(net~comrsks,data=lowsite,family="binomial")
m1ln.est<-c(exp(summary(m1ln)$coef[2,1]),exp(summary(m1ln)$coef[2,1]-1.96*summary(m1ln)$coef[2,2]),exp(summary(m1ln)$coef[2,1]+1.96*summary(m1ln)$coef[2,2]))

m1hs<-glm(hspray~comrsks,data=highsite,family="binomial")
m1hs.est<-c(exp(summary(m1hs)$coef[2,1]),exp(summary(m1hs)$coef[2,1]-1.96*summary(m1hs)$coef[2,2]),exp(summary(m1hs)$coef[2,1]+1.96*summary(m1hs)$coef[2,2]))
m1hn<-glm(net~comrsks,data=highsite,family="binomial")
m1hn.est<-c(exp(summary(m1hn)$coef[2,1]),exp(summary(m1hn)$coef[2,1]-1.96*summary(m1hn)$coef[2,2]),exp(summary(m1hn)$coef[2,1]+1.96*summary(m1hn)$coef[2,2]))

Treatment<-rep(c("Net","Spray"),2)
Site<-c("High","","Low","")
outmats<-round(matrix(c(m1hn.est,m1hs.est,m1ln.est,m1ls.est),4,3,byrow=T),2)
colnames(outmats)<-c("OR","Lower 95% CI","Upper 95% CI")
obj1outs<-cbind(Site,Treatment,outmat,outmats)


## ----Obj1Table_sens,echo=FALSE,results='asis'----------------------------
pandoc.table(obj1outs,type="rmarkdown",split.tables=Inf)


## ----SepModels_sens,echo=FALSE-------------------------------------------
#Separate model for each risk type
m2als<-glm(hspray~arisk,data=lowsite,family="binomial")
m2als.est<-c(exp(summary(m2als)$coef[2,1]),exp(summary(m2als)$coef[2,1]-1.96*summary(m2als)$coef[2,2]),exp(summary(m2als)$coef[2,1]+1.96*summary(m2als)$coef[2,2]))
m2aln<-glm(net~arisk,data=lowsite,family="binomial")
m2aln.est<-c(exp(summary(m2aln)$coef[2,1]),exp(summary(m2aln)$coef[2,1]-1.96*summary(m2aln)$coef[2,2]),exp(summary(m2aln)$coef[2,1]+1.96*summary(m2aln)$coef[2,2]))
m2mls<-glm(hspray~arisks,data=lowsite,family="binomial")
m2mls.est<-c(exp(summary(m2mls)$coef[2,1]),exp(summary(m2mls)$coef[2,1]-1.96*summary(m2mls)$coef[2,2]),exp(summary(m2mls)$coef[2,1]+1.96*summary(m2mls)$coef[2,2]))
m2mln<-glm(net~arisks,data=lowsite,family="binomial")
m2mln.est<-c(exp(summary(m2mln)$coef[2,1]),exp(summary(m2mln)$coef[2,1]-1.96*summary(m2mln)$coef[2,2]),exp(summary(m2mln)$coef[2,1]+1.96*summary(m2mln)$coef[2,2]))

m2ahs<-glm(hspray~arisk,data=highsite,family="binomial")
m2ahs.est<-c(exp(summary(m2ahs)$coef[2,1]),exp(summary(m2ahs)$coef[2,1]-1.96*summary(m2ahs)$coef[2,2]),exp(summary(m2ahs)$coef[2,1]+1.96*summary(m2ahs)$coef[2,2]))
m2ahn<-glm(net~arisk,data=highsite,family="binomial")
m2ahn.est<-c(exp(summary(m2ahn)$coef[2,1]),exp(summary(m2ahn)$coef[2,1]-1.96*summary(m2ahn)$coef[2,2]),exp(summary(m2ahn)$coef[2,1]+1.96*summary(m2ahn)$coef[2,2]))
m2mhs<-glm(hspray~arisks,data=highsite,family="binomial")
m2mhs.est<-c(exp(summary(m2mhs)$coef[2,1]),exp(summary(m2mhs)$coef[2,1]-1.96*summary(m2mhs)$coef[2,2]),exp(summary(m2mhs)$coef[2,1]+1.96*summary(m2mhs)$coef[2,2]))
m2mhn<-glm(net~arisks,data=highsite,family="binomial")
m2mhn.est<-c(exp(summary(m2mhn)$coef[2,1]),exp(summary(m2mhn)$coef[2,1]-1.96*summary(m2mhn)$coef[2,2]),exp(summary(m2mhn)$coef[2,1]+1.96*summary(m2mhn)$coef[2,2]))

sepmats<-round(matrix(c(m2ahn.est,m2mhn.est,m2ahs.est,m2mhs.est,m2aln.est,m2mln.est,m2als.est,m2mls.est),4,6,byrow=T),2)
colnames(sepmats)<-c(rep("Age Risk (<5 or >65)",3),rep("Age Risk (<5)",3))
est<-c("","",rep(c("OR","Lower 95% CI","Upper 95% CI"),2))
sepouts<-cbind(Site,Treatment,sepmats)
sepouts<-rbind(est,sepouts)
rownames(sepouts)<-1:nrow(sepouts)


## ----objective2Table_sens,echo=FALSE,results='asis'----------------------
pandoc.table(sepouts,type="rmarkdown",split.tables=Inf)


