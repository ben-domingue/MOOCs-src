library(readstata13)
setwd("/home/bd/Dropbox/moocs/data/lang_data/Stata/data")
list.files(pattern="*v3")->lf
#source("/home/bd/Dropbox/moocs/src/lang_meta/time_conv.R")

courses<-
    c(ReservoirGeog="EarthSciences_ResGeo202_Spring2015_Problems_With_Item_types_pkey_v3.dta",
                                        #"Education_EDUC115-S_Spring2014_Problems_With_Item_types_pkey_v3.dta",
      QuantumMech="Engineering_QMSE-01_Fall2014_Problems_With_Item_types_pkey_v3.dta",
      StocksBonds="GSB_StocksBonds_SelfPaced_Problems_With_Item_types_pkey_v3.dta",
      StatLearning="HumanitiesandScience_StatLearning_Winter2015_Problems_With_Item_types_pkey_v3.dta",
      Econ1="HumanitiesSciences_Econ_1_Summer2015_Problems_With_Item_types_pkey_v3.dta",
      PatientEngagement="Medicine_ANES205_Fall2014_Problems_With_Item_types_pkey_v3.dta")
lf[lf %in% courses]->lf


                                        #just simple densities
resp_time<-function(file.nm,threshold=0.5) {
                                        #read.csv(file.nm)->x
    read.dta13(file.nm)->x
    #sub("_Problems_With_Item_types_pkey_v3.dta","FirstObservationTime.dta",file.nm)->fn2
    #read.dta13(fn2)->x2
    x[order(x$item_order),]->x
    unique(x$problem_key)->item.nms
    as.double(x$first_attempt_time)->x$first_attempt_time
    as.double(x$observation_time)->x$observation_time
    (x$first_attempt_time - x$observation_time)/(1000*60) -> x$response_time #in minutes
    ifelse(x$response_time>60,NA,x$response_time)->x$response_time
    ifelse(x$response_time<0,NA,x$response_time)->x$response_time
    sum(is.na(x$response_time))/nrow(x)->per
    x[x$first_attempt==1,]->xy
    x[x$first_attempt==0,]->xn
    plot(density(xy$response_time,na.rm=TRUE),main="",xlab="Response time in minutes",ylab="")
    lines(density(xn$response_time,na.rm=TRUE),main="",col="red")
    paste(names(courses)[which(courses==file.nm)],"; NA proportion",round(per,2))->tit
    mtext(side=3,line=0,tit)
}
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,resp_time)
