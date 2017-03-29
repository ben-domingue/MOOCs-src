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


##                                         #just simple densities
## resp_time<-function(file.nm,threshold=0.5) {
##                                         #read.csv(file.nm)->x
##     read.dta13(file.nm)->x
##     #sub("_Problems_With_Item_types_pkey_v3.dta","FirstObservationTime.dta",file.nm)->fn2
##     #read.dta13(fn2)->x2
##     x[order(x$item_order),]->x
##     unique(x$problem_key)->item.nms
##     as.double(x$first_attempt_time)->x$first_attempt_time
##     as.double(x$observation_time)->x$observation_time
##     (x$first_attempt_time - x$observation_time)/(1000*60) -> x$response_time #in minutes
##     ifelse(x$response_time>60,NA,x$response_time)->x$response_time
##     ifelse(x$response_time<0,NA,x$response_time)->x$response_time
##     sum(is.na(x$response_time))/nrow(x)->per
##     plot(density(x$response_time,na.rm=TRUE),main="",xlab="Response time in minutes",ylab="")
##     paste(names(courses)[which(courses==file.nm)],"; NA proportion",round(per,2))->tit
##     mtext(side=3,line=0,tit)
## }
## par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
## lapply(lf,resp_time)

                                        #looking at resopnse times by items
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
    ##
    mean(x$response_time,na.rm=TRUE)->mm1
    sum(is.na(x$response_time))/nrow(x)->per
    density(x$response_time,na.rm=TRUE)->den
    which.min(abs(den$x-mm1))->iii
    plot(den,main="",xlab="Response time in minutes",ylab="")
    segments(den$x[iii],0,den$x[iii],den$y[iii])
    paste(names(courses)[which(courses==file.nm)],"; NA proportion",round(per,2))->tit
    ##
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=threshold)->L
    sapply(L,is.null)->index
    L[!index]->L
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_key",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    z<-list()
    for (var in c("response_time","first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    colMeans(z$response_time,na.rm=TRUE)->cm
    summary(cm)->mm2
    plot(cm,type="l",ylim=c(0,35),xlab="item index",ylab="mean response time")
    abline(h=mean(cm,na.rm=TRUE))
    paste(names(courses)[which(courses==file.nm)])->tit
    mtext(side=3,line=0,tit)
                                        #list(mm1,mm2)
    data.frame(problem_key=item.nms,mean_response_time=cm)
}
par(mfrow=c(6,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,resp_time)->out
lf->names(out)
save(out,file="/tmp/mean_time.Rdata")


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
    ifelse(x$response_time<0 ,NA,x$response_time)->x$response_time
    ifelse(x$response_time>60 ,NA,x$response_time)->x$response_time
    #summary(x$response_time)->tr2
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=threshold)->L
    sapply(L,is.null)->index
    L[!index]->L
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_key",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    z<-list()
    for (var in c("response_time","first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    mean(colMeans(is.na(z$response_time)))->M
    apply(z$response_time,2,mean,na.rm=TRUE)->m1
    apply(z$first_attempt,2,mean,na.rm=TRUE)->m2
    plot(NULL,pch=19,xlab="mean response time",ylab="p-value",xlim=c(0,30),ylim=c(0,1))
    which(courses==file.nm)->index
    mtext(side=3,line=0,paste(names(courses)[index]," (no time=",round(M,2)*100,"%)",sep=""),cex=1)
    pf<-function(x,y,col="black") {
        cbind(x,y)->tmp
        tmp[rowSums(is.na(tmp))==0,]->tmp
        tmp[order(tmp[,1]),]->tmp
        points(tmp[,1],tmp[,2],col="black",pch=19,cex=.5)
                                        #lm(xv~ii)->mod
        loess(tmp[,2]~tmp[,1])->mod
        predict(mod,se=TRUE)->tmp2 #interval="confidence")->tmp
        col2rgb(col)->cc
        polygon(c(tmp[,1],rev(tmp[,1])),c(tmp2$fit-1.96*tmp2$se.fit,rev(tmp2$fit+1.96*tmp2$se.fit)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55))
    }
    pf(m1,m2)
}
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,resp_time)

    
    
