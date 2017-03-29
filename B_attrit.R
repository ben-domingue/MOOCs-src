library(readstata13)
setwd("/home/bd/Dropbox/moocs/data/lang_data/Stata/data")
list.files(pattern="*v3")->lf

courses<-
    c(ReservoirGeog="EarthSciences_ResGeo202_Spring2015_Problems_With_Item_types_pkey_v3.dta",
                                        #"Education_EDUC115-S_Spring2014_Problems_With_Item_types_pkey_v3.dta",
      QuantumMech="Engineering_QMSE-01_Fall2014_Problems_With_Item_types_pkey_v3.dta",
      StocksBonds="GSB_StocksBonds_SelfPaced_Problems_With_Item_types_pkey_v3.dta",
      StatLearning="HumanitiesandScience_StatLearning_Winter2015_Problems_With_Item_types_pkey_v3.dta",
      Econ1="HumanitiesSciences_Econ_1_Summer2015_Problems_With_Item_types_pkey_v3.dta",
      PatientEngagement="Medicine_ANES205_Fall2014_Problems_With_Item_types_pkey_v3.dta")
lf[lf %in% courses]->lf

attrit<-function(file.nm) {
    read.dta13(file.nm)->x
    x[order(x$item_order),]->x
    unique(x$problem_key)->item.nms
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,window) { 
        nrow(y)->n
        if (abs(n/N-window)<.05) y else NULL
    }
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_key",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    ##get correlation between mean correct and item location
    lapply(L,get.learners,N=N,window=1)->L2
    sapply(L2,is.null)->index
    L2[!index]->L2
    z<-list()
    for (var in c("first_attempt")) {
        lapply(L2,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    colMeans(z$first_attempt,na.rm=TRUE)->cm
    cor(cm,1:length(cm),use='p')->cc
    print(cc)
    ##
    mm<-list()
    for (thr in seq(0,1,by=.01)) {
        lapply(L,get.learners,N=N,window=thr)->L2
        sapply(L2,is.null)->index
        L2[!index]->L2
        z<-list()
        if (length(L2)>0) {
            for (var in c("first_attempt")) {
                lapply(L2,make.wide,nms=item.nms,col.nm=var)->out
                do.call("rbind",out)->z[[var]]
            }
            rowMeans(z$first_attempt,na.rm=TRUE)->rm
            quantile(rm,c(.25,.75),na.rm=TRUE)->mid
            c(mid[1],mean(rm,na.rm=TRUE),mid[2])->mm[[as.character(thr)]]
        } else rep(NA,3)->mm[[as.character(thr)]]
    }
    as.numeric(names(mm))->nms
    do.call("rbind",mm)->mm
    plot(nms,mm[,2],ylim=c(0,1),xlab="proportion attempted +/- 0.05",ylab="proportion correct",pch=19,type="b")
    lines(nms,mm[,1],col="red")
    lines(nms,mm[,3],col="red")
    mtext(side=3,line=0,names(courses)[which(file.nm==courses)],file.nm,cex=1)
    text(.7,.1,paste("r(item difficulty, item order) = ",round(cc,2),sep=""))
}
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,attrit)



attrit<-function(file.nm,threshold=0.5) {
    read.dta13(file.nm)->x
    x[order(x$item_order),]->x
    unique(x$problem_key)->item.nms
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
    for (var in c("first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    rowMeans(z$first_attempt,na.rm=TRUE)->rm
    m.na<-m.nna<-m.1<-m.0<-numeric()
    for (i in 1:ncol(z$first_attempt)) {
        is.na(z$first_attempt[,i])->index1
        mean(rm[index1])->m.na[i]
        mean(rm[!index1])->m.nna[i]
        mean(rm[!index1 & z$first_attempt[,i]==1])->m.1[i]
        mean(rm[!index1 & z$first_attempt[,i]==0])->m.0[i]
    }
    pf<-function(xv,col) {
        lines(xv,col=col)
        1:length(xv)->ii
                                        #lm(xv~ii)->mod
        loess(xv~ii)->mod
        predict(mod,se=TRUE)->tmp #interval="confidence")->tmp
        col2rgb(col)->cc
        polygon(c(ii,rev(ii)),c(tmp$fit-1.96*tmp$se.fit,rev(tmp$fit+1.96*tmp$se.fit)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55))
    }
    which(is.na(m.nna))->ii
    if (length(ii)>0) {
        m.nna[-ii]->m.nna
        m.na[-ii]->m.na
        m.1[-ii]->m.1
        m.0[-ii]->m.0
    }
    plot(NULL,ylim=c(-.15,0.4),xlim=c(1,N),xlab="item index",ylab="difference")
    pf(m.nna-m.na,col="black")
    pf(m.1-m.0,col="gray")
    mtext(side=3,line=0,names(courses)[which(file.nm==courses)],file.nm,cex=1)
    abline(h=0,lwd=3)
}
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,attrit)

