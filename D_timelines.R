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


time<-function(file.nm,threshold=0.5) {
                                        #read.csv(file.nm)->x
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
    for (var in c("first_attempt_time","first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    time.conv(z$first_attempt_time)->t
    ifelse(nrow(t)<1000,nrow(t),1000)->NN
    sample(1:nrow(t),NN)->index
    t[index,]->t
    t->tmp
    for (i in 1:nrow(tmp)) {
        which.max(tmp[i,])->ii
        if (ii==1 & is.na(tmp[i,2])) NA->tmp[i,1]
        if (ii==N & is.na(tmp[i,N])) NA->tmp[i,N]
        if (ii>1 & ii<N) if (is.na(tmp[i,ii-1]) & is.na(tmp[i,ii+1])) NA->tmp[i,ii]
    }
    apply(tmp,2,max,na.rm=TRUE)->m
                                        #
    apply(t,1,diff)->del
    t(del)->del
    ifelse(del<0,del,0)->del
    rowMeans(abs(del),na.rm=TRUE)->sig
    min(sig)->lo
    max(sig)->hi
    .1+.9*((sig-lo)/(hi-lo))->del
                                        #
    t[order(del),]->t
    del[order(del)]->del
    plot(NULL,xlim=c(1,ncol(t)),ylim=c(0,max(m)),ylab="days since course start",xlab="item index")
    for (i in 1:nrow(t)) lines(t[i,],lwd=.3+2*del[i],col=gray(1-del[i]))
    mtext(side=3,line=0,names(courses)[which(file.nm==courses)],file.nm,cex=1)
                                        #for (i in 1:nrow(t)) lines(t[i,],lwd=.3)
    print(file.nm)
}
#pdf("/tmp/ts.pdf")
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,time,threshold=0.8)
#dev.off()


## time<-function(file.nm,threshold=0.5) {
##                                         #read.csv(file.nm)->x
##     read.dta13(file.nm)->x
##     x[order(x$item_order),]->x
##     unique(x$problem_key)->item.nms
##     length(item.nms)->N #total number of items
##     split(x,x$anon_screen_name)->L #each element of L is the info for a single person
##     length(L)->n.total #total number of people
##     ##this is going to get those who respond to at least threshold % of items
##     get.learners<-function(y,N,threshold) { 
##         nrow(y)->n
##         if (n/N>threshold) y else NULL
##     }
##     lapply(L,get.learners,N=N,threshold=threshold)->L
##     sapply(L,is.null)->index
##     L[!index]->L
##     ##now we'll start putting together item-level data
##     make.wide<-function(resp,nms,col.nm="first_attempt") {
##         resp[,c("problem_key",col.nm),]->resp
##         match(resp[,1],nms)->index
##         rep(NA,length(nms))->out
##         for (i in 1:length(index)) resp[i,2]->out[index[i] ]
##         out
##     }
##     z<-list()
##     for (var in c("first_attempt_time","first_attempt")) {
##         lapply(L,make.wide,nms=item.nms,col.nm=var)->out
##         do.call("rbind",out)->z[[var]]
##     }
##     #par(mfrow=c(2,1))
##     time.conv<-function(raw.time,M=min(raw.time,na.rm=TRUE)) {
##         as.POSIXct(raw.time/1000,origin="1960-01-01 00:00.00 UTC")->time.conv
##         as.POSIXct(M/1000,origin="1960-01-01 00:00.00 UTC")->M
##         as.numeric(time.conv-M) -> tmp
##         tmp/(24*60*60)->time.in.days #
##         time.in.days #since the first person responded to the first item
##     }
##     apply(z$first_attempt_time,2,min,na.rm=TRUE)->clk
##     time.conv(clk)->t.min
##     apply(z$first_attempt_time,2,quantile,.01,na.rm=TRUE)->clk
##     time.conv(clk)->t.1
##     apply(z$first_attempt_time,2,quantile,.1,na.rm=TRUE)->clk
##     time.conv(clk)->t.10
##     apply(z$first_attempt_time,2,quantile,.5,na.rm=TRUE)->clk
##     time.conv(clk)->t.50
##     pf<-function(t) {
##         diff(t)->del
##         c(NA,del)->del
##         plot(t,type="l")
##         which(del<0)->index
##         abline(v=index)
##         which(del< -1)->index
##         abline(v=index,col="red")
##     }
##     par(mfrow=c(4,1),mgp=c(2,1,0),mar=c(3,3,1,1))
##     pf(t.min)
##     mtext(cex=.6,paste(file.nm,nrow(z$first_attempt),"people"))
##     pf(t.1)    
##     mtext(cex=.6,nrow(z$first_attempt_time))
##     pf(t.10)    
##     pf(t.50)    
##     print(file.nm)
## }
## pdf("/tmp/time.pdf")
## lapply(lf,time)
## dev.off()
