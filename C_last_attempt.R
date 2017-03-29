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

last.fun<-function(file.nm) {
    read.dta13(file.nm)->x
    x[order(x$item_order),]->x
    unique(x$problem_key)->item.nms
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>=threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=0.5)->L
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
    ##
    z<-list()
    for (var in c("first_attempt","last_attempt","n_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    lapply(z,colMeans,na.rm=TRUE)->cm
    plot(cm$first_attempt,cm$n_attempt,pch=19,xlab="p value, first attempt",ylab="# attempts")
    mtext(side=3,line=0,names(courses)[which(file.nm==courses)],file.nm,cex=1)
}

par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
lapply(lf,last.fun)
