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


OOfun<-function(file.nm,
                look.ahead.seq=c(1,2,5,10), #the look.ahead window will be used to define OO. response i will have to occur after response i+look.ahead to be OO (slight modification of this rule to deal with NA values)
                threshold=.5,
                threshold.oo=0.3) #some items end of having way too many OO responses. if the item has more than threshold proportion of the responses OO, remove it.
{
    read.dta13(file.nm)->x
    x[order(x$item_order),]->x
    unique(x$problem_key)->item.nms
    as.double(x$first_attempt_time)->x$first_attempt_time
    as.double(x$observation_time)->x$observation_time
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
    for (var in c("first_attempt_time","first_attempt","observation_time")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    z$first_attempt -> fg
    z$first_attempt_time->fa
    z$observation_time->obs
                                        #time.conv(z$first_attempt_time)->fa
                                        #time.conv(z$observation_time)->obs
    apply(fa,2,max,na.rm=TRUE)->tmp.index
    fa[,is.finite(tmp.index)]->fa
    fg[,is.finite(tmp.index)]->fg
    obs[,is.finite(tmp.index)]->obs
    tr<-list()
########################################################
    for (look.ahead in look.ahead.seq) {
        ## defining OO items
        ##strategy:
        ##find difference in time between column i and all columns i+1:N
        ##for each row, look at the first non-NA value (we have lots of NAs, so i don't want to omit all items that come before NAs)
        ##test whether that value is greater than 0. if so, item i occurred before the next item to which they responded (by next, i mean item order). 
        oo<-list()
        for (i in 1:(ncol(fa)-look.ahead)) {
            fa[,((i+look.ahead):ncol(fa)),drop=FALSE]-fa[,i] -> del
            obs[,((i+look.ahead):ncol(obs)),drop=FALSE]-obs[,i] -> del.obs
            #if (class(del)=="numeric") matrix(del,ncol=1)->del
            find.min<-function(x) {
                x[!is.na(x)]->x
                if (length(x)>0) x[1]<0 else NA
            }
                                        #apply(del,1,find.min)->mm
            apply(del.obs,1,find.min)->mm
            mm->oo[[i]] #this contains the OO items for each column
        }
        do.call("cbind",oo)->ord #this will contain all OO items minus the last column
        cbind(ord,matrix(TRUE,nrow=nrow(ord),ncol=look.ahead))->ord #the last column can't be OO based on the def'n used here. 
        #
        colMeans(ord,na.rm=TRUE)->cm
        for (i in 1:ncol(ord)) if (cm[i]>threshold.oo) NA->ord[,i]
########################################################
##now remove those responses for OO items
        ifelse(ord,NA,fg)->fg2
########################################################
##A few courses had items that, after redaction, were all 1s or 0s. want to get rid of those. 
        lapply(as.data.frame(fg2),unique)->tab
        sapply(tab,function(x) 0 %in% x)->t0
        sapply(tab,function(x) 1 %in% x)->t1
        fg2[,t0 & t1]->fg2
########################################################
##Prep analysis, get all those who had at least one correct response
##(this is really meant to ensure that they aren't all NA, but i'm ok with the side effectd that we don't have people with all incorrect responses). 
        rowSums(!is.na(fg2))->rs
        rs>0 -> test
        fg2[test,]->fg2
        fg[test,]->fg
########################################################
##assemble a dataset containing:
##y-there true response (note that it comes from fg and not the redacted fg2)
##x-their mean for all other items
##oo-an indicator of whether the item was OO or not. 
        out<-list()
        for (ii in 10:ncol(fg2)) {
            fg2[,1:(ii-1)]->tmp
            rowMeans(tmp,na.rm=TRUE)->rs
            if (ii<ncol(fg2)) {
                fg2[,((ii+1):ncol(fg2)),drop=FALSE]->tmp.post
                rowMeans(tmp.post,na.rm=TRUE)->rs.post
            } else NA->rs.post
            which(ord[,ii])->oo.index
            data.frame(y=fg[,ii],x=rs,post=rs.post)->tmp
            ifelse(1:nrow(tmp) %in% oo.index,1,0)->tmp$oo
                                        #glm(y~x+oo,tmp,family="binomial")->m1 ##i was originally considering item-specific analyses but this is foolish. 
                                        #summary(m1)$coef[3,]->out[[as.character(ii)]]
            tmp->out[[as.character(ii)]]
        }        
        do.call("rbind",out)->tab
        data.frame(tab)->tmp
        tmp->tr[[as.character(look.ahead)]]
    }
    tr
}

out<-list()
for (file.nm in lf) {
    OOfun(file.nm)->out[[file.nm]]
}
do.call("c",out)->out2



bigfun<-function(xx) {
    fun<-function(tmp) {
########################################################
        ##our hypothesis: a y=1 response should be more likely if oo=1 (conditional on x)
        ##glm(y~x+oo,tmp,family="binomial")->m1 #takes too long
        lm(y~x+oo,tmp)->m1 #hoping for a positive coef on oo
        summary(m1)$coef
    }
    lapply(xx,fun)->tmp
    lapply(tmp,function(x) x[3,1])->zz
    unlist(zz)
}
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
y1<-list()
for (i in 1:length(out)) {
    bigfun(out[[i]])->y1[[i]]
    plot(y1[[i]],type="l",ylim=c(-.3,0.1),xlab=names(out)[i]); abline(h=0)
}

###
                                        #cor of oo with remaining items. perhaps should limit these to just N after the oo item (eg N=10)
fun<-function(x) {
    cor(x$post,x$oo,use='p')
}
y2<-list()
for (i in 1:length(out))  {
    sapply(out[[i]],fun)->y2[[i]]
}
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(1.5,4))
for (i in 1:length(foo)) {
    plot(y2[[i]],type="l",xlab=names(out)[i],ylim=c(-.1,.1)); abline(h=0)
}

plot(unlist(y1),unlist(y2)) 


