## Finally, we consider psychometric models (e.g., does a consideration of responsive time offer new information about learner ability or engagement compared to item response alone) of MOOC item responses in light of above findings. 
load("desc1.Rdata")

png("/tmp/sessions.png",units="in",height=9,width=12,res=100)
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    dat[[course]]->L
    #
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa.hold
    min(unlist(fa.hold),na.rm=TRUE)->m
    as.POSIXct(m,origin="1970-01-01 00:00.00 UTC")->m.conv
    fa.hold-m -> fa
    fa/(30*24*60*60)->fa
    #plot(colMeans(fa,na.rm=TRUE),type="b",pch=19)
    apply(fa,2,median,na.rm=TRUE)->M
    plot(M,type="b",pch=19)
    mtext(side=3,line=.2,nm)
}
for (nm in names(dat)) fun(nm,dat)
dev.off()

library(lme4)
fun<-function(course,dat) {
    print(course)
    dat[[course]]->L
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        resp
    }
    infun(L$first_grade)->fg
    L$first_attempt->fa.hold
    ##sampling
    rowSums(is.na(fg))==0 -> index
    which(index)->index    
    if (length(index)==0) 1:nrow(fg)->index
    if (length(index)>5000) sample(index,5000)->index
    fg[index,]->fg
    fa.hold[index,]->fa.hold
    ##
    apply(fa.hold,2,as.numeric)->fa.hold
    ff<-function(x) {
        min(x,na.rm=TRUE)->m
        as.POSIXct(m,origin="1970-01-01 00:00.00 UTC")->m.conv
        x-m->fa
        fa/(30*24*60*60)->fa
        max(fa,na.rm=TRUE)->M
        fa/M
    }
    apply(fa.hold,1,ff)->fa
    t(fa)->fa
    #
    all(dim(fg)==dim(fa))
    1:nrow(fa)->id
    tmp<-list()
    for (i in 1:ncol(fa)) cbind(id,fg[,i],fa[,i],rep(i,length(id)))->tmp[[i]]
    do.call("rbind",tmp)->z
    1:nrow(z)->rownames(z)
    data.frame(z)->z
    names(z)<-c("id","resp","time","item")
    ##    
    #glmer(resp~(1|item)+(1|id),z,family="binomial")->m0
    #glmer(resp~time+(1|item)+(1|id),z,family="binomial")->m1
    glmer(resp~time+(1|item)+(1+time|id),z,family="binomial")->m2
    summary(m2)$coef
}
tab<-list()
for (nm in names(dat)) fun(nm,dat)->tab[[nm]]
save(tab,file="/tmp/linear.Rdata")

sapply(tab,function(x) x[2,])->z
library(gplots)
par(mgp=c(2,1,0),mar=c(4,10,2,2))
barplot2(z[1,],horiz=TRUE,names.arg=colnames(z),plot.ci=TRUE,ci.l=z[1,]-1.96*z[4,],ci.u=z[1,]+1.96*z[4,],las=2)


