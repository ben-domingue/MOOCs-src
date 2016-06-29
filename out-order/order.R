load("desc1.Rdata")
dat$`C-19`->L

outfun<-function(L) {
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        resp
    }
    infun(L$first_grade)->fg
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa
                                        #
    #rowSums(is.na(fg))==0 -> index
    #if (sum(index)>250) {
                                        #fg[index,]->fg
                                        #fa[index,]->fa
                                        #
                                        ## order.fun<-function(x) {
                                        ##     order(x,na.last=NA)->x
                                        ##     x<-1:length(x)-x #want to get rid of negatie numbers
                                        ##     x
                                        ## }
                                        ## apply(fa,1,order.fun)->ord
                                        ## t(ord)->ord
    oo<-list()
    for (i in 2:(ncol(fa)-1)) {
        fa[,i]>fa[,i+1] -> oo.after
        fa[,i]<fa[,i-1] -> oo.before
        oo.after & oo.before->oo[[i]] #true here should be oo (out of order)
    }
    do.call("cbind",oo)->ord
    cbind(TRUE,ord,TRUE)->ord

    

                                        #
    ifelse(ord,NA,fg)->fg2
                                        #
    lapply(as.data.frame(fg2),unique)->tab
    sapply(tab,function(x) 0 %in% x)->t0
    sapply(tab,function(x) 1 %in% x)->t1
    fg2[,t0 & t1]->fg2
    library(mirt)
    mirt(fg2,1,itemtype="Rasch",method="EM")->mod
    fscores(mod)->fs
                                        #
    coef(mod)->co
    do.call("rbind",co[-length(co)])->co
    th<-matrix(fs[,1],nrow(fs),nrow(co),byrow=FALSE)
    ease<-matrix(co[,2],nrow(fs),nrow(co),byrow=TRUE)
    disc<-matrix(co[,1],nrow(fs),nrow(co),byrow=TRUE)
    exp(disc*(th+ease))->kern
    kern/(1+kern)->pv
                                        #
    tmp<-list()
    for (i in 1:ncol(fg2)) data.frame(item=i,id=1:nrow(fg),th=fs[,1],ease=rep(co[i,2],nrow(fg)),ord=ord[,i],pv=pv[,i],resp=fg[,i])->tmp[[i]]
    data.frame(do.call("rbind",tmp))->df
    df[df$ord & !is.na(df$ord),]->df
                                        #
    c(mean(df$pv),mean(df$resp,na.rm=TRUE))
    #
    by(df[,c("pv","resp")],df$item,colMeans,na.rm=TRUE)->z
    do.call("rbind",z)->z
    plot(z[,2]-z[,1],pch=19); abline(h=0,lty=2)
    #
    1:nrow(z)->xv
    loess(z[,2]-z[,1]~xv)->m
    cbind(m$x,m$fit)->tmp
    tmp[order(tmp[,1]),]->tmp
    lines(tmp)
    
       
    
}
zz<-list()
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
for (i in 1:length(dat)) outfun(dat[[i]])->zz[[i]]

do.call("rbind",zz)->zz
plot(zz[,1],zz[,2],type="n")
text(zz[,1],zz[,2],rownames(zz))
abline(0,1)





lm(resp~pv,df[df$ord>1,])

for (i in 1:10) {
    df[df$ord>1 & df$pv>(i-1)/10 & df$pv<i/10,]->tmp
    if (nrow(tmp)>0) {
        print(i)
        print(mean(tmp$resp))
    }
}

