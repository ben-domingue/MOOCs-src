load("desc1.Rdata")
#dat$`C-19`->L

outfun<-function(L) {
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        resp
    }
    infun(L$first_grade)->fg
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa
    min(fa,na.rm=TRUE)->m
    fa-m->fa
                                        #
    oo<-list()
    for (i in 1:(ncol(fa)-1)) {
        fa[,((i+1):ncol(fa))]-fa[,i] -> del
        if (class(del)=="numeric") matrix(del,ncol=1)->del
        find.min<-function(x) {
            x[!is.na(x)]->x
            if (length(x)>0) x[1]<0 else NA
        }
        apply(del,1,find.min)->mm
        mm->oo[[i]]
        #fa[,i]>fa[,i+1] -> oo.after
        #fa[,i]<fa[,i-1] -> oo.before
        #oo.after & oo.before->oo[[i]] #true here should be oo (out of order)
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
    return(c(mean(df$pv),mean(df$resp,na.rm=TRUE)))
}

zz<-list()
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
for (i in 1:length(dat)) {
    outfun(dat[[i]])->zz[[i]]
    mtext(side=3,line=.2,names(dat)[i])
}


do.call("rbind",zz)->zz
names(dat)->rownames(zz)
plot(zz[,1],zz[,2],type="n")
text(zz[,1],zz[,2],rownames(zz),cex=1)
abline(0,1)





