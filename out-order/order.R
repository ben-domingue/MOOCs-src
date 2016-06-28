load("desc1.Rdata")

outfun<-function(L) {
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        resp
    }
    infun(L$first_grade)->fg
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa
    #
    rowSums(is.na(fg))==0 -> index
    if (sum(index)>250) {
        fg[index,]->fg
        fa[index,]->fa
                                        #
        order.fun<-function(x) {
            order(x,na.last=NA)->x
            x<-1:length(x)-x #want to get rid of negatie numbers
            x
        }
        apply(fa,1,order.fun)->ord
        t(ord)->ord
                                        #
        ifelse(ord>0,NA,fg)->fg2
        #
        apply(fg2,2,unique)->tab
        sapply(tab,function(x) 0 %in% x)->t0
        sapply(tab,function(x) 1 %in% x)->t1
        fg2[,t0 & t1]->fg2
        library(mirt)
        mirt(fg2,1,itemtype="Rasch")->mod
        fscores(mod)->fs
                                        #
        coef(mod)->co
        do.call("rbind",co[-length(co)])->co
        th<-matrix(fs[,1],nrow(fs),nrow(co),byrow=FALSE)
        ease<-matrix(co[,2],nrow(fs),nrow(co),byrow=TRUE)
        exp(th+ease)->kern
        kern/(1+kern)->pv
                                        #
        tmp<-list()
        for (i in 1:ncol(fg2)) data.frame(item=i,id=1:nrow(fg),th=fs[,1],ease=rep(co[i,2],nrow(fg)),ord=ord[,i],pv=pv[,i],resp=fg[,i])->tmp[[i]]
        data.frame(do.call("rbind",tmp))->df
        df[df$ord>0,]->df
                                        #
        c(mean(df$pv),mean(df$resp))
    } else c(NA,NA)
}
zz<-list()
for (i in 1:length(dat)) outfun(dat[[i]])->zz[[i]]





lm(resp~pv,df[df$ord>1,])

for (i in 1:10) {
    df[df$ord>1 & df$pv>(i-1)/10 & df$pv<i/10,]->tmp
    if (nrow(tmp)>0) {
        print(i)
        print(mean(tmp$resp))
    }
}

