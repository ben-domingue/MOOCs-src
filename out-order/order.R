load("desc1.Rdata")
dat$ResGeo->L
dat$MedStat.v2->L

infun<-function(resp) {
    ifelse(as.matrix(resp)=="correct",1,0)->resp
    resp
}
infun(L$first_grade)->fg
L$first_attempt->fa.hold
apply(fa.hold,2,as.numeric)->fa

rowSums(is.na(fg))==0 -> index
fg[index,]->fg
fa[index,]->fa

#resgeo has columns that are duplicates
L<-list()
for (i in 1:(ncol(fg)-1)) {
    if (all(fg[,i]==fg[,i+1],na.rm=TRUE)) i->L[[i]]
}
unlist(L)->index
if (length(index)>0) {
    fg[,-index]->fg
    fa[,-index]->fa
}


order.fun<-function(x) {
    order(x,na.last=NA)->x
    x<-1:length(x)-x #want to get rid of negatie numbers
    x
}
apply(fa,1,order.fun)->ord
t(ord)->ord

ifelse(ord>0,NA,fg)->fg2
library(mirt)
mirt(fg2,1,itemtype="Rasch")->mod
fscores(mod)->fs

coef(mod)->co
do.call("rbind",co[-length(co)])->co
th<-matrix(fs[,1],nrow(fs),nrow(co),byrow=FALSE)
ease<-matrix(co[,2],nrow(fs),nrow(co),byrow=TRUE)
exp(th+ease)->kern
kern/(1+kern)->pv

tmp<-list()
for (i in 1:ncol(fg)) data.frame(item=i,id=1:nrow(fg),th=fs[,1],ease=rep(co[i,2],nrow(fg)),ord=ord[,i],pv=pv[,i],resp=fg[,i])->tmp[[i]]
data.frame(do.call("rbind",tmp))->df
df[df$ord>0,]->df

mean(df$pv)
mean(df$resp)


lm(resp~pv,df[df$ord>1,])

for (i in 1:10) {
    df[df$ord>1 & df$pv>(i-1)/10 & df$pv<i/10,]->tmp
    if (nrow(tmp)>0) {
        print(i)
        print(mean(tmp$resp))
    }
}

