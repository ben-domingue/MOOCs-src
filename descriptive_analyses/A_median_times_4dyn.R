
session<-function(nm,dat) {
    dat[[nm]]->L
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa
    min(fa,na.rm=TRUE)->m
    fa-m->fa
    fa/(60*60*24)->fa
    apply(fa,2,median,na.rm=TRUE)->m
    plot(m,main=nm)
}
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
lapply(names(dat),session,dat=dat)
