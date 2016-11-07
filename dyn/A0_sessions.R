load("desc1.Rdata")

est.dyn<-function(dat,nm) {
    dat[[nm]]->L
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa
    min(fa,na.rm=TRUE)->m
    fa-m->fa
    fa/(60*60*24)->fa
    apply(fa,2,median,na.rm=TRUE)->m
    plot(m,pch=19,xlab=nm,ylab="Days")
}

layout(matrix(c(1,1,2,3,4,5),3,2,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(3.3,3.3,1,1))
#est.dyn(dat,nm="Anes.v2")
#est.dyn(dat,nm="MedStat")
#est.dyn(dat,nm="MedStat.v2")
#est.dyn(dat,nm="Nano",blk=1:75)
est.dyn(dat,nm="C-10")
est.dyn(dat,nm="C-15")
est.dyn(dat,nm="C-17")
est.dyn(dat,nm="C-19")
est.dyn(dat,nm="C-13")
