load("imps.Rdata")

#trajectory
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    options(set.seed=11012021)
    dat[[course]]->L
    L$first_attempt->fa.hold
    sample(1:nrow(fa.hold),100)->random.index
    fa.hold[random.index,]->fa.hold
    apply(fa.hold,2,as.numeric)->fa.hold
    min(unlist(fa.hold),na.rm=TRUE)->m
    as.POSIXct(m,origin="1970-01-01 00:00.00 UTC")->m.conv
    fa.hold-m -> fa
    fa/(30*24*60*60)->fa
    max(unlist(fa),na.rm=TRUE)-> M
    plot(NULL,xlim=c(1,ncol(fa.hold)),ylim=c(0,M),ylab="months after start",xlab="")
    mtext(side=3,line=.2,nm)
    mtext(side=1,line=2,paste("start time",m.conv),cex=.6)
    for (i in 1:nrow(fa)) lines(1:ncol(fa),fa[i,])
}
for (nm in names(dat)) fun(nm,dat)
