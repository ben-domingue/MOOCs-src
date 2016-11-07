load("imps.Rdata")

#
png("/tmp/time.png",units="in",height=4,width=5,res=100)
par(mgp=c(2,1,0))
plot(NULL,xlim=c(0,90),ylim=c(0,.08),xlab="days",ylab="",main="")
for (course in names(dat)) {
    dat[[course]]->x
    x$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa.hold
    min(unlist(fa.hold),na.rm=TRUE)->m
    apply(fa.hold,1,min,na.rm=TRUE)->m1
    apply(fa.hold,1,max,na.rm=TRUE)->M1
##########################################################
##Variability in when people take items. 
    (M1-m1)/(24*60*60)->days
    density(days)->den
    lines(den,xlab="days",lwd=3,col="gray",ylab="",main="")
}
dev.off()
