load(file="/home/bd/Dropbox/moocs/data/proc/desc1.Rdata")

## To begin to understand this variation, we first describe variation in item characteristics (e.g. mean item response correctness) across MOOCs.

png("~/Downloads/moocs1.png",units="in",height=9,width=12,res=100)
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    dat[[course]]->L
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        colMeans(resp,na.rm=TRUE)
    }
    infun(L$first_grade)->fp
    infun(L$last_grade)->lp
    density(lp,na.rm=TRUE)->dl
    density(fp,na.rm=TRUE)->df
    range(c(dl$y,df$y))->ran
    plot(df,col="gray",ylim=ran,xlim=c(0,1),xlab=" ",ylab=" ",sub=" ",main="",lwd=3)
    mtext(side=3,line=.2,nm)
    lines(dl,lwd=3)
    legend("topleft",bty="n",c("first","last"),lty=1,lwd=2,col=c("gray","black"))
}
for (nm in names(dat)) fun(nm,dat)
dev.off()

png("~/Downloads/moocs1b.png",units="in",height=9,width=12,res=100)
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    dat[[course]]->L
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        colMeans(resp,na.rm=TRUE)
    }
    infun(L$first_grade)->fp
    plot(fp,ylim=c(0,1))
    mtext(side=3,line=.2,nm)
}
for (nm in names(dat)) fun(nm,dat)
dev.off()























