
load("imps.Rdata")

png("/tmp/pv.png",units="in",height=4,width=5,res=100)
par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
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
    plot(df,col="black",ylim=ran,xlim=c(0,1),xlab=" ",ylab=" ",sub=" ",main="",lwd=3)
    mtext(side=3,line=.2,nm)
    lines(dl,lwd=3,col="gray")
    legend("topleft",bty="n",c("first","last"),lty=1,lwd=2,col=c("black","gray"))
}
for (nm in names(dat)) fun(nm,dat)
dev.off()
