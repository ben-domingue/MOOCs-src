load(file="~/moocs/resp.Rdata")
sapply(dat,length)->N
dat[N>0]->dat
dat[[20]]->L

##########################################################
for (i in 1:length(L)) assign(names(L)[i],L[[i]])
last_grade[,-1]->gr
as.matrix(gr)->gr
ncol(gr)->N
ifelse(gr=="none",NA,gr)->gr
rowSums(!is.na(gr))->rs
rs/ncol(gr)->rs
rs>=0.5 -> pi #these are people that complete at least half the items
as.numeric(first_view[pi,ncol(first_view)])->max.time
if (sum(!is.na(max.time))>0) {
    max.time-as.numeric(first_view[pi,2])->del
    del/(60^2)->time.invested
    summary(time.invested)
    summary(time.invested/24)
                                        #plot(density(time.invested/24,na.rm=TRUE))
    if (sum(!is.na(time.invested))==0) plot(NULL,xlim=c(1,2),ylim=c(1,2),)
    else hist(time.invested/24,breaks=25,xlab="days",main="",sub=paste(length(rs[pi]),"people"))
    mtext(side=3,line=0,nm)
    abline(v=0,col="red")
}

fun<-function(x) {
    x[pi,-1]->fv
    unlist(fv)->tmp
    as.numeric(tmp)->tmp
    min(tmp,na.rm=TRUE)->m
    max(tmp,na.rm=TRUE)->M
    for (i in 1:ncol(fv)) (as.numeric(fv[,i])-m)/(M-m)->fv[,i]
                                        #
    sample(1:nrow(fv),25)->index
    par(mfrow=c(5,5),mar=rep(0,4),oma=rep(1,4))
    for (i in index) {
        plot(1:ncol(fv),fv[i,],type="l",xlab="",xaxt="n",yaxt="n",ylab="")
        1:ncol(fv)->xv
        as.numeric(fv[i,])->yv
        abline(lm(yv~xv),col="red")
    }
}

pdf("/tmp/time.pdf",width=12,height=12)
fun(first_view)
fun(first_attempt)
fun(last_attempt)
dev.off()




## time_to_first_attempt[pi,-1]->tfa
## for (i in 1:ncol(tfa)) {
##     as.numeric(tfa[,i])->z
##     z->tfa[,i]
## }
## #as.matrix(tfa)->tfa
## stack(tfa)->tmp
## #boxplot(tmp[,1]~tmp[,2])

## if (sum(!is.na(tfa))>0) {
##     ifelse(tfa>3600,3600,tfa)->tfa
##     apply(tfa,2,median,na.rm=TRUE)->tab
##     hist(tab/60,breaks=35,xlab="",sub=paste(length(rs[pi]),"people"))
##     abline(v=0,col="red")
##                                         #
##     time_to_first_attempt[pi,-1]->tfa
##     apply(tfa,2,as.numeric)->tfa
##     time_to_last_attempt[pi,-1]->tla
##     apply(tla,2,as.numeric)->tla
##                                         #ifelse(tfa>3600,3600,tfa)->tfa
##                                         #ifelse(tla>3600,3600,tla)->tla
##     tla-tfa -> time.attempts
##     ifelse(time.attempts>(60*60*3),60*60*3,time.attempts)->time.attempts
##                                         #
## }

