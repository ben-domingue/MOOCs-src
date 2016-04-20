last_grade[,-1]->gr
as.matrix(gr)->gr
ifelse(gr=="none",NA,gr)->gr
apply(gr,2,table)
rowSums(!is.na(gr))->rs
rs/ncol(gr)->rs
rs>=0.5 -> pi

#Variability in time invested in course. 
as.numeric(first_view[pi,ncol(first_view)])-as.numeric(first_view[pi,2])->del
del/(60^2)->time.invested
summary(time.invested)
summary(time.invested/24)
plot(density(time.invested/24,na.rm=TRUE))
hist(time.invested/24,breaks=25,xlab="days")


#Variability in when people take items. 
first_attempt[pi,-1]->fa
apply(fa,2,as.numeric)->fa
min(unlist(fa),na.rm=TRUE)->m
fa-m -> fa
max(unlist(fa),na.rm=TRUE)-> M
plot(NULL,xlim=c(0,M),ylim=c(1,nrow(fa)),xaxt="n",xlab="days",ylab="person number")
axis(side=1,at=seq(0,M,length.out=5),round(seq(0,M,length.out=5)/(60*60*24),1))
for (i in 1:nrow(fa)) {
    abline(h=i,lty=1,lwd=.3)
    points(fa[i,],rep(i,ncol(fa)),cex=.4,pch=19)
}

first_attempt[pi,-1]->fa
apply(fa,2,as.numeric)->fa
apply(fa,1,min,na.rm=TRUE)->m
apply(fa,1,max,na.rm=TRUE)->M
plot(NULL,xlim=c(0,1),ylim=c(1,nrow(fa)),xlab="proportion of elapsed course",ylab="person number")
for (i in 1:nrow(fa)) {
    abline(h=i,lty=1,lwd=.3)
    points((fa[i,]-m[i])/(M[i]-m[i]),rep(i,ncol(fa)),cex=.4,pch=19)
}



#How much time people spend on items
time_to_first_attempt[pi,-1]->tfa
apply(tfa,2,as.numeric)->tfa
ifelse(tfa>3600,3600,tfa)->tfa
apply(tfa,2,median,na.rm=TRUE)->tab
hist(tab/60,breaks=35,xlab="time in minutes till first attempt")

time_to_first_attempt[pi,-1]->tfa
apply(tfa,2,as.numeric)->tfa
time_to_last_attempt[pi,-1]->tla
apply(tla,2,as.numeric)->tla
#ifelse(tfa>3600,3600,tfa)->tfa
#ifelse(tla>3600,3600,tla)->tla
tla-tfa -> time.attempts
ifelse(time.attempts>(60*60*3),60*60*3,time.attempts)->time.attempts

layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
hist(apply(tla,2,median,na.rm=TRUE)/60,breaks=35,xlab="time in minutes until last attempt",sub="capped at 3 hours",main="")
ifelse(tfa>(60*60*2),60*60*2,tfa)->tfa
ifelse(tla>(60*60*2),60*60*2,tla)->tla
plot(apply(tfa,2,median,na.rm=TRUE)/60,apply(tla,2,median,na.rm=TRUE)/60,xlab="time till first attempt, minutes",ylab="time till first attempt, minutes",sub="all capped at 2 hours")
n_attempts[pi,-1]->na
apply(na,2,as.numeric)->na
plot(apply(tla,2,median,na.rm=TRUE)/60,colMeans(na,na.rm=TRUE),xlab="time till last attempt, minutes",ylab="mean # of attempts",sub="all capped at 2 hours")

