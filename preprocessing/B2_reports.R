load(file="~/moocs/resp.Rdata")
sapply(dat,length)->N
dat[N>0]->dat
grep("NetworkingSP",names(dat))->i
dat[-i]->dat

##########################################################
fun<-function(L,nm) {
    for (i in 1:length(L)) assign(names(L)[i],L[[i]])
    last_grade[,-1]->gr
    as.matrix(gr)->gr
    ncol(gr)->N
    if (N<5) {
        print("nsf items")
        plot(NULL,xlim=c(1,2),ylim=c(1,2),ylab=nm)
    } else {
        ifelse(gr=="none",NA,gr)->gr
        rowSums(!is.na(gr))->rs
        rs/ncol(gr)->rs
        rs>=0.5 -> pi #these are people that complete at least half the items
                                        #looking at how many people completed how many items
                                        #apply(gr,2,table)
        hist(rs,sub=paste(length(rs),"people"),ylab=nm,main="")
        mtext(side=3,line=0,nm)
        abline(v=0)
        abline(v=N)
        NULL
    }
}
pdf(paste("/tmp/mooc-A.pdf",sep=""),width=15,height=12)
par(mgp=c(1.7,1,0),mar=c(3.5,3.3,3,.5),oma=c(1,1,1,1),mfrow=c(7,4))
for (i in 1:length(dat)) {
    names(dat)[i]->nm
    print(nm)
    fun(dat[[i]],nm=nm)
}
dev.off()



##########################################################
fun<-function(L,nm) {
    for (i in 1:length(L)) assign(names(L)[i],L[[i]])
    last_grade[,-1]->gr
    as.matrix(gr)->gr
    ncol(gr)->N
    if (N<5) {
        print("nsf items")
        plot(NULL,xlim=c(1,2),ylim=c(1,2),ylab=nm)
    } else {
        ifelse(gr=="none",NA,gr)->gr
        rowSums(!is.na(gr))->rs
        rs/ncol(gr)->rs
        rs>=0.5 -> pi #these are people that complete at least half the items
        hist(breaks=20,rs[pi],xlim=c(0.5,1),main="",sub=paste(length(rs[pi]),"people"))
        mtext(side=3,line=0,nm)
        NULL
    }
}
pdf(paste("/tmp/mooc-B.pdf",sep=""),width=15,height=12)
par(mgp=c(1.7,1,0),mar=c(3.5,3.3,3,.5),oma=c(1,1,1,1),mfrow=c(7,4))
for (i in 1:length(dat)) {
    names(dat)[i]->nm
    print(nm)
    fun(dat[[i]],nm=nm)
}
dev.off()


##########################################################
fun<-function(L,nm) {
    for (i in 1:length(L)) assign(names(L)[i],L[[i]])
    last_grade[,-1]->gr
    as.matrix(gr)->gr
    ncol(gr)->N
    if (N<5) {
        print("nsf items")
        plot(NULL,xlim=c(1,2),ylim=c(1,2),ylab=nm)
    } else {
        ifelse(gr=="none",NA,gr)->gr
        rowSums(!is.na(gr))->rs
        rs/ncol(gr)->rs
        rs>=0.5 -> pi #these are people that complete at least half the items
        colMeans(!is.na(gr[pi,,drop=FALSE]))->cs
        plot(cs,type="l",main="",sub=paste(length(rs[pi]),"people"))
        mtext(side=3,line=0,nm)
        NULL
    }
}
pdf(paste("/tmp/mooc-C.pdf",sep=""),width=15,height=12)
par(mgp=c(1.7,1,0),mar=c(3.5,3.3,3,.5),oma=c(1,1,1,1),mfrow=c(7,4))
for (i in 1:length(dat)) {
    names(dat)[i]->nm
    print(nm)
    fun(dat[[i]],nm=nm)
}
dev.off()


##########################################################
fun<-function(L,nm) {
    for (i in 1:length(L)) assign(names(L)[i],L[[i]])
    last_grade[,-1]->gr
    as.matrix(gr)->gr
    ncol(gr)->N
    if (N<5) {
        print("nsf items")
        plot(NULL,xlim=c(1,2),ylim=c(1,2),ylab=nm)
    } else {
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
        NULL
    }
}
pdf(paste("/tmp/mooc-G.pdf",sep=""),width=15,height=12)
par(mgp=c(1.7,1,0),mar=c(3.5,3.3,3,.5),oma=c(1,1,1,1),mfrow=c(7,4))
for (i in 1:length(dat)) {
    names(dat)[i]->nm
    print(nm)
    fun(dat[[i]],nm=nm)
}
dev.off()


##########################################################
fun<-function(L,nm) {
    for (i in 1:length(L)) assign(names(L)[i],L[[i]])
    last_grade[,-1]->gr
    as.matrix(gr)->gr
    ncol(gr)->N
    if (N<5) {
        print("nsf items")
        plot(NULL,xlim=c(1,2),ylim=c(1,2),ylab=nm)
    } else {
        ifelse(gr=="none",NA,gr)->gr
        rowSums(!is.na(gr))->rs
        rs/ncol(gr)->rs
        rs>=0.5 -> pi #these are people that complete at least half the items
        time_to_first_attempt[pi,-1]->tfa
        for (i in 1:ncol(tfa)) {
            as.numeric(tfa[,i])->z
            z->tfa[,i]
            ## if (all(is.na(z))) {
            ##     dev.off()
            ##     return("no time")
            ## }
        }
        as.matrix(tfa)->tfa
        if (sum(!is.na(tfa))>0) {
            ifelse(tfa>3600,3600,tfa)->tfa
            apply(tfa,2,median,na.rm=TRUE)->tab
            hist(tab/60,breaks=35,xlab="",sub=paste(length(rs[pi]),"people"))
            abline(v=0,col="red")
            mtext(side=3,line=0,nm)
                                        #
            time_to_first_attempt[pi,-1]->tfa
            apply(tfa,2,as.numeric)->tfa
            time_to_last_attempt[pi,-1]->tla
            apply(tla,2,as.numeric)->tla
                                        #ifelse(tfa>3600,3600,tfa)->tfa
                                        #ifelse(tla>3600,3600,tla)->tla
            tla-tfa -> time.attempts
            ifelse(time.attempts>(60*60*3),60*60*3,time.attempts)->time.attempts
                                        #
        }
    }
}
pdf(paste("/tmp/mooc-J.pdf",sep=""),width=15,height=12)
par(mgp=c(1.7,1,0),mar=c(3.5,3.3,3,.5),oma=c(1,1,1,1),mfrow=c(7,4))
for (i in 1:length(dat)) {
    names(dat)[i]->nm
    print(nm)
    fun(dat[[i]],nm=nm)
}
dev.off()















