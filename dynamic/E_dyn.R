load("imps.Rdata")

est.dyn<-function(L,blk=NULL) {
    if (!is.null(blk)) {
        for (i in 1:length(L)) {
            L[[i]][,blk]->L[[i]]
        }
    }
    session<-function(L) {
        L$first_attempt->fa.hold
        apply(fa.hold,2,as.numeric)->fa
        min(fa,na.rm=TRUE)->m
        fa-m->fa
        fa/(60*60*24)->fa
        apply(fa,2,median,na.rm=TRUE)->m
        library(Ckmeans.1d.dp)
        Ckmeans.1d.dp(m,c(1,30))->cl
        cl$cluster-1
    }
##
                                        #prep data
    library(emIRT)
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,-1)->resp
        ifelse(is.na(resp),0,resp)
    }
    infun(L$first_grade)->fg
    rownames(fg)<-paste("p",1:nrow(fg),sep="")
    colnames(fg)<-1:ncol(fg)
    session(L)->cl
    nrow(fg)->N
    ncol(fg)->J
    max(cl)+1->T
                                        #
                                        #assign items to sessions
    dat<-list()
    dat$rc<-fg
    blk<-list()
    for (i in unique(cl)) {
        fg[,cl==i,drop=FALSE]->tmp
        rowSums(tmp==0)->rs
        ifelse(rs==ncol(tmp),0,1)->blk[[i+1]]
    }
    do.call("cbind",blk)->blk
                                        #table(paste(blk,collapse=""))
    fun<-function(x) {
        which(x==1)[1]-1 -> st
        rev(x)->x
        which(x==1)[1] -> end
        length(x)-end->end
        c(st,end)
    }
    apply(blk,1,fun)->st.end
    t(st.end)->st.end
    dat$startlegis<-matrix(st.end[,1],ncol=1,nrow=N)
    dat$endlegis<-matrix(st.end[,2],ncol=1,nrow=N)
    dat$bill.session<-matrix(cl,ncol=1,nrow=J)
    dat$T<-T
    ##
                                        #starting values
    st<-list()
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
    }
    apply(L$first_grade,2,infun)->resp
    colMeans(resp,na.rm=TRUE)->cm
    -1*(cm-mean(cm))/sd(cm)->cm
    matrix(cm,ncol=1)->st$alpha
                                        #matrix(rep(0,J),ncol=1)->st$alpha
    rowMeans(resp,na.rm=TRUE)->rm
    C<-numeric()
    for (i in 1:ncol(resp)) cor(rm,resp[,i],use='p')->C[i]
    cut(C,c(0,quantile(C,c(.25,.5,.75)),1),labels=1:4)->index
    c(.7,.9,1.1,1.3)[index]->beta
    matrix(beta,ncol=1)->st$beta
                                        #matrix(rep(0,J),ncol=1)->st$beta
    x<-matrix(0,nrow=N,ncol=T)
    rowMeans(resp,na.rm=TRUE)->rm
    (rm-mean(rm))/sd(rm)->rm
    for (i in 1:nrow(x)) {
        dat$startlegis[i]:dat$endlegis[i]+1->index
        rm[i]->x[i,index]
    }
    x->st$x
    ##
                                        #priors
    pr<-list()
    pr$x.mu0<-matrix(0,nrow=N,ncol=1)
    pr$x.sigma0<-matrix(1,nrow=N,ncol=1)
    pr$beta.mu<-matrix(c(0,0),2,1)
    pr$beta.sigma<-matrix(c(1,0,0,1),2,2)
    pr$omega2<-matrix(0.1,nrow=N,ncol=1)
    ##
    nrow(dat$rc)
    ncol(dat$rc)
    dat$T
    str(dat)
    str(st)
    str(pr)
    ##
    dynIRT(dat,.starts=st,.priors=pr,
           .control = list(
               threads = 1,
               verbose = TRUE,
               thresh = 1e-6,
               maxit=500
           ))->mod
    list(mod=mod,st=st)
}


lapply(dat[c("C-6","C-15","C-17","C-19")],est.dyn)->out

fun<-function(nm,out) {
    out[[nm]]->mod
                                        #
    mod$st->st
    mod$mod->mod
    mod$means$x->aa
    sum(abs(st$x)<.000000001)==sum(abs(aa)<.000000001)
    ifelse(abs(aa)<.000001,NA,aa)->aa
    aa->qumat
    for (i in 1:ncol(aa)) {
        ecdf(aa[,i])->f
        f(aa[,i])->qu
        qu->qumat[,i]
    }
    del<-numeric()
    for (i in 2:ncol(qumat)) mean(abs(qumat[,i]-qumat[,i-1]),na.rm=TRUE)->del[i]
    plot(del,main=nm,ylim=c(0,0.15),pch=19,type="b",lwd=2,cex=2,xlab="delta session",ylab="mean(abs(delta(ability)))")
                                        #sample(1:nrow(aa),80)->index
                                        #matplot(t(aa[index,]),type="l",lty=1,col="black",ylim=c(-3.5,3.5),ylab="ability",xlab="session")
    apply(aa,2,var,na.rm=TRUE)->v
    plot(v,main=nm,ylim=c(0,2.5),pch=19,type="b",lwd=2,cex=2,xlab="session",ylab="var ability")
}
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
lapply(c("C-17","C-19"),fun,out=out)

fun<-function(nm,out) {
    out[[nm]]->mod
    #
    mod$st->st
    mod$mod->mod
    mod$means$x->aa
    sum(abs(st$x)<.000000001)==sum(abs(aa)<.000000001)
    ifelse(abs(aa)<.000001,NA,aa)->aa
    sample(1:nrow(aa),80)->index
    #matplot(t(aa[index,]),type="l",lty=1,col="black",ylim=c(-3.5,3.5))
    #apply(aa,2,mean,na.rm=TRUE)->v
    #plot(v,main=nm,ylim=c(-2.5,2.5))
    #apply(aa,2,var,na.rm=TRUE)->v
    #plot(v,main=nm,ylim=c(0,2.5))
    plot(mod$means$alpha,ylim=c(-3,3),xlab="item",ylab="difficulty",pch=19)
    plot(mod$means$beta,ylim=c(-.2,2.5),xlab="item",ylab="discrimination",pch=19)
}
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
lapply(c("C-17","C-19"),fun,out=out)


