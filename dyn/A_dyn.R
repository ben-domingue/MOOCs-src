
est.dyn<-function(dat,nm,blk=NULL) {
    dat[[nm]]->L
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

                                        #priors
    pr<-list()
    pr$x.mu0<-matrix(0,nrow=N,ncol=1)
    pr$x.sigma0<-matrix(1,nrow=N,ncol=1)
    pr$beta.mu<-matrix(c(0,0),2,1)
    pr$beta.sigma<-matrix(c(1,0,0,1),2,2)
    pr$omega2<-matrix(0.1,nrow=N,ncol=1)

    nrow(dat$rc)
    ncol(dat$rc)
    dat$T
    str(dat)
    str(st)
    str(pr)

    dynIRT(dat,.starts=st,.priors=pr,
           .control = list(
               threads = 1,
               verbose = TRUE,
               thresh = 1e-6,
               maxit=500
           ))->mod

    mod$means$x->aa
    sum(abs(st$x)<.000000001)==sum(abs(aa)<.000000001)
    ifelse(abs(aa)<.000001,NA,aa)->aa
    sample(1:nrow(aa),80)->index
    matplot(t(aa[index,]),type="l",lty=1,col="black",ylim=c(-3.5,3.5))
    apply(aa,2,var,na.rm=TRUE)->v
    plot(v,main=nm,ylim=c(0,2.5))
    plot(mod$means$alpha,ylim=c(-3,3))
    plot(mod$means$beta,ylim=c(-.2,2.5))
}


par(mfrow=c(4,4))
est.dyn(dat,nm="Anes.v2")
est.dyn(dat,nm="MedStat")
est.dyn(dat,nm="MedStat.v2")
est.dyn(dat,nm="Nano",blk=1:75)
#est.dyn(dat,nm="QMSE.v3",blk=1:190)


#resgeo
#[1] 0.76 0.88 0.99 1.11 1.22 1.33 1.44 1.50
#medstat
# [1] 0.48 0.51 0.53 0.58 0.64 0.69 0.78 0.82 0.90 0.91


     
## #############################
## ## Estimate dynamic variational model using dynIRT()
## data(mq_data)
## nrow(mq_data$data.mq$rc)->N
## ncol(mq_data$data.mq$rc)->J
## mq_data$data.mq$T->T
## str(mq_data$data.mq)
## str(mq_data$cur.mq)
## str(mq_data$priors.mq)

## #starting values
## st<-list()
## #infun<-function(resp) {
## #    ifelse(as.matrix(resp)=="correct",1,0)->resp
## #}
## #infun(L$first_grade)->resp
## #colMeans(resp,na.rm=TRUE)->cm
## #-1*(cm-mean(cm,na.rm=TRUE))/sd(cm,na.rm=TRUE)->cm
## matrix(rep(0,J),ncol=1)->st$alpha
## matrix(rep(0,J),ncol=1)->st$beta
## st$x<-matrix(c(1,0),nrow=N,ncol=T)
## #priors
## pr<-list()
## pr$x.mu0<-matrix(0,nrow=N,ncol=1)
## pr$x.sigma0<-matrix(1,nrow=N,ncol=1)
## pr$beta.mu<-matrix(c(0,0),2,1)
## pr$beta.sigma<-matrix(c(1,0,0,1),2,2)
## pr$omega2<-matrix(0.1,nrow=N,ncol=1)

## lout <- dynIRT(.data = mq_data$data.mq,#
##                .starts=st,
##                .priors=pr,
##                #.starts = mq_data$cur.mq,
##                                              #.priors = mq_data$priors.mq,
##                .control = {list(
##                                threads = 1,
##                                verbose = TRUE,
##                                thresh = 1e-6,
##                                maxit=500
##                            )})

