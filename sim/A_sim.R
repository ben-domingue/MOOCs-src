
ff<-function(M,N=1000,n=50) {
    sim<-function(a,b,N,M) {
        theta0<-rnorm(N)
        sl<-rnorm(N,mean=M,sd=.5)
        data.frame(th0=theta0,sl=sl)->th
        out<-list()
        for (i in 1:n) {
            th$th0+th$sl*i/n+rnorm(nrow(th),mean=0,sd=.03) -> x
            #rnorm(N)->x #putzing around with totally random theta at each item
            a[i]*(x-b[i])->kern
            exp(kern)->ek
            ek/(1+ek)->pr
            test<-runif(N)
            ifelse(pr>test,1,0)->out[[i]]
            x->thN
        }
        cor(theta0,thN)
        do.call("cbind",out)->resp
        resp
    }
    tr<-list()
    for (i in 1:50) {
        b<-rnorm(n)
        a<-runif(n,.8,1.2)
        sim(a,b,N,M=M)->resp
        library(mirt)
        mirt(resp,1,itemtype="2PL",verbose=FALSE)->mod
        coef(mod)->co
        co[-length(co)]->co
        do.call("rbind",co)->co
        #plot(a,co[,1])
        cor(a,co[,1])->c1
        cor(b,-1*co[,2])->c2
        c(c1,c2)->tr[[i]]
        ## ##
        ## a->x
        ## co[,1]->y
        ## y-x->z
        ## 1:length(z)->xv
        ## lm(z~xv)->mod
        ## coef(mod)[2]->m1
        ## lm(z~x)->mod
        ## coef(mod)[2]->m2
        ## ##
        ## b->x
        ## -1*co[,2]->y
        ## y-x->z
        ## 1:length(z)->xv
        ## lm(z~xv)->mod
        ## coef(mod)[2]->m3
        ## lm(z~x)->mod
        ## coef(mod)[2]->m4
        ## c(m1,m2,m3,m4)->tr[[i]]
    }
    do.call("rbind",tr)->out
    out
}

out<-list()
for (M in seq(0,2,by=.25)) ff(M=0)->out[[as.character(M)]]
out->hold
for (i in 1:length(out)) cbind(as.numeric(names(out)[i]),out[[i]])->out[[i]]
data.frame(do.call("rbind",out))->tab

names(tab)<-c("M","a.cor","a.b.cor")
par(mfrow=c(2,1))
for (i in 2:3) {
    boxplot(tab[[names(tab)[i] ]]~tab$M)
    abline(h=0)
}

    
##     plot(z,xlab="order in course",ylab="bias")
##     1:length(z)->xv
##     loess(z~xv)->mod
##     abline(h=0)
##     lines(xv,mod$fitted,col="red")
##     lm(z~xv)->mod
##     abline(mod,col="red",lty=2)
##                                         #
##     plot(x,y-x,pch=19,xlab="true",ylab="bias")
##     loess(z~x)->mod
##     abline(h=0)
##     cbind(x,mod$fitted)->tmp
##     tmp[order(tmp[,1]),]->tmp
##     lines(tmp,col="red")
##     lm(z~x)->mod
##     abline(mod,col="red",lty=2)

## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.5,3.5,2,1))
## pf<-function(x,y) {
##     y-x->z
##     plot(z,xlab="order in course",ylab="bias")
##     1:length(z)->xv
##     loess(z~xv)->mod
##     abline(h=0)
##     lines(xv,mod$fitted,col="red")
##     lm(z~xv)->mod
##     abline(mod,col="red",lty=2)
##                                         #
##     plot(x,y-x,pch=19,xlab="true",ylab="bias")
##     loess(z~x)->mod
##     abline(h=0)
##     cbind(x,mod$fitted)->tmp
##     tmp[order(tmp[,1]),]->tmp
##     lines(tmp,col="red")
##     lm(z~x)->mod
##     abline(mod,col="red",lty=2)
## }
## pf(a,co[,1])
## mtext(side=3,line=0,"discrimination")
## pf(b,(-1*co[,2]))
## mtext(side=3,line=0,"difficulty")

    
    
