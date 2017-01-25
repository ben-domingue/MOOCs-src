setwd("/home/bd/Dropbox/moocs/data/lang_data/Stata/data")
#read.csv("HumanitiesandScience_StatLearning_Winter2015first_grade.csv")->fg
#read.csv("HumanitiesandScience_StatLearning_Winter2015playback_collapsed.csv")->pb
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Problem_Events.csv")->pe
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Problems_With_Item_types.csv")->pr
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Registration.csv")->re

time<-function(file.nm,threshold=0.5) {
    read.csv(file.nm)->x
    unique(x$problem_key)->item.nms
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=threshold)->L
    sapply(L,is.null)->index
    L[!index]->L
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_key",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    z<-list()
    for (var in c("first_attempt_time","first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    #par(mfrow=c(2,1))
    apply(z$first_attempt_time,2,median,na.rm=TRUE)->clk.median
    apply(z$first_attempt_time,2,min,na.rm=TRUE)->clk
    ifelse(!is.finite(clk),NA,clk)->clk.min
    apply(z$first_attempt_time,2,quantile,na.rm=TRUE,.05)->clk
    ifelse(!is.finite(clk),NA,clk)->clk.5
    apply(z$first_attempt_time,2,quantile,na.rm=TRUE,.95)->clk
    ifelse(!is.finite(clk),NA,clk)->clk.95
    range(c(clk.95,clk.min),na.rm=TRUE)->yl
    plot(clk.median,pch=19,main=file.nm,sub=paste(nrow(z$first_attempt),"people"),ylim=yl)
    points(clk.min,pch=19,col="red")
    points(clk.5,pch=19,col="blue")
    points(clk.95,pch=19,col="green")
    legend("topleft",c("95th percentile","median","5th percentile","min"),pch=19,col=c("green","black","blue","red"))
    ## colMeans(resp,na.rm=TRUE)->cm
    ## rowMeans(resp,na.rm=TRUE)->rm
    ## coors<-numeric()
    ## for (i in 1:ncol(resp)) cor(resp[,i],rm,use='p')->coors[i]
    print(file.nm)
}
list.files(pattern="*Item_types_pkey.csv")->lf
pdf("/tmp/time.pdf")
lapply(lf,time)
dev.off()



ctt<-function(file.nm,threshold=0.5) {
    read.csv(file.nm)->x
    unique(x$problem_key)->item.nms
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=threshold)->L
    sapply(L,is.null)->index
    L[!index]->L
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_key",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    z<-list()
    for (var in c("first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    #par(mfrow=c(2,1))
    apply(z$first_attempt,2,mean,na.rm=TRUE)->pv
    rowMeans(z$first_attempt,na.rm=TRUE)->rs
    pt_biserial<-function(x,person.sum) { 
        num<-mean(person.sum[x==1],na.rm=TRUE)-mean(person.sum) #this is the difference between the mean for those with a 1 on the item versus the overall mean
        num*sqrt(mean(x,na.rm=TRUE)/(1-mean(x,na.rm=TRUE)))/sd(person.sum) 
    }
    pb<-numeric()
    for (i in 1:ncol(z$first_attempt)) {
                                        #pt_biserial(x=z$first_attempt[,i],person.sum=rs)->pb[i]
        cor(rs,z$first_attempt[,i],use='p')->pb[i]
    }
    #
    plot(pv,pch=19,main=file.nm,sub=paste(nrow(z$first_attempt),"people"),ylim=c(0,1),type="b")
    points(pb,pch=19,col="red",type="b")
    print(file.nm)
}
list.files(pattern="*Item_types_pkey.csv")->lf
pdf("/tmp/ctt.pdf")
lapply(lf,ctt)
dev.off()

attrit<-function(file.nm,threshold=0.5) {
    read.csv(file.nm)->x
    unique(x$problem_key)->item.nms
    length(item.nms)->N #total number of items
    split(x,x$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=threshold)->L
    sapply(L,is.null)->index
    L[!index]->L
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_key",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    z<-list()
    for (var in c("first_attempt")) {
        lapply(L,make.wide,nms=item.nms,col.nm=var)->out
        do.call("rbind",out)->z[[var]]
    }
    #par(mfrow=c(2,1))
    rowMeans(z$first_attempt,na.rm=TRUE)->rm
    m.na<-m.nna<-m.1<-m.0<-numeric()
    for (i in 1:ncol(z$first_attempt)) {
        is.na(z$first_attempt[,i])->index1
        mean(rm[index1])->m.na[i]
        mean(rm[!index1])->m.nna[i]
        mean(rm[!index1 & z$first_attempt[,i]==1])->m.1[i]
        mean(rm[!index1 & z$first_attempt[,i]==0])->m.0[i]
    }
    plot(m.na,type="l",col="red",ylim=c(0,1),pch=19,main=file.nm,sub=paste(nrow(z$first_attempt),"people"))
    points(m.nna,type="l",pch=19)
    points(m.1,type="l",col="blue",pch=19)
    points(m.0,type="l",col="green",pch=19)
    legend("bottomleft",bty="n",c("na","not na","right","wrong"),lty=1,lwd=2,col=c("red","black","blue","green"))
}
list.files(pattern="*Item_types_pkey.csv")->lf
pdf("/tmp/attrit.pdf")
lapply(lf,attrit)
dev.off()

    
    
    
