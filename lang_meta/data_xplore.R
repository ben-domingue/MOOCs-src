setwd("/home/bd/Dropbox/moocs/data/lang_data/Stata/data")
#read.csv("HumanitiesandScience_StatLearning_Winter2015first_grade.csv")->fg
#read.csv("HumanitiesandScience_StatLearning_Winter2015playback_collapsed.csv")->pb
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Problem_Events.csv")->pe
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Problems_With_Item_types.csv")->pr
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Registration.csv")->re

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
    range(c(clk.median,clk.min),na.rm=TRUE)->yl
    plot(clk.median,pch=19,main=file.nm,sub=paste(nrow(z$first_attempt),"people"),ylim=yl)
    points(clk.min,pch=19,col="red")
    points(clk.5,pch=19,col="blue")
    legend("topleft",c("median","5th percentile","min"),pch=19,col=c("black","blue","red"))
    ## colMeans(resp,na.rm=TRUE)->cm
    ## rowMeans(resp,na.rm=TRUE)->rm
    ## coors<-numeric()
    ## for (i in 1:ncol(resp)) cor(resp[,i],rm,use='p')->coors[i]
    print(file.nm)
}
list.files(pattern="*Item_types_pkey.csv")->lf
pdf("/tmp/time.pdf")
lapply(lf,ctt)
dev.off()

    
    
    
