setwd("/home/bd/Dropbox/moocs/data/lang_data/Stata/data")
#read.csv("HumanitiesandScience_StatLearning_Winter2015first_grade.csv")->fg
#read.csv("HumanitiesandScience_StatLearning_Winter2015playback_collapsed.csv")->pb
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Problem_Events.csv")->pe
read.csv("HumanitiesandScience_StatLearning_Winter2015_Problems_With_Item_types.csv")->pr
#read.csv("HumanitiesandScience_StatLearning_Winter2015_Registration.csv")->re

ctt<-function(x,threshold=0.5) {
    unique(x$problem_display_name)->item.nms
    length(item.nms)->N #total number of items
    split(pr,pr$anon_screen_name)->L #each element of L is the info for a single person
    length(L)->n.total #total number of people
    ##this is going to get those who respond to at least threshold % of items
    get.learners<-function(y,N,threshold) { 
        nrow(y)->n
        if (n/N>threshold) y else NULL
    }
    lapply(L,get.learners,N=N,threshold=threshold)->L
    sapply(L,is.null)->index
    L[!index]->L
    #data.frame(do.call("rbind",L))->x
    ##now we'll start putting together item-level data
    make.wide<-function(resp,nms,col.nm="first_attempt") {
        resp[,c("problem_display_name",col.nm),]->resp
        match(resp[,1],nms)->index
        rep(NA,length(nms))->out
        for (i in 1:length(index)) resp[i,2]->out[index[i] ]
        out
    }
    lapply(L,make.wide,nms=item.nms)->out
    do.call("rbind",out)->resp
    colMeans(resp,na.rm=TRUE)->cm
    rowMeans(resp,na.rm=TRUE)->rm
    coors<-numeric()
    for (i in 1:ncol(resp)) cor(resp[,i],rm,use='p')->coors[i]
    
    
    
