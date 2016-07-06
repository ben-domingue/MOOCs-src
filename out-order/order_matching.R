load("desc1.Rdata")
#dat$`C-19`->L

OOfun<-function(L, #this is just the list of course-specific data
                look.ahead=c(1,2,5,10,25), #the look.ahead window will be used to define OO. response i will have to occur after response i+look.ahead to be OO (slight modification of this rule to deal with NA values)
                threshold=0.3) #some items end of having way too many OO responses. if the item has more than threshold proportion of the responses OO, remove it.
{
    tr<-list()
    for (look.ahead in look.ahead) {
########################################################
##prepping data
        infun<-function(resp) { #this just converts responses to 0/1
            ifelse(as.matrix(resp)=="correct",1,0)->resp
            resp
        }
        infun(L$first_grade)->fg
        L$first_attempt->fa.hold
        apply(fa.hold,2,as.numeric)->fa
        min(fa,na.rm=TRUE)->m
        fa-m->fa #easier to just rescale everything based on first observed item response in course
########################################################
## defining OO items
##strategy:
##find difference in time between column i and all columns i+1:N
##for each row, look at the first non-NA value (we have lots of NAs, so i don't want to omit all items that come before NAs)
##test whether that value is greater than 0. if so, item i occurred before the next item to which they responded (by next, i mean item order). 
        oo<-list()
        for (i in 1:(ncol(fa)-look.ahead)) {
            fa[,((i+look.ahead):ncol(fa))]-fa[,i] -> del
            if (class(del)=="numeric") matrix(del,ncol=1)->del
            find.min<-function(x) {
                x[!is.na(x)]->x
                if (length(x)>0) x[1]<0 else NA
            }
            apply(del,1,find.min)->mm
            mm->oo[[i]] #this contains the OO items for each column
        }
        do.call("cbind",oo)->ord #this will contain all OO items minus the last column
        cbind(ord,matrix(TRUE,nrow=nrow(ord),ncol=look.ahead))->ord #the last column can't be OO based on the def'n used here. 
        #
        colMeans(ord,na.rm=TRUE)->cm
        for (i in 1:ncol(ord)) if (cm[i]>threshold) NA->ord[,i]
########################################################
##now remove those responses for OO items
        ifelse(ord,NA,fg)->fg2
########################################################
##A few courses had items that, after redaction, were all 1s or 0s. want to get rid of those. 
        lapply(as.data.frame(fg2),unique)->tab
        sapply(tab,function(x) 0 %in% x)->t0
        sapply(tab,function(x) 1 %in% x)->t1
        fg2[,t0 & t1]->fg2
########################################################
##Prep analysis, get all those who had at least one correct response
##(this is really meant to ensure that they aren't all NA, but i'm ok with the side effectd that we don't have people with all incorrect responses). 
        rowSums(!is.na(fg2))->rs
        rs>0 -> test
        fg2[test,]->fg2
        fg[test,]->fg
########################################################
##assemble a dataset containing:
##y-there true response (note that it comes from fg and not the redacted fg2)
##x-their mean for all other items
##oo-an indicator of whether the item was OO or not. 
        out<-list()
        for (ii in 10:ncol(fg2)) {
            fg2[,1:(ii-1)]->tmp
            rowMeans(tmp,na.rm=TRUE)->rs
            which(ord[,ii])->oo.index
            data.frame(y=fg[,ii],x=rs)->tmp
            ifelse(1:nrow(tmp) %in% oo.index,1,0)->tmp$oo
                                        #glm(y~x+oo,tmp,family="binomial")->m1 ##i was originally considering item-specific analyses but this is foolish. 
                                        #summary(m1)$coef[3,]->out[[as.character(ii)]]
            tmp->out[[as.character(ii)]]
        }        
        do.call("rbind",out)->tab
        data.frame(tab)->tmp
########################################################
##our hypothesis: a y=1 response should be more likely if oo=1 (conditional on x)
##glm(y~x+oo,tmp,family="binomial")->m1 #takes too long
        lm(y~x+oo,tmp)->m1 #hoping for a positive coef on oo
        summary(m1)$coef->tr[[as.character(look.ahead)]]
    }
    tr
}

out<-list()
for (nm in c("C-10","C-17","C-19","C-6","C-8")) OOfun(dat[[nm]])->out[[nm]]

fun<-function(xx) {
    sapply(xx,function(x) x[3,1])
}
lapply(out,fun)->zz
do.call("rbind",zz)->zz
matplot(t(zz),type="l",lty=1,col="black")


sapply(out,nrow)->N
out[N==3]->out
lapply(out,function(x) x[,1])->tab
do.call("rbind",tab)->tab
plot(data.frame(tab))->tab
