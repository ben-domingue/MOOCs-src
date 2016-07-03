load("mre-data.Rdata")


nrow(z$d$rc)
ncol(z$d$rc)
z$d$T
str(z$d)
str(z$s)
str(z$p)

library(emIRT)
dynIRT(z$d,.starts=z$s,.priors=z$p)->mod





#moocs data
dat$MedStat->L
infun<-function(resp) {
    ifelse(as.matrix(resp)=="correct",1,-1)->resp
    ifelse(is.na(resp),0,resp)
}
infun(L$first_grade)->fg

library(emIRT)
data(mq_data)
mq_data$data.mq$rc->z
sample(1:nrow(fg),nrow(z),replace=TRUE)->index1
sample(1:ncol(fg),ncol(z),replace=TRUE)->index2
fg[index1,index2]->z2
for (i in 1:nrow(z)) z2[i,]->z[i,]
z->mq_data$dat.mq$rc


nrow(mq_data$data.mq$rc)->N
ncol(mq_data$data.mq$rc)->J
mq_data$data.mq$T->T
str(mq_data$data.mq)
str(mq_data$cur.mq)
str(mq_data$priors.mq)

#starting values
st<-list()
#infun<-function(resp) {
#    ifelse(as.matrix(resp)=="correct",1,0)->resp
#}
#infun(L$first_grade)->resp
#colMeans(resp,na.rm=TRUE)->cm
#-1*(cm-mean(cm,na.rm=TRUE))/sd(cm,na.rm=TRUE)->cm
matrix(rep(0,J),ncol=1)->st$alpha
matrix(rep(0,J),ncol=1)->st$beta
st$x<-matrix(0,nrow=N,ncol=T)
#priors
pr<-list()
pr$x.mu0<-matrix(0,nrow=N,ncol=1)
pr$x.sigma0<-matrix(1,nrow=N,ncol=1)
pr$beta.mu<-matrix(c(0,0),2,1)
pr$beta.sigma<-matrix(c(1,0,0,1),2,2)
pr$omega2<-matrix(0.1,nrow=N,ncol=1)

lout <- dynIRT(.data = mq_data$data.mq,#
               #.starts=st,
               .priors=pr,
                                        .starts = mq_data$cur.mq,
                                             #.priors = mq_data$priors.mq,
               .control = {list(
                               threads = 1,
                               verbose = TRUE,
                               thresh = 1e-6,
                               maxit=500
                           )})
