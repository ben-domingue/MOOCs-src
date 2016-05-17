load(file="~/moocs/resp.Rdata")
sapply(dat,length)->N
dat[N>0]->dat
dat[[26]]->L

##########################################################
for (i in 1:length(L)) assign(names(L)[i],L[[i]])
last_grade[,-1]->gr
as.matrix(gr)->gr
ncol(gr)->N
ifelse(gr=="none",NA,gr)->gr
rowSums(!is.na(gr))->rs
rs/ncol(gr)->rs
rs>=0.5 -> pi #these are people that complete at least half the items


first_grade[pi,-1]->fg
last_grade[pi,-1]->lg
fun<-function(x) {
    for (i in 1:ncol(x)) {
        ifelse(x[,i]=="none",NA,x[,i])->x[,i]
        ifelse(x[,i]=="correct",1,0)->x[,i]
    }
    x
}
fun(fg)->fg
fun(lg)->lg

par(mfrow=c(1,2))
plot(density(rowSums(lg,na.rm=TRUE)))
lines(density(rowSums(fg,na.rm=TRUE)),col="red")
plot(density(rowMeans(lg,na.rm=TRUE)))
lines(density(rowMeans(fg,na.rm=TRUE)),col="red")

n_attempts[pi,-1]->na
list(fg,lg,na)->L
save(L,file="/tmp/resgeo.Rdata")
