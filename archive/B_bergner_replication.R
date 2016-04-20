#replicating stuff from bergnet et al.
#http://dl.acm.org/citation.cfm?id=2723582

last_grade[,-1]->gr
as.matrix(gr)->gr
ifelse(gr=="none",NA,gr)->gr
apply(gr,2,table)
rowSums(!is.na(gr))->rs
plot(density(rs))
cumsum(table(rs))
rs/ncol(gr)->rs
rs>=0.5 -> pi

#consider attrition here, we only get 283/1553 who have at least 100 valid entries
hist(breaks=20,rs[pi],xlim=c(0.5,1))

#attrition as a function of item position
colMeans(!is.na(gr[pi,]))->cs
plot(cs,type="l")

apply(n_attempts[pi,-1],2,function(x) median(as.numeric(x),na.rm=TRUE))->med.n.attempts
cumsum(table(med.n.attempts))/136

last_grade[pi,-1]->resp
ifelse(as.matrix(resp)=="correct",1,0)->resp
NULL->colnames(resp)
rowSums(resp,na.rm=TRUE)->ec
n_attempts[pi,-1]->tmp
as.matrix(apply(tmp,2,as.numeric))->tmp
ifelse(tmp==1,1,0)->tmp
rowSums(resp*tmp,na.rm=TRUE)->cfa
plot(density(cfa),col="red",xlim=c(0,135))
lines(density(ec),col="black")
      


n_attempts[pi,-1]->tmp
as.matrix(apply(tmp,2,as.numeric))->tmp

rowMeans(tmp,na.rm=TRUE)->mean.tries

plot(cfa,mean.tries)
cor(cfa,-1/mean.tries)
