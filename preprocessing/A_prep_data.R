base.dir<-"~/moocs/353A/datastage.stanford.edu/researcher/EDUC_353A/exports" #ozzy, #base.dir<-"/home/bd/Dropbox/moocs/data/353A/datastage.stanford.edu/researcher/EDUC_353A/exports"
list.files()->lf
grepl("^index",lf)->index

dat<-list()
for (dir in lf[!index]) {
    paste(base.dir,dir,sep="/")->dir.nm
    list.files(path=dir.nm,pattern="*.csv")->lf
    L<-list()
    for (fn in lf) {
        #assign(strsplit(fn,".csv")[[1]][1],read.csv(fn,colClasses="character"))
        read.csv(paste(dir.nm,fn,sep="/"),colClasses="character")->x
        x->L[[strsplit(fn,".csv")[[1]][1]]]
    }
    L->dat[[dir]]
}
save(dat,file="/tmp/resp.Rdata")

