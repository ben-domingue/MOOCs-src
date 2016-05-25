courses<-list(ResGeo="EarthSciences_ResGeo202_Spring2015",
              GlobalHealth="GlobalHealth_IWHHR_Summer2014",
              MathEdu="Education_115SP_2015",
              Stocks="GSB_StocksBonds_SelfPaced",
              MathEdu.v2="Education_EDUC115-S_Spring2014",
              StatLearn="HumanitiesandScience_StatLearning_Winter2015",
              OpenKnowl="Education_OpenKnowledge_Fall2014",
              Econ1="HumanitiesSciences_Econ-1_Summer2014",
              Compilers="Engineering_Compilers_Fall2014",
              Econ1="HumanitiesSciences_Econ_1_Summer2015",
              ChemE="Engineering_IntroChE_SelfStudy",
              EP="HumanitiesSciences_EP-101_Spring2015",
              Nano="Engineering_Nano_Summer2014",
              Anes="Medicine_ANES204_Fall2014",
           #"Engineering_Networking-SP_SelfPaced",
              Anes.v2="Medicine_ANES205_Fall2014",
              QMSE="Engineering_QMSE01._Autumn2015",
              MedStat="Medicine_MedStats_Summer2014",
              QMSE.v2="Engineering_QMSE-01_Fall2014",
              MedStat.v2="Medicine_MedStats._Summer2015",
              QMSE.v3="Engineering_QMSE-02_Winter2015",
              MolFound="Medicine_MolFoundations_SelfPaced",
              DigDeeper="English_DiggingDeeper1_Winter2015",
              SciWrite="Medicine_Sci-Write_Fall2014",
              DigDeep2="English_diggingdeeper2_Spring2015",
              SciWrite.v2="Medicine_SciWrite._Fall2015",
              WomensHealth="GlobalHealth_IntWomensHealth_Jan2015",
              Haptics="SelfPaced_Haptics_2014",
              WomensHealth.v2="GlobalHealth_INT.WomensHealth_July2015"
           )

tab<-dat<-list()
for (course in unlist(courses)) {
    id.list<-L<-list()
    setwd("/home/bd/Dropbox/moocs/data/datastage.stanford.edu/researcher/EDUC_353A/exports_5-12/raws")
    read.csv(paste(course,"_ProblemMetadata.csv",sep=""))->pm
    setwd(paste("/home/bd/Dropbox/moocs/data/datastage.stanford.edu/researcher/EDUC_353A/exports_5-12/transforms/",course,sep=""))
    list.files()->lf
    grep("export_summary.txt",lf)->index
    lf[-index]->lf
    for (ii in 1:length(lf)) {
        lf[ii]->fn
        skip<-0
        read.csv(fn,header=FALSE)->fv
        if (ii==1) {
            fv[,1]->ids
        } else {
            match(ids,fv[,1])->index
            fv[index,]->fv
        }
        fv[,1]->id.list[[fn]]
        fv[,-1]->fv
        strsplit(as.character(fv[1,]),"-problem-",fixed=TRUE)->tmp
        sapply(tmp,"[",2)->tmp
        strsplit(tmp,"_")->tmp
        sapply(tmp,"[",1)->tmp
        match(pm[,1],tmp)->index
        if (any(is.na(index))) {
            skip<-1
        }
        if (skip==0) {
            fv[,index]->fv
            fv[-1,]->fv
            fv->L[[gsub(".csv","",fn)]]
                                        #assign(gsub(".csv","",fn),fv)
        }
    }
    for (i in 2:length(id.list)) print(all(id.list[[1]]==id.list[[i]]))
                                        #
    if (skip!=1) {
                                        #
        L$last_grade->gr
        as.matrix(gr)->gr
        ncol(gr)->N
        ifelse(gr=="none",NA,gr)->gr
        rowSums(!is.na(gr))->rs
        rs/ncol(gr)->rs
        rs>=0.5 -> pi #these are people that complete at least half the items
        grep(course,courses)->index
        if (sum(pi)>300) {
            for (ii in 1:length(L)) L[[ii]][pi,]->L[[ii]]
            L->dat[[names(courses)[index] ]]
        }
        c(course,nrow(gr),sum(pi),N)->tab[[names(courses)[index]]]
    }
}
order(names(dat))->index
dat[index]->dat
do.call("rbind",tab)->tab

save(dat,file="/home/bd/Dropbox/moocs/data/proc/desc1.Rdata")
