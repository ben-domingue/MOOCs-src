load("desc1.Rdata")
c("C-6","C-8","C-10","C-15","C-17","C-19")->nms
dat[nms]->dat
save(dat,file="imps.Rdata")
