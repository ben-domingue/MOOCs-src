coefReturn <- function(mirt.mod,coef) {
  coef(mirt.mod)->co
  co[-length(co)]->co
  do.call("rbind",co)->co
  co[,coef]
}

# pull out all the datasets
datagen <- function(i) {
  dat.sub <- dat[[i]]
  first_attempt <<- as.data.frame(dat.sub["first_attempt"]) 
  first_grade <<- as.data.frame(dat.sub["first_grade"])
  first_view <<- as.data.frame(dat.sub["first_view"])
  last_attempt <<- as.data.frame(dat.sub["last_attempt"])
  last_grade <<- as.data.frame(dat.sub["last_grade"]) 
  n_attempts <<- as.data.frame(dat.sub["n_attempts"])
  time_spent_attempting <<- as.data.frame(dat.sub["time_spent_attempting"])
  time_to_first_attempt <<- as.data.frame(dat.sub["time_to_first_attempt"])
  time_to_last_attempt <<- as.data.frame(dat.sub["time_to_last_attempt"])
}

