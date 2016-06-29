# this file is testing the development of a function, pos, that determines the position that an item was taken in. 
# It needs further investigaiton about tie handling, for now it seems to be working 
# negative output values means an item was taken early, positive mean it was taken late

# 
# t1 <- c("3/10/14", "3/11/14", "3/12/14", "3/13/14") 
# t2 <- c("3/13/14", "3/12/14", "3/11/14", "3/10/14") 
# t3 <- c("3/13/14", "3/11/14", "3/12/14", "3/10/14")
# t4 <- c("3/11/14", "3/11/14", "3/11/14", "3/10/14") 
# t5 <- c("3/10/14",  NA,       NA,        "3/11/14") 
# t6 <- c("3/12/14",  NA,       NA,        "3/11/14") 
# test <- rbind(r1,r2,r3,r4,r5,r6)
# 
# p1 <- c(0,0,0,0)
# p2 <- c(3, 1, -1, -3)
# p3 <- c(3, 0, 0, -3)
# p4 <- c(1, 0, 0, -3) # maybe want 1 0 0 -3 (have to think about tie handling?) / get 1 1 1 -3
# p5 <- c(0, NA, NA, 0)
# p6 <- c(1, NA, NA, -1)
# check <-  rbind(p1,p2,p3,p4,p5,p6)

positionTaken <- function(x) {
  tmp <- c()
  list <- c()
  order <- sort(x, index.return=1)[[2]] # this pulls the ordering index (the first value is the position of the lowest item, the last value is the position of the highest item)
  for(i in 1:length(x)) {
    item <- order[i]
    tmp[item] <- i - item
  }
  j <- 1
  for(i in 1:length(x)) {
    if (is.na(x[i])) list[i] <- NA
    else{
      list[i] <- tmp[j]
      j <- j +1
    }
  }
  return(list)
}
# 
# t(apply(test,1,positionTaken))
# 
# t(apply(test,1,positionTaken)) == check
