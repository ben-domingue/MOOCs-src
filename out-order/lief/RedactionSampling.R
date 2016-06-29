library(mirt)
load("/Users/williamlief/Documents/DataHouse/Moocs/resp_5-12.Rdata")
setwd("/Users/williamlief/Documents/Research Projects/MOOC Growth")
source("functions.r")
source("positionTaken.r")
source("redactCompare.r")

datagen("MedStat.v2")

# grab the first recorded grade and code it as 0/1
resp <- first_grade
n.Items <- length(resp)
n.People <- nrow(resp)
resp[,1:n.Items][resp[,1:n.Items] == "correct"] = 1
resp[,1:n.Items][resp[,1:n.Items] == "incorrect"] = 0
resp <- sapply(resp, as.numeric)
resp <- as.data.frame(resp)

# First pass at redaction
## Find late taken items
time <- as.matrix(first_attempt)
nas <- rep(NA,n.People)

shiftLate <- time[,-1] 
shiftLate <- cbind(shiftLate,nas) 
Late <- ifelse(time > shiftLate, 1, 0) # if an item was taken after the next item, its late (1), else 0

rc.late <- redactCompare(resp,Late)


# Random redaction
rand <- matrix(ncol=n.Items,nrow=n.People)
n.Rand <- n.Items*n.People
rand[sample(1:n.Rand,sum(Late,na.rm=T))] <- 1

rc.rand <- redactCompare(resp,rand)

 # find what position each item was taken in for each person - we know that the item variables are in the correct order, see data parse.R

order <- t(apply(first_attempt,1,positionTaken))

# median/mean position by item
plot(apply(order,2,median,na.rm=T))
plot(apply(order,2,mean,na.rm=T))
# median position is zero for most items (except last three - fine)
# mean position is never zero, seeing bifurcation between inlecture q's and quizzes

# lets identify the late items using a few different cut offs
lateMat <- function(cut){
  late <- matrix(nrow=n.People,ncol=n.Items)
  late[order>cut] <- 1
  late[order<=cut] <- 0
  return(late)
}

late5 <- lateMat(5)
late10 <- lateMat(10)
late25 <- lateMat(25)
late50 <- lateMat(50)

# lets check some early items
earlyMat <- function(cut){
  early <- matrix(nrow=n.People,ncol=n.Items)
  early[order<cut] <- 1
  early[order>=cut] <- 0
  return(early)
}

early50 <- earlyMat(-50)

rc.5 <- redactCompare(resp,late5)
rc.10 <- redactCompare(resp,late10)
rc.25 <- redactCompare(resp,late25)
rc.50 <- redactCompare(resp,late50)
rc._50 <- redactCompare(resp,early50)

# print out the t-tests, the sum of the redaction matrix gives us the number redacted
rc.rand[3]; sum(rand,na.rm=T)
rc.late[3]; sum(Late,na.rm=T)
rc.5[3]; sum(late5,na.rm=T)
rc.10[3]; sum(late10,na.rm=T)
rc.25[3]; sum(late25,na.rm=T)
rc.50[3]; sum(late50,na.rm=T)
rc._50[3]; sum(early50,na.rm=T)


#### 
# lets do a zero redaction and see what we get
redact0 <- matrix(nrow=n.People, ncol=n.Items)
rc.none <- redactCompare(resp,redact0)



