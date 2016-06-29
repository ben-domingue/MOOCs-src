# redactCompare takes a matrix of response data, and a matrix of responses to redact as its inputs
# it creates beta and theta estimates using the Rasch model on the redacted data set
# it then applies those betas and thetas to estimate responses for the items that were redacted
# the estimated responses are compared to the observed responses to see if the redacted person/item repsonses systematically vary from the rest of the data

redactCompare <- function(resp,redact){
  # Need to implement a check for existence of coefReturn function
  
  n.Items <- ncol(resp)
  n.People <- nrow(resp)
  
  redacted <- redactee <- as.matrix(resp)
  redacted[which(redact==1)] <- NA # Remove all the items from the main response matrix
  redactee[which(redact!=1)] <- NA # Create a version of the main response matrix that is only the redacted items
  
                                        # need to parse out the entirely nas
                                        #   index <- rowSums(is.na(redacted))!=n.Items
                                        #   redacted <- redacted[index, ]
  
  # Create the betas and thetas using the redacted data
  mirt(redacted,1,itemtype="Rasch",method="EM",technical=list(NCYCLES=5000))->mirt
  theta <- fscores(mirt,response.pattern=redacted,method="WLE")
  theta <- theta[,ncol(theta)-1]
  beta <- coefReturn(mirt,2)
  
  # fill in the NAs for the theta
#   j = 1
#   theta2 <- c()
#   for (i in 1:n.People) {
#     if(index==TRUE) {
#       theta2[i] <- theta[j]
#       j = j + 1
#     } else theta2 = NA
#   }
  
  # for every person/item probability of getting it right
  pr <- matrix(ncol=n.Items,nrow=n.People)
  for (b in 1:n.Items) {
    for (t in 1:n.People) {
      kern <- exp(theta[t]+beta[b])
      pr[t,b] <- kern/(1+kern)
    }
  }
  
  # redacted to the skipped items
  pr[which(redact!=1)] <- NA
  
  # t-test
  ttest <- t.test(pr,redactee)
  print(ttest)
  
  # Return out all the stuff 
  list <- list(theta,beta,ttest,pr,redactee)
  names(list) <- c("theta","beta","ttest","predicted responses","observed responses")
  return(list)
}
