rankall <- function(outcome, num = "best") {
    ## Read outcome data
    csvdata<-getassignmentdata()
    
    ## Check that state and outcome are valid
    outcomedata<-checkoutcome(outcome, csvdata, cols=c(1,2,7))
    
    setoutcome<-'^Hospital.30.Day.Death..Mortality..Rates.from.'
    outcomedata<-checksetoutcome(setoutcome, outcomedata, cols=c(1,2,3))
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    outcomedata<-prepareoutcomedata(outcomedata, 4)
    outcomedata<-split(outcomedata, outcomedata[,3])
    
    if(num=="best") {
        outcomedata<-sapply(outcomedata, function(outcomedata) as.vector(outcomedata[which.min(outcomedata[,4]),c(2,3)]))
    }
    else if (num=="worst") {
        outcomedata<-sapply(outcomedata, function(outcomedata) as.vector(outcomedata[which.max(outcomedata[,4]),c(2,3)]))
    }
    else {
        outcomedata<-sapply(outcomedata, function(outcomedata) which.maxN(outcomedata,4,num)[,c(2,3)])
    }
    
    outcomedata<-data.frame(t(outcomedata))
    colnames(outcomedata)<-c("hospital","state")
    outcomedata
}