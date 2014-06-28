rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    csvdata<-getassignmentdata()
    
    ## Check that state and outcome are valid
    statedata<-checkstate(state, csvdata)
    outcomedata<-checkoutcome(outcome, statedata)
    
    setoutcome<-'^Hospital.30.Day.Death..Mortality..Rates.from.'
    outcomedata<-checksetoutcome(setoutcome,outcomedata)
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    outcomedata<-prepareoutcomedata(outcomedata)
    
    if(num=="best") {
        outcomedata<-as.vector(outcomedata[which.min(outcomedata[,3]),2])
    }
    else if (num=="worst") {
        outcomedata<-as.vector(outcomedata[which.max(outcomedata[,3]),2])
    }
    else {
        outcomedata<-which.maxN(outcomedata,3,num)[,2]
        ##outcomedata<-outcomedata
    }
    
    outcomedata
}

which.maxN <- function(x, col, N=2) {
    len <- length(x[,col])
    if(N>len){
        x[1,col]<-NA
    }
    x[sort.list(x[,col]),][N,]
}
