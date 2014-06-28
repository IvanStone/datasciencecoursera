best <- function(state, outcome) {
    ## Read outcome data
    csvdata<-getassignmentdata()
    
    ## Check that state and outcome are valid
    statedata<-checkstate(state, csvdata)
    outcomedata<-checkoutcome(outcome, statedata)
    
    setoutcome<-'^Hospital.30.Day.Death..Mortality..Rates.from.'
    outcomedata<-checksetoutcome(setoutcome,outcomedata)
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate?
    outcomedata<-prepareoutcomedata(outcomedata)
    
    outcomedata<-as.vector(outcomedata[which.min(outcomedata[,3]),2])
    
    outcomedata
}

##-----------------------------------------------------------------------------

readcsvfile <- function(csvfile){
    returndata<-NULL
    returndata<-read.csv(csvfile, colClasses = "character", na.strings = "NA")
    ##returndata<-read.csv(csvfile, header=TRUE, sep=",")
    returndata
}

getassignmentdata<-function(){
    csvfile<-"rprog-data-ProgAssignment3-data/19.outcome-of-care-measures.csv"
    csvdata<-NULL
    csvdata<-readcsvfile(csvfile)
}

getstate <- function(state, data) {
    returndata<-data[which(data$State == state),]
    returndata
}

getoutcome <- function(outcome, data, maxdistance=0, ignorecase=FALSE, fixed=TRUE, cols=1:2) {
    returndata<-data[,c(cols,
                        agrep(outcome, 
                            names(data), 
                            max.distance = maxdistance, 
                            ignore.case=ignorecase,
                            fixed=fixed))]
    returndata
}

checkstate<-function(state, data) {
    statedata<-NULL
    statedata<-getstate(state, data)
    if(nrow(statedata) == 0) {
        stop("invalid state")
    }
    statedata
}

checkoutcome<-function(outcome, data, ...) {
    outcomedata<-NULL
    outcomedata<-getoutcome(outcome, 
                            data, 
                            c(cost=1,all=0,insertions=1,deletions=1,substitutions=1), 
                            TRUE,
                            ...)
    if(ncol(outcomedata) <= 2) {
        stop("invalid outcome")
    }
    outcomedata
}

checksetoutcome<-function(outcome, data, ...) {
    outcomedata<-NULL
    outcomedata<-getoutcome(outcome, data, fixed=FALSE, ...)
    if(ncol(outcomedata) <= 2) {
        stop("invalid fixed outcome")
    }
    outcomedata
}

prepareoutcomedata<-function(outcomedata, col=3, ordercol=2) {
    outcomedata[,col]<-replace(outcomedata[,col], outcomedata[,col]=="Not Available",NA)
    outcomedata[,col]<-as.numeric(outcomedata[,col])
    outcomedata<-outcomedata[order(outcomedata[,ordercol]),]
    outcomedata
}