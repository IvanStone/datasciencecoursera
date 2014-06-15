complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases    
    
    readcsvfilesnobs(directory, id)
}

formatfilenamematrix <- function(id, type="csv"){
    cbind(id=id, filename=paste(formatC(id, width=3, flag="0"), type, sep="."))
}

formatfilenamematrixdir <- function(filenames, directory){
    cbind(id=filenames[,"id"], filename=paste(directory,filenames[,"filename"], sep="/"))
}

outputnobsrow <- function(filenames, filedata, i){
    cbind(id=as.numeric(filenames[i,"id"]), 
          nobs=as.numeric(nrow(filedata)),
          cors=cor(filedata[,"sulfate"], filedata[,"nitrate"]))
}

readcsvfilesnobs <- function(directory, id){
    filenames <- formatfilenamematrix(id)    
    filenames <- formatfilenamematrixdir(filenames, directory)
    
    fileouput<-NULL
    for(i in 1:nrow(filenames)) {
        filedata <- read.csv(filenames[i,"filename"], header=TRUE, sep=",")
        filedata <- filedata[complete.cases(filedata),]
        
        fileouput<-rbind(fileouput, outputnobsrow(filenames, filedata, i))
    }
    #filedata
    #filenames
    fileouput<-as.data.frame(fileouput)
    fileouput
}