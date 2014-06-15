pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    filenames <- formatfilename(id)
    data <- readcsvfiles(directory,filenames)
    col <- getcolumnindex(pollutant,data)
    data <- data[complete.cases(data[,col]),]
    
    round(mean(data[,col]), digits = 3)
}

formatfilename <- function(id, type="csv"){
    filenames <- formatC(id, width=3, flag="0")
    paste(filenames, type, sep=".")
}

readcsvfiles <- function(directory, filenames){
    files<-paste(directory,filenames,sep="/")
    filedata<-NULL
    for(file in files){
        filedata<-rbind(filedata, read.csv(file, header=TRUE, sep=","))
    }
    filedata
}

getcolumnindex <- function(colname, datatable) {
    which(names(datatable)==colname) 
}