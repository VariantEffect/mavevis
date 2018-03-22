#!/usr/bin/Rscript

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
setMessageSink("/var/www/mavevis/logs/exec.log")

#Caching directory
cache.dir <- "/cache/"

#read data from HTTP POST request
postdata <- readPOST()

#check that minimal parameters were supplied
if (!all(c("ssid","uniprotId","pdbs","mainChains") %in% names(postdata))) {
	#otherwise respond with error message
	respond400("Missing arguments!")
	#exit status must be 0 to avoid "Internal Server Error"
	quit(save="no",status=0) 
}

#create a new job ID
jobID <- makeUUID()

#Create an input file with the provided parameters
#This will be picked up by the daemon, who will actually launch the job.
inDataFile <- paste0(cache.dir,"input_",jobID,".json")
con <- file(inDataFile,open="w")
writeChar(toJSON(c(postdata,jobID=jobID)))
close(con)

#respond to HTTP request with the jobID
respondJSON(toJSON(list(jobID=jobID)))
