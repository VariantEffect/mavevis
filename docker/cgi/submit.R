#!/usr/bin/Rscript

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")

launcher.loc <- "/var/www/html/mavevis/mavevis_launcher.R"

#read data from HTTP POST request
postdata <- readPOST()

#check that minimal parameters were supplied
if (!all(c("scoresetID","uniprot","pdb","mainChain") %in% names(postdata))) {
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
writeChar(toJSON(c(postdata,job=jobID)),con)
close(con)

# #respond to HTTP request with the jobID
respondJSON(toJSON(list(jobID=jobID)))
