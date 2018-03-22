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

#check if jobID was supplied
if (!("jobID" %in% names(postdata))) {
	#otherwise respond with error message
	respond400("No jobID provided!")
	quit(save="no",status=0)
}

#extract jobID from POST
jobID <- postdata$jobID

#locate logfile
logfile <- paste0(cache.dir,"progress_",jobID,".log")
#if the logfile doesn't exist yet, the job hasn't started yet.
if (!file.exists(logfile)) {
	logs <- "Waiting for job"
} else {
	#otherwise read the logs
	logs <- scan(logfile,what="character",sep="\n")
}

#and respond to the HTTP request with the log contents 
respondTEXT(paste(logs,collapse="\n"))
