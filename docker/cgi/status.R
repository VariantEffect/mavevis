#!/usr/bin/Rscript

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")

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

error.msg <- NULL

status <- if (any(grepl("Error",logs))) {
	error.msg <- logs[which(grepl("Error",logs)):length(logs)]
	"Error"
} else if (any(grepl("Done!",logs))) {
	"Done"
} else {
	"Processing"
}

#and respond to the HTTP request with the log contents 
# respondTEXT(paste(logs,collapse="\n"))
respondJSON(toJSON(list(
	status=status,
	log=paste(logs,collapse="\n"),
	message=paste(error.msg,collapse="\n")
)))
