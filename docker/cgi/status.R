#!/usr/bin/Rscript

# Copyright (C) 2018  Jochen Weile, Roth Lab
#
# This file is part of MaveVis.
#
# MaveVis is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# MaveVis is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with MaveVis.  If not, see <https://www.gnu.org/licenses/>.

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/html/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/html/mavevis/cache/")

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
	i <- which(grepl("Error",logs))[[1]]
	error.msg <- logs[i:length(logs)]
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
