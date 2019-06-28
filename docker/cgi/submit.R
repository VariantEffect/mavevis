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
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")

launcher.loc <- "/var/www/html/mavevis/mavevis_launcher.R"

#read data from HTTP POST request
postdata <- readPOST()

#check that minimal parameters were supplied
if (!(("scoresetID" %in% names(postdata)) & any(c("uniprot","WT") %in% names(postdata)))) {
# if (!all(c("scoresetID","uniprot","pdb","mainChain") %in% names(postdata))) {
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
