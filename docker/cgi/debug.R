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

#read data from HTTP GET
input <- readGET()

if (!("code" %in% names(input))) {
	#otherwise respond with error message
	respond400("No code provided!")
	quit(save="no",status=0)
}
if (input$code != "lamarama") {
	respond400("Code incorrect!")
	quit(save="no",status=0)
}


#Read file contents into a string
readFile <- function(filename) {
	if (file.exists(filename)) {
		# con <- file(filename,open="r")
		con <- pipe(paste("tail -1000",filename))
		lines <- readLines(con)
		close(con)
		return(paste(lines,collapse="\n"))
	} else {
		return("No entries.")
	}
}

#List of logfiles to read
files <- c(
	paste0(cache.dir,"daemon.log"),
	paste0(cache.dir,"statusDB.csv"),
	paste0(cache.dir,"statusHistory.csv"),
	"/var/www/html/mavevis/logs/access.log",
	"/var/www/html/mavevis/logs/error.log",
	"/var/log/apache2/access.log",
	"/var/log/apache2/error.log"
)

content <- sapply(files,readFile)
names(content) <- c("daemon","db","history","maveacc","maveerr","apacc","aperr")

respondTemplateHTML("/var/www/html/mavevis/httpdocs/debugTmpl.html",content)

# #Read log files and concatenate them
# output <- paste(do.call(c,lapply(files,function(f) c(
# 	"########################",
# 	f,"\n",
# 	readFile(f),"\n"
# ))),collapse="\n")

# #Respond to HTTP request
# respondTEXT(output)
