#!/usr/bin/Rscript

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")

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
		con <- file(filename,open="r")
		lines <- readLines(con)
		close(con)
		return(paste(lines,collapse="\n"))
	} else {
		return("This file does not exist.")
	}
}

#List of logfiles to read
files <- c(
	paste0(cache.dir,"daemon.log"),
	paste0(cache.dir,"statusDB.csv"),
	paste0(cache.dir,"statusHistory.csv"),
	"/var/www/mavevis/logs/access.log",
	"/var/www/mavevis/logs/error.log",
	"/var/log/apache2/access.log",
	"/var/log/apache2/error.log"
)

#Read log files and concatenate them
output <- paste(do.call(c,lapply(files,function(f) c(
	"########################",
	f,"\n",
	readFile(f),"\n"
))),collapse="\n")

#Respond to HTTP request
respondTEXT(output)
