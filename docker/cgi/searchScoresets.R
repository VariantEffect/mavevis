#!/usr/bin/Rscript

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
options(stringsAsFactors=FALSE)
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")

#read data from HTTP GET
input <- readGET()

#read search index
tryCatch({
	idxFile <- paste0(cache.dir,"searchIndex.csv")
	idx <- read.csv(idxFile)
},error=function(e) {
	respond500("Search index cannot be read")
	quit(save="no",status=0)
})	

#if a search term was provided, filter the index by it
if (("term" %in% names(input))) {
	is <- which(grepl(tolower(input$term),tolower(idx$label)) | idx$ssid == input$term)
	if (length(is) == 0) {
		respondJSON(list())
		quit(save="no",status=0)
	}
	idx <- idx[is,]
}

#convert result table to JSON-style list of lists
output <- lapply(1:nrow(idx),function(i) as.list(idx[i,]))

respondJSON(output)
