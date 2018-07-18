#!/usr/bin/Rscript

options(stringsAsFactors=FALSE)

suppressMessages({
	library(cgir)
	library(RJSONIO)
	library(mavevis)
})
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")

#read data from HTTP POST request
postdata <- readPOST()

#check if uniprot acc was supplied
if (!("uniprot" %in% names(postdata))) {
	#otherwise respond with error message
	respond400("No uniprot accession provided!")
	quit(save="no",status=0)
}

#check if uniprot accession was valid
uniprotRegex <- "^[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}$"
if (!grepl(uniprotRegex,postdata$uniprot)) {
	#otherwise respond with error message
	respond400("Invalid uniprot accession provided!")
	quit(save="no",status=0)
}

#check if a range was defined
if (
	all(c("rangeStart","rangeEnd") %in% names(postdata)) && 
	!is.na(as.numeric(postdata$rangeStart)) && 
	!is.na(as.numeric(postdata$rangeEnd))
) {
	filterRange <- c(as.numeric(postdata$rangeStart),as.numeric(postdata$rangeEnd))
} else {
	filterRange <- c(NA,NA)
}

#helper function to turn data.frame into list of lists
lol <- function(df) lapply(1:nrow(df),function(i) as.list(df[i,]))

tryCatch({
	messages <- capture.output({
		results <- find.pdbs(postdata$uniprot,filterRange)
	})
	if (!is.null(results) && nrow(results) > 0) {
		respondJSON(lol(results))
	} else {
		respondJSON(list())
	}
},error=function(e) {
	respond500(e)
})
