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

options(stringsAsFactors=FALSE)

suppressMessages({
	library(cgir)
	library(RJSONIO)
	library(mavevis)
})
log.dir <- Sys.getenv("MAVEVIS_LOGS",unset="/var/www/html/mavevis/logs/")
setMessageSink(paste0(log.dir,"exec.log"))

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/html/mavevis/cache/")

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
