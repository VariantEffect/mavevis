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
	is <- which(grepl(tolower(input$term),tolower(idx$label)) | idx$urn == input$term)
	if (length(is) == 0) {
		respondJSON(list())
		quit(save="no",status=0)
	}
	idx <- idx[is,]
}

#convert result table to JSON-style list of lists
output <- lapply(1:nrow(idx),function(i) as.list(idx[i,]))

respondJSON(output)
