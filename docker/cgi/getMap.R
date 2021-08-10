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

options(stringsAsFactors=FALSE)

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/html/mavevis/cache/")

staging.dir <- "/var/www/html/mavevis/httpdocs/results/"

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

#check that the map file exists
mapfile <- paste0(cache.dir,"result_",jobID,"_pxmap_png.Rdata")
if (!file.exists(mapfile)) {
	respond404("No image map Rdata file exists for this job!")
	quit(save="no",status=0)
}

#load the pxMap object from the data file
tryCatch({
	load(mapfile)
	mainMap <- pxMap$main
	summaryMap <- pxMap$summary

	errName <- "uncertainty"
	if ("errName" %in% names(pxMap)) {
		errName <- pxMap$errName
	}

	#translate main map into HTML areas
	mainAreas <- paste(do.call(c,lapply(1:nrow(mainMap),function(i) with(mainMap[i,],{
		paste0("<area shape=\"rect\" coords=\"",
			paste(x0,y1,x1,y0,sep=","),
			"\" ","title=\"",
			sprintf(
				"Variant: %s%d%s; score = %.2f; %s = %.2f",
				wt,pos,aa,score,errName,error
			),"\"/>"
		)
	}))),collapse="\n")

	#translate summary map into HTML areas
	summaryAreas <- paste(do.call(c,lapply(1:nrow(summaryMap),function(i) with(summaryMap[i,],{
		paste0("<area shape=\"rect\" coords=\"",
			paste(x0,y1,x1,y0,sep=","),"\" ",
			"title=\"",sprintf("Residue: %s%d; median=%.2f",wt,pos,Median),"\"",
			"/>"
		)
	}))),collapse="\n")

	#assemble and return the full map HTML
	mapContent <- paste0("<map name=\"pxmap\">\n",mainAreas,"\n",summaryAreas,"\n</map>")
	respondTEXT(mapContent)
	quit(save="no",status=0)

},error=function(e) { 

	respond500("Error reading map data!")
	quit(save="no",status=0)

})

