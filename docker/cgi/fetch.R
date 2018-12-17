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

#check if output format was supplied
if (!("format" %in% names(postdata))) {
	#otherwise respond with error message
	respond400("Missing argument!")
	quit(save="no",status=0)
}

if (!("output" %in% names(postdata))) {
	output <- "direct"
} else {
	output <- postdata$output
}

#return the file contents corresponding to the requested output format
switch(
	postdata$format,
	png={
		fname <- paste0("result_",jobID,".png")
		pngFile <- paste0(cache.dir,fname)
		if (file.exists(pngFile)) {
			switch(output,
				url={
					file.copy(pngFile,staging.dir)
					respondTEXT(paste0("results/",fname))
				},
				respondPNG(pngFile,filename=fname,download=TRUE)
			)
		} else {
			respond404("No PNG output exists for this job!")
			quit(save="no",status=0)
		}
	},
	pdf={
		fname <- paste0("result_",jobID,".pdf")
		pdfFile <- paste0(cache.dir,fname)
		if (file.exists(pdfFile)) {
			switch(output,
				url={
					file.copy(pdfFile,staging.dir)
					respondTEXT(paste0("results/",fname))
				},
				respondPDF(pdfFile,filename=fname,download=TRUE)
			)
		} else {
			respond404("No PDF output exists for this job!")
			quit(save="no",status=0)
		}
	},
	svg={
		fname <- paste0("result_",jobID,".svg")
		svgFile <- paste0(cache.dir,fname)
		if (file.exists(svgFile)) {
			switch(output,
				url={
					file.copy(svgFile,staging.dir)
					respondTEXT(paste0("results/",fname))
				},
				respondSVG(svgFile,filename=fname,download=TRUE)
			)
		} else {
			respond404("No SVG output exists for this job!")
			quit(save="no",status=0)
		}
	},
	{#default case: unrecognized argument
		respond400("Unsupported output format!")
		quit(save="no",status=0)
	}
)