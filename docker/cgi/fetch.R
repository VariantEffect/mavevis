#!/usr/bin/Rscript

suppressMessages({
	library(cgir)
	library(RJSONIO)
})
setMessageSink("/var/www/mavevis/logs/exec.log")

#Caching directory
cache.dir <- "/cache/"

#read data from HTTP POST request
postdata <- readPOST()

#check if jobID was supplied
if (!("jobID" %in% names(postdata))) {
	#otherwise respond with error message
	respond400("No jobID provided!")
	quit(save="no",status=0)
}

jobID <- postdata$jobID

#check if output format was supplied
if (!("format" %in% names(postdata))) {
	#otherwise respond with error message
	respond400("Missing argument!")
	quit(save="no",status=0)
}

#return the file contents corresponding to the requested output format
switch(
	postdata$format,
	png={
		pngFile <- paste0(cache.dir,"result_",jobID,".png")
		if (file.exists(pngFile)) {
			respondPNG(pngFile)
		} else {
			respond404("No PNG output exists for this job!")
			quit(save="no",status=0)
		}
	},
	pdf={
		pdfFile <- paste0(cache.dir,"result_",jobID,".pdf")
		if (file.exists(pdfFile)) {
			respondPDF(pdfFile)
		} else {
			respond404("No PDF output exists for this job!")
			quit(save="no",status=0)
		}
	},
	{#default case: unrecognized argument
		respond400("Unsupported output format!")
		quit(save="no",status=0)
	}
)