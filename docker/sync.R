#!/usr/bin/Rscript

options(stringsAsFactors=FALSE)

#This requires the new changes that are in the rapimave devel branch!!
library(rapimave)
library(hgvsParseR)
library(yogitools)

baseURL <- "http://ec2-13-210-169-246.ap-southeast-2.compute.amazonaws.com/api/"

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")
if (!file.exists(cache.dir)) {
	stop("Cache directory does not exist!")
}

#a log file to record events
logfile <- paste0(cache.dir,"daemon.log")

#function to make log entries in the log file
logger <- function(msg) {
	con <- file(logfile,open="a")
	writeLines(paste(Sys.time(),msg),con)
	close(con)
}

logger("Starting new synchronization cyle.")

#Open existing scorest index
indexFile <- paste0(cache.dir,"searchIndex.csv")
if (file.exists(indexFile)) {
	index <- read.csv(indexFile)
} else {
	index <- NULL
}

#Open API connection
rmave <- new.rapimave(baseURL)

#Query list of scoresets
scoresets <- rmave$getAllScoreSets()

#Iterate overscoresets
invisible(lapply(scoresets,function(scoreset) {

	urn <- scoreset$getURN()
	name <- scoreset$getTitle()

	#No need to process if it's already known
	if (!is.null(index) && urn %in% index$urn) {
		return(NULL)
	}

	logger(paste("New scoreset found:",urn))

	target <- scoreset$getTarget()
	tname <- target$getName()
	wtseq <- target$getSequence()
	uniprot <- target$getXrefUniprot()

	label <- paste0(urn,": ",tname," - ",name)

	#Download scores and write to cache location
	scoreTable <- rmave$getScores(urn)
	scoreCacheFile <- paste0(cache.dir,urn,".csv")
	write.table(scoreTable,scoreCacheFile,sep=",",row.names=FALSE)

	#Parse score file to check for presence of syn/stop
	if (grepl(" \\(",scoreTable$hgvs[[1]])) {
		hgvsp <- sub("\\)$","",sapply(strsplit(scoreTable$hgvs," \\("),`[[`,2))
	} else {
		hgvsp <- scoreTable$hgvs
	}
	varInfo <- parseHGVS(hgvsp)
	#TODO: pre-cache varInfo
	
	hasStop <- any(varInfo$variant %in% c("Ter","*"))
	hasSyn <- any(varInfo$type == "synonymous")

	#add scoreset information to index
	index <<- rbind(index,data.frame(
		label=label,urn=urn,target=tname,wt=wtseq,
		uniprot=uniprot$getID(),
		syn=if (hasSyn) "auto" else "manual",
		stop=if (hasStop) "auto" else "manual"
	))

	logger("...cached and indexed")
})

#TODO: pre-cache alignments PDB files and structure tracks.

logger("Synchronization complete.")
