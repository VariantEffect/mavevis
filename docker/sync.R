#!/usr/bin/Rscript

options(stringsAsFactors=FALSE)

#This requires the new changes that are in the rapimave devel branch!!
library(rapimave)
library(hgvsParseR)
library(yogitools)
library(mavevis)

baseURL <- "http://ec2-13-210-169-246.ap-southeast-2.compute.amazonaws.com/api/"

#Caching directory
# cache.dir <- paste0(tempdir(),"/")
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

#translate nucleotide sequence to amino acid sequence
translate <- function(dna) {
	#load translation table from mavevis package
	data("trtable")
	#define codon start positions
	cstarts <- seq(1,nchar(dna),3)
	#extract codons and translate
	aa.seq <- paste(sapply(cstarts,function(cs) trtable[[substr(dna,cs,cs+2)]]),collapse="")
	return(aa.seq)
}

#function to calculate the --ungapped-- offset from the uniprot sequence
calcOffset <- function(uniprot.acc, maveSeq) {

	#if it's DNA, translate to protein first
	if (grepl("^[ACGT]+$",maveSeq)) {
		maveSeq <- translate(maveSeq)
	}
	
	#use getUniprotSeq function from mavevis package to download sequence from Uniprot
	uniSeq <- getUniprotSeq(uniprot.acc)
	
	error <- "ERROR: WT sequence does not match Uniprot entry"
	
	#if the sequence in MaveDB is longer than the one in Uniprot, it can't be a match
	if (nchar(uniSeq) < nchar(maveSeq)) {
		logger(error)
		return(-1)
	#if they're identical, there is no offset
	} else if (uniSeq == maveSeq) {
		return(0)
	} else {
		#linearly traverse the longer sequence until it matches
		i <- 1
		imax <- nchar(uniSeq)-nchar(maveSeq)
		while (maveSeq != substr(uniSeq,i,i+nchar(maveSeq)-1) && i <= imax+1) {
			i <- i+1
		}
		if (i <= imax) {
			return(i)
		} else {
			logger(error)
			return(-1)
		}
	}
}

logger("Starting new synchronization cycle.")

#Open existing scorest index
indexFile <- paste0(cache.dir,"searchIndex.csv")
if (file.exists(indexFile)) {
	index <- read.csv(indexFile)
} else {
	index <- NULL
}

tryCatch({

	#Open API connection
	rmave <- new.rapimave(baseURL)

	#Query list of scoresets
	scoresets <- rmave$getAllScoreSets()

	#Iterate overscoresets
	invisible(lapply(scoresets,function(scoreset) {

		if (!is.null(scoreset$getNextVersion())) {
			#it's an outdated scoreset!
			return(NULL)
		}

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

		value <- paste0(urn,": ",tname," - ",name)
		label <- paste0(tname," - ",name)

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

		#Cache varInfo
		mutCacheFile <- paste0(cache.dir,urn,"_muts.csv")
		write.table(varInfo,mutCacheFile,sep=",",row.names=FALSE)

		#Get off set or calculate if necessary
		offset <- uniprot$getOffset()
		if (is.null(offset)) {
			offset <- calcOffset(uniprot$getID(),wtseq)
		}
		
		#determine whether stop and synonymous variants are present
		hasStop <- any(varInfo$variant %in% c("Ter","*"))
		hasSyn <- any(varInfo$type == "synonymous")

		#add scoreset information to index
		index <<- rbind(index,data.frame(
			value=value,label=label,urn=urn,target=tname,
			uniprot=uniprot$getID(),
			syn=if (hasSyn) "auto" else "manual",
			stop=if (hasStop) "auto" else "manual",
			offset=offset, wt=wtseq
		))

		logger("...cached and indexed")
	}))

	#TODO: pre-cache alignments, PDB files, and structure tracks.

	logger("Synchronization complete.")

},error=function(e) {
	logger("ERROR: Synchronization failed!")
	logger(e)
})
