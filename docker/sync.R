#!/usr/bin/Rscript

options(stringsAsFactors=FALSE)

library(rapimave)
library(hgvsParseR)
library(yogitools)
library(mavevis)


baseURL <- "https://www.mavedb.org/api/"

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

tryCatch({

	#Open API connection
	rmave <- new.rapimave(baseURL)

	#Query list of scoresets
	scoresets <- rmave$getAllScoreSets()

	#Iterate overscoresets and build index
	index <- to.df(do.call(rbind,lapply(scoresets,function(scoreset) {

		#If it's an outdated scoreset, skip it!
		if (!is.null(scoreset$getNextVersion())) {
			return(NULL)
		}

		urn <- scoreset$getURN()
		name <- scoreset$getTitle()

		#No need to process if it's already known
		# if (!is.null(index) && urn %in% index$urn) {
		# 	return(NULL)
		# }

		target <- scoreset$getTarget()
		tname <- target$getName()
		wtseq <- target$getSequence()
		uniprot <- target$getXrefUniprot()

		#If the title already contains the target name, there's no need to repeat it
		if (grepl(tname,name)) {
			value <- paste0(urn,": ",name)
			label <- paste0(name)
		} else {
			value <- paste0(urn,": ",tname," - ",name)
			label <- paste0(tname," - ",name)
		}

		#Check if scores are already cached
		scoreCacheFile <- paste0(cache.dir,urn,".csv")
		if (!file.exists(scoreCacheFile)) {
			#If not, download scores and write to cache location
			scoreTable <- rmave$getScores(urn)
			write.table(scoreTable,scoreCacheFile,sep=",",row.names=FALSE)
		} 

		#Check if variant descriptors have already been cached
		mutCacheFile <- paste0(cache.dir,urn,"_muts.csv")
		if (!file.exists(mutCacheFile)) {
			#if not, do so
			if (!exists("scoreTable")) {
				scoreTable <- read.csv(scoreCacheFile)
			}
			if (!all(is.na(scoreTable$hgvs_pro))) {
				varInfo <- parseHGVS(scoreTable$hgvs_pro,aacode=1)
				write.table(varInfo,mutCacheFile,sep=",",row.names=FALSE)
			} else {
				logger(paste("WARNING: Scoreset",urn,"has no protein-level variant descriptors."))
				return(NULL)
			}
		} else {
			varInfo <- read.csv(mutCacheFile)
		}


		#Parse score file to check for presence of syn/stop
		
		#Get off set or calculate if necessary
		if (!is.null(uniprot)) {
			uniprotId <- uniprot$getID()
			offset <- uniprot$getOffset()
			if (is.null(offset)) {
				offset <- calcOffset(uniprot$getID(),wtseq)
			}
		} else {
			offset <- NA
			uniprotId <- NA
		}
		
		#determine whether stop and synonymous variants are present
		if ("multiPart" %in% colnames(varInfo)) {
			hasStop <- any(varInfo$variant %in% c("Ter","*") & is.na(varInfo$multiPart))
			hasSyn <- any(varInfo$type == "synonymous" & is.na(varInfo$multiPart))
		} else {
			hasStop <- any(varInfo$variant %in% c("Ter","*"))
			hasSyn <- any(varInfo$type == "synonymous")
		}


		#add scoreset information to index
		list(
			value=value,label=label,urn=urn,target=tname,
			uniprot=uniprotId,
			syn=if (hasSyn) "auto" else "manual",
			stop=if (hasStop) "auto" else "manual",
			offset=offset, wt=wtseq
		)

	})))

	indexFile <- paste0(cache.dir,"searchIndex.csv")
	write.table(index,indexFile,sep=",",row.names=FALSE)

	logger("Index successfully updated.")

	logger("Starting caching cycle.")

	#pre-cache alignments, PDB files, and structure tracks.
	invisible(lapply(index$uniprot,function(acc) {

		#skip unknown uniprot entries
		if (is.na(acc)) {
			return(NULL)
		}

		#check if pre-calculated alignment exists. If not, create it.
		alignment.file <- getCacheFile(paste0(acc,"_alignment.fasta"))
		if (!file.exists(alignment.file)) {
			logger(paste("Caching alignment for",acc))
			tryCatch({
				calc.conservation(acc)
			},error=function(e) {
				logger(paste(
					"ERROR: Conservation calculation failed for",
					acc,"\n",e
				))
			})
			
		}

		#check if pre-calculated pdb table exists. If not, create it.
		pdb.table.file <- getCacheFile(paste0(acc,"_pdbs.csv"))
		if (!file.exists(pdb.table.file)) {

			logger(paste("Caching structures for",acc))
			pdb.table <- find.pdbs(acc)

			#iterate over associated pdb structures
			apply(pdb.table,1,function(pdb.row) {
				pdbacc <- pdb.row[["pdb"]]
				mainChains <- strsplit(pdb.row[["mainChains"]],"/")[[1]]
				#iterate over possible main chains
				lapply(mainChains,function(mc) {
					#check if pre-calculated structure data exists. If not, create it.
					struc.cache.file <- getCacheFile(paste0(pdbacc,":",mc,"_features.csv"))
					if (!file.exists(struc.cache.file)) {
						logger(paste("Caching features for",acc,":",pdbacc,"-",mc))
						tryCatch({
							calc.strucfeats(pdbacc,mc)
						},
						error=function(e) {
							logger(paste(
								"ERROR: Features calculation failed for",
								acc,":",pdbacc,"-",mc,"\n",e
							))
						})
					}
				})
			})
			
		}

	})) 


	logger("Synchronization complete.")

},error=function(e) {
	logger("ERROR: Synchronization failed!")
	logger(e)
})
