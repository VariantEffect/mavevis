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


#' Find cache file location by name
#' 
#' Finds the location for a cache file. The file does not necessary need to exist yet,
#' as this function is meant to be used determine to a location for both storage and retrieval.
#' 
#' Depending on the execution context, the storage location may differ. The cache location can 
#' be controlled with the environment variable \code{$MAVEVIS_CACHE}. This will be made use of within
#' the mavevis server docker container. If the variable is not set, a directory ".mavecache/"
#' will be created in the user's home directory to be used as the storage location.
#' 
#' @param name the name of the file, e.g. "P12456_alignment.fasta"
#' @return the full path to the file
#' @export
#' @examples
#' file <- getCacheFile("P12345_alignment.fasta")
#' 
getCacheFile <- function(name) {
	cache.loc <- Sys.getenv("MAVEVIS_CACHE",unset=NA)
	if (is.na(cache.loc)) {
		cache.loc <- paste0(Sys.getenv("HOME"),"/.mavecache/")
	}
	if (!file.exists(cache.loc)) {
		dir.create(cache.loc,showWarnings=FALSE,recursive=TRUE)
	}
	paste0(cache.loc,name)
}

#' Draw dashboard for scoreset
#' 
#' Retrieves a scoreset entry from MaveDB and renders a dashboard plot consisting
#' of a genophenogram heatmap, interface burial, solvent accessibility, secondary
#' structure and conservation. The output is controlled by the \code{outFormats}
#' parameter and can be either on an X11 device or in PDF, SVG or PNG format (or any 
#' combination thereof). Structural and conservation information is obtained from
#' UniProtKB and PDB, thus the respective database accessions are required. Multiple
#' PDB files can be used at once.
#' 
#' @param ssid The MaveDB ScoreSet ID of the dataset to be visualized
#' @param uniprotId The UniprotKB accession for the corresponding protein
#' @param pdbs A vector of PDB accessions to be used for the structure tracks
#' @param mainChains the chain identifiers corresponding to the protein in question
#'   for each provided pdb accession.
#' @param wt.seq An (optional) wild-type sequence for the given protein to use instead of the
#'    sequence found in UniprotKB. This can be either a nucleotide or amino acid sequence.
#' @param seq.offset A parameter describing the start position of the map in the protein's
#'    amino acid sequence. For example, if only a domain was scanned.
#' @param syn.med The median value of synonymous variants as an estimate of wild-type
#'    function. This is only necessary of no synonymous variants are present in the dataset.
#' @param stop.med The median value of nonsense variants as an estimate of complete loss
#'    of function. This is only necessary if no nonsense variants are present in the dataset.
#' @param overrideCache defaults to \code{FALSE}. If set to \code{TRUE}, data will always
#'    be re-downloaded from remote locations instead of using a local cache.
#' @param outFormats a vector containing any of the following strings: x11, svg, pdf, png .
#'    Using two or all three at once is allowed and will result in multiple output files.
#' @param pngRes Resolution of PNG output in DPI. Defaults to 100.
#' @param outID a name for the output file to which the plot will be written. Defaults to ssid.
#' @param pixelMap boolean flag, whether to generate a pixel map for a png image. The map will
#'    be written to an Rdata file named according to the outID parameter. Defaults to \code{FALSE}.
#' @return \code{NULL}.
#' @export
#' @examples
#' \dontrun{
#' dashboard(
#'	 ssid="urn:mavedb:00000001-a-1", uniprotId="P63279",
#'	 pdbs="3UIP", mainChains="A",
#'   syn.med=1,stop.med=0
#' )
#' }
dashboard <- function(ssid,uniprotId=NULL,pdbs=NULL,mainChains=NULL,
		wt.seq=NULL,seq.offset=0,syn.med=NULL,stop.med=NULL,
		overrideCache=FALSE,outFormats=c("pdf","png"),pngRes=100,outID=ssid,
		pixelMap=FALSE) {
	
	library("rapimave")
	library("hgvsParseR")
	library("yogitools")
	library("hash")

	options(stringsAsFactors=FALSE)

	##########################
	# Check parameter validity

	for (pdb in pdbs) {
		if (!grepl("^[A-Za-z0-9]{4}$",pdb)) {
			stop("Parameter \'",pdb,"\' is not a valid PDB identifier.")
		}
	}

	if (length(mainChains) != length(pdbs)) {
		stop("Parameters pdb and mainChain must have the same number of elements!")
	}

	for (mainChain in mainChains) {
		if (!grepl("^[A-Z]{1}$",mainChain)) {
			stop("Parameter \'",mainChain,"\' is not a valid chain identifier.")
		}
	}

	if (is.na(overrideCache) || !is.logical(overrideCache)) {
		warning("Parameter 'overrideCache' must be TRUE or FALSE. Defaulting to FALSE.")
		overrideCache <- FALSE
	}

	if (is.na(seq.offset) || !is.numeric(seq.offset)) {
		cat("Invalid offset parameter! Defaulting to 0")
		seq.offset <- 0
	}

	# syn.med <- as.numeric(getArg("synMed",default=NULL))
	if (length(syn.med > 0) && is.na(syn.med)) {
		stop("Parameter 'synMed' must be a numerical value!")
	}

	# stop.med <- as.numeric(getArg("stopMed",default=NULL))
	if (length(stop.med > 0) && is.na(stop.med)) {
		stop("Parameter 'stopMed' must be a numerical value! Found:",stop.med," (",class(stop.med),")")
	}

	# outFormats <- strsplit(getArg("outFormats",default="pdf,png"),",")[[1]]
	if (!all(outFormats %in% c("x11","pdf","png","svg"))) {
		stop("Parameter 'outFormats' must be comma-separated list of any of the following: 'x11', 'pdf', 'svg', or 'png'.")
	}

	# pngRes <- as.numeric(getArg("pngRes",default=100))
	if (is.na(pngRes)) {
		stop("Parameter 'pngRes' must be numeric")
	}
	if (pngRes < 50 || pngRes > 400) {
		stop("PNG resolutions below 50 DPI and above 400 DPI are not allowed.")
	}

	#If no WT sequence is provided, try to obtain one
	if (is.null(wt.seq)) {
		# dnaTranslate <- FALSE
		cat("No WT sequence provided. Obtaining from MaveDB.\n")
		mave <- new.rapimave()
		sset <- mave$getScoreSet(ssid)
		wt.seq <- sset$getTarget()$getSequence()

		if (is.null(wt.seq) || length(wt.seq)==0) {
			if (!is.null(uniprotId)) {
				cat("No WT sequence found on MaveDB. Obtaining Uniprot sequence.\n")
				aa.seq <- getUniprotSeq(uniprotId)
			} else {
				stop("No WT sequence can be found. Unable to proceed.")
			}
		}
	}

	#Check if WT seq is DNA or protein
	if (grepl("^[ACGT]+$",wt.seq)) {
		#set flag to translate to protein later
		# dnaTranslate <- TRUE
		cat("Translating WT sequence to Protein...\n")
		data("trtable")
		cstarts <- seq(1,nchar(wt.seq),3)
		aa.seq <- paste(sapply(cstarts,function(cs) trtable[[substr(wt.seq,cs,cs+2)]]),collapse="")
	} else if (grepl("^[ACDEFGHIKLMNPQRSTVWY]+$",wt.seq)) {#it's already protein
		aa.seq <- wt.seq
		# dnaTranslate <- FALSE
	} else {
		stop("The supplied WT sequence is neither valid DNA nor Protein.")
	}
	
	wt.aa <- toChars(aa.seq)

	###############
	# Load data
	###############

	cacheFile <- getCacheFile(paste0(ssid,".csv"))
	if (!file.exists(cacheFile) || overrideCache) {
		cat("Querying scoreset from MaveDB...\n")
		#retrieve score set
		#New instance of R-API for MaveDB
		mave <- new.rapimave()
		sset <- mave$getScoreSet(ssid)
		data <- mave$getScores(ssid)
		cat("Caching scoreset locally...\n")
		write.table(data,cacheFile,sep=",",row.names=FALSE)
	} else {
		cat("Retrieving scoreset from local cache...\n")
		data <- read.csv(cacheFile)
	}

	####################
	# Load WT sequence #
	####################
	

	mutCacheFile <- getCacheFile(paste0(ssid,"_muts.csv"))
	if (!file.exists(mutCacheFile) || overrideCache) {

		cat("Interpreting variants...\n")

		if (!all(is.na(data$hgvs_pro))) {
			mut.prot <- parseHGVS(data$hgvs_pro,aacode=1)
		} else {
			stop("Dataset does not describe AA-level variation. Not yet implemented!")
		}

		cat("Caching scoreset locally...\n")
		#row names must be saved also, to ensure correct index resolution!
		write.table(mut.prot,mutCacheFile,sep=",",row.names=TRUE)

	} else {#if cache exists for mutations:
		cat("Retrieving parsed variants from local cache...\n")
		mut.prot <- read.csv(mutCacheFile)
	}

	#Index for multi-mutants
	if ("multiPart" %in% colnames(mut.prot)) {
		index.prot <- as.integer(sapply(strsplit(rownames(mut.prot),"\\."),`[[`,1))
		nmut.prot <- table(index.prot)
	} else {#no multi-mutations should exist in this case
		if (nrow(data) != nrow(mut.prot)) {
			stop("HGVS parse result does not match data! If you see this, report this as a bug.")
		}
		index.prot <- 1:nrow(data)
		nmut.prot <- rep(1,nrow(data))
	}

	#Handle inconsistent error column names
	errColIdx <- which(colnames(data) %in% c("se","SE","stderr","sem","SEM","sd","SD","stdev","std","STD"))
	if (length(errColIdx) == 0) {
		errCol <- NULL
		errName <- NULL
	} else {
		errCol <- as.numeric(data[,errColIdx[[1]]])
		errName <- colnames(data)[errColIdx]
	}



	cat("Filtering for single mutant variants...\n")
	#Reduce to single mutants
	#TODO: Add option to average over multi-mutants
	sm.data <- data[nmut.prot < 2,]
	sm.errCol <- errCol[nmut.prot < 2]
	sm.mut <- mut.prot[row.names(sm.data),]

	#Remove "None" or NA entries
	if (inherits(sm.data$score,"character")) {
		sm.data$score <- as.numeric(sm.data$score)
	}
	filter <- which(!is.na(sm.data$score))
	sm.data <- sm.data[filter,]
	sm.mut <- sm.mut[filter,]
	sm.errCol <- sm.errCol[filter]

	######
	# Obtain median values for stop and synonymous variants, if not provided by user
	#
	if (length(syn.med)==0) {
		if (any(sm.mut$type == "synonymous")) {
			syn.med <- median(sm.data$score[which(sm.mut$type == "synonymous")])
		} else {
			stop("Synonymous variant median could not be determined. Please provide a value.")
		}
	} 
	if (length(stop.med)==0) {
		if (any(sm.mut$variant=="*")) {
			stop.med <- median(sm.data$score[which(sm.mut$variant=="*")])
		} else {
			stop("Nonsense variant median could not be determined. Please provide a value.")
		}
	}

	if (!is.null(uniprotId)) {
		cat("Obtaining conservation information...\n")
		cons <- calc.conservation(uniprotId)

		domains <- data.frame()
		try({
			# domains <- fetch.domains.pfam(acc=uniprotId)
			domains <- fetch.domains.uniprot(uniprotId)
			colkey <- c(DOMAIN="goldenrod1",REPEAT="goldenrod2",SIGNAL="goldenrod3")
		})

		td <- new.trackdrawer(l=length(wt.aa),nox=TRUE)
		td$add.constrack(cons)
		if (nrow(domains) > 0) {
			# td$add.domtrack(domains,c(`Pfam-A`="orange"))
			td$add.domtrack(domains,colkey)
		}

		if (!is.null(pdbs)) {
			cat("Obtaining structural features...\n")
			strucfeats <- mapply(calc.strucfeats,pdb=pdbs,main.chain=mainChains,SIMPLIFY=FALSE)

			#truncate features to scanned domain
			frag.l <- max(sm.mut$start,na.rm=TRUE)-min(sm.mut$start,na.rm=TRUE)
			domain.range <- seq.offset:(seq.offset+frag.l)
			strucfeats <- lapply(strucfeats,function(sf)sf[domain.range,])
			cons <- cons[domain.range]

			sscols <- lapply(strucfeats,`[`,,"secstruc")
			fillup.max <- max(sapply(sscols,length))
			sscols <- lapply(sscols,function(xs) c(xs,rep(NA,fillup.max-length(xs))))
			if (length(sscols) > 1) {
				ss.consensus <- apply(do.call(cbind,sscols),1,function(xs) if (!all(is.na(xs))) names(which.max(table(xs))) else NA)
			} else {
				ss.consensus <- sscols[[1]]
			}

			accols <- lapply(strucfeats,`[`,,"all.rel")
			fillup.max <- max(sapply(sscols,length))
			accols <- lapply(accols,function(xs) c(xs,rep(NA,fillup.max-length(xs))))
			acc.consensus <- apply(do.call(cbind,accols),1,median,na.rm=TRUE)

			td$add.ss.track(ss.consensus)
			td$add.track(acc.consensus,"Rel. ASA","steelblue3")
			for (sf in strucfeats) {
				burial.columns <- which(grepl("rel.burial",colnames(sf)))
				if (length(burial.columns) > 0) {
					for (col in burial.columns) {
						prot <- sub("rel.burial.","",colnames(sf)[[col]])
						td$add.track(sf[,col],prot,"orange",maxVal=1)
					}
				}
			}
		} else {
			cat("No PDB references supplied. Skipping structure tracks.\n")
		}
	} else {
		cat("No uniprot ID supplied. Skipping all tracks.\n")
		td <- NULL
	}


	cat("Plotting...")

	# img.width <- length(wt.aa) * 0.13 + 3
	img.width <- length(wt.aa) * 0.13 + 4
	img.height <- 4.5 + 0.13 * if(is.null(td)) 0 else td$num.tracks()

	for (outFormat in outFormats) {
		cat(".")
		switch(outFormat,
			x11=x11(,width=img.width,height=img.height),
			pdf=pdf(getCacheFile(paste0("result_",outID,".pdf")),width=img.width,height=img.height),
			png=png(getCacheFile(paste0("result_",outID,".png")),width=img.width*pngRes,
				height=img.height*pngRes,res=pngRes),
			svg=svg(getCacheFile(paste0("result_",outID,".svg")),width=img.width,height=img.height)
		)
		pxMap <- genophenogram(wt.aa=wt.aa,pos=sm.mut$start,mut.aa=sm.mut$variant,
			score=sm.data$score,error=sm.errCol,syn.med=syn.med,stop.med=stop.med,
			grayBack=TRUE,img.width=img.width,tracks=td,pixelMap=pixelMap)
		if (pixelMap && !is.null(pxMap)) {
			pxMap$errName <- errName
			mapFile <- getCacheFile(paste0("result_",outID,"_pxmap_",outFormat,".Rdata"))
			save(pxMap,file=mapFile)
		}
		if (outFormat != "x11") invisible(dev.off())
	}

	cat("Done!\n")

	return(invisible(NULL))
}
