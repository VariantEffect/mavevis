
# source("lib/trackdrawer.R")
# source("lib/genophenogram.R")
# source("lib/calcStrucFeats.R")
# source("lib/orthologs.R")


#' Find cache file location by name
#' 
#' Finds the location for a cache file. The file does not necessary need to exist yet,
#' as this function is meant to be used determine to a location for both storage and retrieval.
#' 
#' Depending on the execution context, the storage location may differ. The cache location can 
#' be controlled with the environment variable \code{$MAVECACHE}. This will be made use of within
#' the mavevis server docker container. If the variable is not set, a directory ".mavecache/"
#' will be created in the user's home directory to be used as the storage location.
#' 
#' @param name the name of the file, e.g. "P12456_alignment.fasta"
#' @return the full path to the file
#' @examples
#' file <- getCacheFile("P12345_alignment.fasta")
#' 
getCacheFile <- function(name) {
	cache.loc <- Sys.getenv("MAVECACHE",unset=NA)
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
#' parameter and can be either on an X11 device or in PDF or PNG format (or any 
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
#' @param outFormats a vector containing any of the following strings: x11, pdf, png .
#'    Using two or all three at once is allowed and will result in multiple output files.
#' @param pngRes Resolution of PNG output in DPI. Defaults to 100.
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' dashboard(
#'	 ssid="SCS000001A.1", uniprotId="P46937",
#'	 pdbs=c("2LAY","2LTW"), mainChains=c("A","A"),
#'   wt.seq="GACGTTCCACTGCCGGCTGGTTGGGAAATGGCTAAAACTAGTTCTGGTCAGCGTTACTTCCTGAACCACATCGACCAGACCACCACGTGGCAGGACCCGCGT",
#'   seqOffset=170, syn.med=0
#' )
#' }
dashboard <- function(ssid,uniprotId,pdbs,mainChains,
		wt.seq=NULL,seq.offset=0,syn.med=NULL,stop.med=NULL,
		overrideCache=FALSE,outFormats=c("pdf","png"),pngRes=100) {
	
	library("rapimave")
	library("hgvsParseR")
	library("yogitools")
	library("hash")

	options(stringsAsFactors=FALSE)

	##########################
	# Check parameter validity

	# if (!grepl("^SCS\\d{6}\\w{1}\\.\\d+$",ssid)) {
	# 	stop("Error: \'",ssid,"\' is not a valid scoreset ID.")
	# }

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

	#Check if WT seq is DNA or protein
	if (!is.null(wt.seq)) {
		if (grepl("^[ACGT]+$",wt.seq)) {
			#set flag to translate to protein later
			dnaTranslate <- TRUE
		} else if (grepl("^[ACDEFGHIKLMNPQRSTVWY]+$",wt.seq)) {#it's already protein
			aa.seq <- wt.seq
			dnaTranslate <- FALSE
		} else {
			stop("The supplied WT sequence is neither valid DNA nor Protein.")
		}
	} else {
		dnaTranslate <- FALSE
		cat("No WT sequence provided. Obtaining Uniprot sequence.")
		aa.seq <- getUniprotSeq(uniprotId)
	}

	# syn.med <- as.numeric(getArg("synMed",default=NULL))
	if (length(syn.med > 0) && is.na(syn.med)) {
		stop("Parameter 'synMed' must be a numerical value!")
	}

	# stop.med <- as.numeric(getArg("stopMed",default=NULL))
	if (length(stop.med > 0) && is.na(stop.med)) {
		stop("Parameter 'stopMed' must be a numerical value!")
	}

	# outFormats <- strsplit(getArg("outFormats",default="pdf,png"),",")[[1]]
	if (!all(outFormats %in% c("x11","pdf","png"))) {
		stop("Parameter 'outFormats' must be comma-separated list of any of the following: 'x11', 'pdf', or 'png'.")
	}

	# pngRes <- as.numeric(getArg("pngRes",default=100))
	if (is.na(pngRes)) {
		stop("Parameter 'pngRes' must be numeric")
	}
	if (pngRes < 50 || pngRes > 400) {
		error("PNG resolutions below 50 DPI and above 400 DPI are not allowed.")
	}

	# new.trt <- function() {
	# 	library("hash")
	# 	ct <- read.delim("res/codontable.txt",header=FALSE)
	# 	aas <- ct[,2]
	# 	codons <- strsplit(ct[,3],"\\|")
	# 	trtable <- hash()
	# 	for (i in 1:nrow(ct)) {
	# 		for (codon in codons[[i]]){
	# 			trtable[[codon]] <- aas[[i]]
	# 		}
	# 	}
	# 	trtable
	# }
	# trt <- new.trt()

	#Translate if necessary
	if (dnaTranslate) {
		cat("Translating WT sequence to Protein...\n")
		data("trtable")
		cstarts <- seq(1,nchar(wt.seq),3)
		aa.seq <- paste(sapply(cstarts,function(cs) trtable[[substr(wt.seq,cs,cs+2)]]),collapse="")
	}

	#split sequence into individual characters
	wt.aa <- sapply(1:nchar(aa.seq),function(i)substr(aa.seq,i,i))


	cacheFile <- getCacheFile(paste0(ssid,".csv"))
	if (!file.exists(cacheFile) || overrideCache) {
		cat("Querying scoreset from MaveDB...\n")
		#New instance of R-API for MaveDB
		mave <- new.rapimave()
		#retrieve score set
		sset <- mave$getScoreSet(ssid)
		data <- mave$getScores(ssid)
		cat("Caching scoreset locally...\n")
		write.table(data,cacheFile,sep=",",row.names=FALSE)
	} else {
		cat("Retrieving scoreset from local cache...\n")
		data <- read.csv(cacheFile)
	}


	cat("Interpreting variants...\n")

	# Check if variants are described at nucleotide-level, amino acid-level, or both
	if (regexpr("c\\..+ \\(p\\..+\\)$",data$hgvs[[1]]) > 0) {
		splits <- strsplit(data$hgvs," \\(")
		if (all(sapply(splits,length)!=2)) {
			stop("Inconsistent HGVS reporting! Some entries do not specify coding- and protein-level variation!")
		}
		mut.coding <- sapply(splits,`[[`,1)
		mut.prot <- as.vector(sapply(sapply(splits,`[[`,2), function(s) 
			#trim trailing parentheses
			if (substr(s,nchar(s),nchar(s))==")") substr(s,1,nchar(s)-1) else s
		))
		mut.coding <- parseHGVS(mut.coding)
		mut.prot <- parseHGVS(mut.prot,aacode=1)
	# or if they are only reported at one level
	} else if (regexpr("^c\\.\\S+$",data$hgvs[[1]],perl=TRUE) > 0) {
		mut.coding <- parseHGVS(data$hgvs)
		#TODO: infer AA changes
		# mut.prot <- data.frame(type=rep(NA,nrow(data)),pos=rep(NA,nrow(data)),
		# 	ancestral=rep(NA,nrow(data)),variant=rep(NA,nrow(data)))
	} else if (regexpr("^p\\.\\S+$",data$hgvs[[1]],perl=TRUE) > 0) {
		mut.prot <- parseHGVS(data$hgvs,aacode=1)
		mut.coding <- data.frame(pos=rep(NA,nrow(data)),ancestral=rep(NA,nrow(data)),variant=rep(NA,nrow(data)))
	} else {
		stop("HGVS column does not parse!")
	}

	#Index for multi-mutants
	if ("multiPart" %in% colnames(mut.prot)) {
		index.prot <- as.integer(sapply(strsplit(rownames(mut.prot),"\\."),`[[`,1))
		nmut.prot <- table(index.prot)
		syns <- with(mut.prot,unique(index.prot[which(type %in% c("synonymous","invalid","NA"))]))
		nmut.prot[syns] <- 0
	} else {#no multi-mutations should exist in this case
		if (nrow(data) != nrow(mut.prot)) {
			stop("HGVS parse result does not match data! If you see this, report this as a bug.")
		}
		index.prot <- 1:nrow(data)
		nmut.prot <- rep(1,nrow(data))
	}


	cat("Filtering for single mutant variants...\n")
	#Reduce to single mutants
	#TODO: Add option to average over multi-mutants
	sm.data <- data[nmut.prot < 2,]
	sm.mut <- mut.prot[row.names(sm.data),]

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

	cat("Obtainin conservation information...\n")
	cons <- calc.conservation(uniprotId)

	cat("Reading structural features...\n")
	# strucfeats <- read.csv(paste0("strucfeats_",pdb,".csv"))
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

	td <- new.trackdrawer(l=length(wt.aa),nox=TRUE)
	td$add.constrack(cons)
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

	# td$draw()

	cat("Plotting...")

	# wt.aa <- c(wt.aa,rep("*",30))

	img.width <- length(wt.aa) * 0.13 + 3
	img.height <- 4.5 + 0.13 * td$num.tracks()


	for (outFormat in outFormats) {
		cat(".")
		switch(outFormat,
			x11=x11(,width=img.width,height=img.height),
			pdf=pdf(paste0(ssid,".pdf"),width=img.width,height=img.height),
			png=png(paste0(ssid,".png"),width=img.width*pngRes,
				height=img.height*pngRes,res=pngRes)
		)
		genophenogram(wt.aa=wt.aa,pos=sm.mut$start,mut.aa=sm.mut$variant,
			score=sm.data$score,error=sm.data$se,syn.med=syn.med,stop.med=stop.med,
			grayBack=TRUE,img.width=img.width,tracks=td)
		if (outFormat != "x11") invisible(dev.off())
	}

	cat("Done!\n")

	return(invisible(NULL))
}