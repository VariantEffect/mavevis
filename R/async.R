
#' Create universally unique ID (UUIDv4)
#' 
#' Creates a universally unique identifier compatible with the UUID v4.0 standard.
#' See \url{https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)}
#' 
#' @return the UUID as a character string
#' @export
makeUUID <- function() {
	baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
	paste(
		substr(baseuuid,1,8),"-",
		substr(baseuuid,9,12),"-","4",
		substr(baseuuid,13,15),"-",
		sample(c("8","9","a","b"),1),
		substr(baseuuid,16,18),"-",
		substr(baseuuid,19,30),
		sep="", collapse=""
	)
}


#' Launch asynchronous dashboard job
#' 
#' Launches a job running the dashboard function in a separate system process
#' in the background. The job is given a unique ID, which is returned by this function.
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
#' @return the job ID
#' @export
dashboard.async.run <- function(ssid,uniprotId,pdbs,mainChains,
		wt.seq=NULL,seq.offset=0,syn.med=NULL,stop.med=NULL,
		overrideCache=FALSE,outFormats=c("pdf","png"),pngRes=100) {

	launcher.loc <- "/setup/mavevis_launcher.R"
	if (!file.exists(launcher.loc)) {
		#FIXME: This is just a dirty hack to get this to work in my testing environment
		# Find a better way to locate the file. system.file() doesn't work, because it's
		# not part of the package installation.
		launcher.loc <- "./docker/mavevis_launcher.R"
	}

	#create a job ID
	jobID <- makeUUID()

	#helper function for skipping optional args
	optionalArg <- function(name,value) {
		if (!is.null(value)) {
			paste0(" ",name,"=",value)
		} else ""
	}

	logfile <- getCacheFile(paste0(jobID,".log"))

	#prep the launch command
	cmd <- paste0("nohup Rscript ",launcher.loc," scoresetID=",ssid,
		" uniprot=",uniprotId," pdb=",paste(pdbs,collapse=","),
		" mainChain=",paste(mainChains,collapse=",")," overrideCache=",overrideCache,
		optionalArg("WT",wt.seq),
		optionalArg("seqOffset",seq.offset),
		optionalArg("synMed",syn.med),
		optionalArg("stopMed",stop.med),
		optionalArg("outFormats",outFormats),
		optionalArg("pngRes",pngRes),
		optionalArg("job",jobID)#,
		#" >",logfile," 2>&1"
	)

	cat(cmd)

	#launch process in background
	system(cmd,wait=FALSE)

	Sys.sleep(10)

	return(jobID)
}


#' Check progress on asynchronous job
#' 
#' Retrieves the standard out log of an asynchronously launched job, if it exists.
#' If the job doesn't exist or hasn't written any log output, the function 
#' returns \code{NULL}.
#' 
#' @param jobID the ID of the job
#' @return the contents of the stdout log for the given job, 
#'    or \code{NULL} if none exists
#' @export
check.async.progress <- function(jobID) {
	logfile <- getCacheFile(paste0(jobID,".log"))
	if (file.exists(logfile)) {
		log <- scan(logfile,what="character",sep="\n")
		return(log)
	} else return(NULL)
}


#' Retrieve result file from asynchronous job
#' 
#' Retrieves the output file from an asynchronous job if it exists.
#' The file is copied to the working directory, so that OpenCPU can expose it
#' on the webservice. 
#' 
#' @param jobID the ID of the job.
#' @param ext the file extension. Defaults to "png".
#' @return \code{TRUE} if successful, otherwise \code{FALSE}.
#' @export
retrieve.async.result.file <- function(jobID,ext="png") {
	cachefile <- getCacheFile(paste0(jobID,".",ext))
	if (file.exists(cachefile)) {
		#copy it to working directory, so that OpenCPU can pick it up
		file.copy(cachefile,"./")
		return(TRUE)
	} else return(FALSE)
}
