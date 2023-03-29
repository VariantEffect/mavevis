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



# source("lib/amasLite.R")
options(stringsAsFactors=FALSE)


#' Find PDB structures for a Uniprot accession
#' 
#' Finds PDB structures that contain the protein indicated by the given Uniprot accession.
#' Checks for a local cache file of previous results. If such a cache exists, the pre-calculated
#' results will be returned, otherwise queries to Uniprot and PDB will be made.
#' 
#' @param acc the Uniprot accession
#' @return a \code{data.frame} with the following columns:
#' \itemize{
#'   \item pdb: the PDB accession of the structure
#'   \item method: the experimental method for this structure, e.g NMR, X-ray or Model
#'   \item resolution: the resolution of this structure, in Angstrom.
#'   \item mainChains: a /-separated list of chain IDs that correspond to the protein
#'      with the given Uniprot accession.
#'   \item start: the first amino acid of the protein represented in the structure
#'   \item end: the last amino acid of the protein represented in the structure
#'   \item partners: a comma-separated list of interaction partners if the structure is 
#'      of a complex. Each item follows the syntax chainID=UniprotID/ProteinName
#' }
#' @export
find.pdbs <- function(acc, filterRange=NA) {
	library("httr")
	set_config(config(ssl_verifypeer = 0L))

	uniprot.base <- "https://rest.uniprot.org/uniprotkb/"
	pdb.header.base <- "https://files.rcsb.org/header/"
	pdb.base <- "https://files.rcsb.org/download/"

	table.file <- getCacheFile(paste0(acc,"_pdbs.csv"))

	if (!file.exists(table.file)) {
		#Get PDB XRefs
		txt.url <- paste0(uniprot.base,acc,".txt")
		htr <- GET(txt.url)
		if (http_status(htr)$category != "Success") {
			stop("Unable to access Uniprot!\n",http_status(htr)$message)
		}
		lines <- strsplit(content(htr,"text",encoding="UTF-8"),"\n")[[1]]
		pdb.refs <- lines[grepl("^DR   PDB; ",lines)]
		if (length(pdb.refs) > 0) {
			pdb.refs <- sub("^DR   PDB; ","",sub("\\.$","",pdb.refs))
			pdb.table <- as.data.frame(do.call(rbind,strsplit(pdb.refs,"; ")))
			colnames(pdb.table) <- c("pdb","method","resolution","chainsAndRange")

			cnr <- strsplit(pdb.table$chainsAndRange,"=")
			pdb.table$mainChains <- sapply(cnr,`[[`,1)
			range <- sapply(cnr,`[[`,2)
			splRange <- strsplit(range,"-")
			pdb.table$start <- as.numeric(sapply(splRange,`[[`,1))
			pdb.table$end <- as.numeric(sapply(splRange,`[[`,2))
			pdb.table$chainsAndRange <- NULL

			cat("Extracting PDB complex partners")
			other.prots <- mapply(function(pdb,self) {
				Sys.sleep(0.5)
				cat(".")
				selves <- strsplit(self,"/")[[1]]
				header.url <- paste0(pdb.base,pdb,".pdb")
				htr <- GET(header.url)
				if (http_status(htr)$category != "Success") {
					#This can actually happen when the structure is excessively large!
					warning("Unable to access PDB for ",pdb,"!\n",http_status(htr)$message)
					return("Error")
				}
				lines <- strsplit(content(htr, "text",encoding="UTF-8"),"\n")[[1]]
				xref.lines <- lines[grepl("^DBREF.+ UNP ",lines)]
				chains <- substr(xref.lines,13,13)
				xrefs <- trimws(substr(xref.lines,34,42))
				protnames <- trimws(substr(xref.lines,43,55))
				if (all(chains %in% selves)) {
					return(NA)
				} else {
					self.i <- which(chains %in% selves)
					return(unique(paste0(chains[-self.i],"=",xrefs[-self.i],"/",protnames[-self.i])))
				}
			},pdb=pdb.table$pdb,self=pdb.table$mainChains)
			cat("done!\n")

			pdb.table$partners <- sapply(other.prots,function(xs) if (length(xs)==1 && (is.na(xs) || xs == "Error")) NA else paste(xs,collapse=","))
			if (any(sapply(other.prots,`[[`,1)== "Error",na.rm=TRUE)) {
				pdb.table <- pdb.table[-which(sapply(other.prots,`[[`,1)== "Error"),]
			}

		} else {
			pdb.table <- NULL
		}
		write.table(pdb.table,table.file,sep=",",row.names=FALSE)
	} else {
		cat("Retrieving data from cache...\n")
		pdb.table <- read.csv(table.file)
	}

	# if a range filter is defined, apply it before returning the result.
	# the unfiltered table gets saved above in case other filter requests 
	# are made in the future.
	if (!any(is.na(filterRange))) {
		# filter out any structure starting after the end 
		# or ending before the start
		filter <- which(!(pdb.table$end < filterRange[[1]] | 
			pdb.table$start > filterRange[[2]]))
		pdb.table <- pdb.table[filter,]
	}

	return(pdb.table)
}

#' Find domains for a Uniprot accession
#' 
#' Finds protein domains belonging to the given Uniprot accession.
#' Checks for a local cache file of previous results. If such a cache exists, the pre-calculated
#' results will be returned, otherwise queries to Uniprot and PDB will be made.
#' 
#' @param acc the Uniprot accession
#' @return a \code{data.frame} with the following columns:
#' \itemize{
#'   \item type: the type of domain or feature (SIGNAL, DOMAIN, or REPEAT)
#'   \item method: the experimental method for this structure, e.g NMR, X-ray or Model
#'   \item name: the name of the domain or feature
#'   \item start: the start amino acid position
#'   \item end: the end amino acid position
#' }
#' @export
fetch.domains.uniprot <- function(acc) {


	cacheFile <- getCacheFile(paste0(acc,"_UPdomains.csv"))

	if (!file.exists(cacheFile)) {

	  library("httr")
	  set_config(config(ssl_verifypeer = 0L))

	  uniprot.base <- "https://rest.uniprot.org/uniprotkb/"

	  htr <- GET(paste0(uniprot.base,acc,".txt"))
	  if (http_status(htr)$category != "Success") {
	    stop("Unable to access Uniprot!\n",http_status(htr)$message)
	  }

	  lines <- strsplit(content(htr,"text",encoding="UTF-8"),"\n")[[1]]

	  domain.lines <- grep("^FT\\s+(SIGNAL|DOMAIN|REPEAT)",lines)
	  if (length(domain.lines) == 0) {
	    out <- data.frame()
	  } else {
		  domain.data <- yogitools::extract.groups(lines[domain.lines],"(SIGNAL|DOMAIN|REPEAT)\\s+(\\d+)..(\\d+)")
		  domain.names <- yogitools::extract.groups(lines[domain.lines+1],"/note=\\\"(.*)\\\"")
		  out <- data.frame(
		    type=domain.data[,1],
		    name=domain.names,
		    start=as.integer(domain.data[,2]),
		    end=as.integer(domain.data[,3])
		  )
		}
	  write.table(out,cacheFile,sep=",",row.names=FALSE)

  } else {

		cat("Retrieving data from cache...\n")
		out <- data.frame(
	    type=character(0),
	    name=character(0),
	    start=integer(0),
	    end=integer(0)
		)
		tryCatch({
			out <- read.csv(cacheFile)
		},error=function(e){
			cat("(Cache file empty!)\n")
		})
  }
  return(out)
}

#' Find Pfam domains for a Uniprot accession
#' 
#' Finds Pfam domains belonging to the given Uniprot accession.
#' Checks for a local cache file of previous results. If such a cache exists, the pre-calculated
#' results will be returned, otherwise queries to Uniprot and PDB will be made.
#' 
#' @param acc the Uniprot accession
#' @return a \code{data.frame} with the following columns:
#' \itemize{
#'   \item type: the type of domain or feature (SIGNAL, DOMAIN, or REPEAT)
#'   \item method: the experimental method for this structure, e.g NMR, X-ray or Model
#'   \item name: the name of the domain or feature
#'   \item start: the start amino acid position
#'   \item end: the end amino acid position
#' }
#' @export
fetch.domains.pfam <- function(acc) {

	cacheFile <- getCacheFile(paste0(acc,"_PFdomains.csv"))

	if (!file.exists(cacheFile)) {

	  library("httr")
	  library("xml2")
	  library("yogitools")
	  set_config(config(ssl_verifypeer = 0L))

	  pfam.base <- "https://pfam.xfam.org/protein/"

	  htr <- GET(paste0(pfam.base,acc,"?output=xml"))
	  if (http_status(htr)$category != "Success") {
	    stop("Unable to access PFAM!\n",http_status(htr)$message)
	  }
	  txt <- content(htr,"text",encoding="UTF-8")
	  doc <- as_list(read_xml(txt))
	  domains <- yogitools::as.df(lapply(doc$pfam$entry$matches,function(domain) {
	    list(
	      type=attr(domain,"type"),
	      name=attr(domain,"id"),
	      start=as.integer(attr(domain$location,"start")),
	      end=as.integer(attr(domain$location,"end"))
	    )
	  }))
	  domains <- domains[order(domains$start),]
	  write.table(domains,cacheFile,sep=",",row.names=FALSE)

  } else {

		cat("Retrieving data from cache...\n")
		domains <- read.csv(cacheFile)

  }
  return(domains)

}

#' Retrieve Uniprot Sequence
#' 
#' Retrieves the amino acid sequence for the protein indicated by a Uniprot accession.
#' @param uniprot.acc the accession
#' @return the amino acid sequence
#' @export
getUniprotSeq <- function(uniprot.acc) {

	url <- paste0("https://rest.uniprot.org/uniprotkb/",uniprot.acc,".fasta")

	readFASTA <- function(file) {
		lines <- scan(file,what="character",sep="\n")
		if (length(lines) < 2) {
			stop("Invalid FASTA format in ",file)
		}
		if (substr(lines[[1]],1,1) != ">") {
			stop("Missing FASTA header in ",file)
		}
		paste(lines[-1],collapse="")
	}

	prot <- readFASTA(url)

	# sapply(1:nchar(prot),function(i)substr(prot,i,i))
	prot

}

#' Select smallest informative subset of PDB structures
#' 
#' Given the results of \code{find.pdbs()}, this function finds the smallest informative
#' subset among them. That is, the smallest set of PDB structures, that still represent all
#' available interaction partners.
#' @param pdb.table The result of \code{find.pdbs()}
#' @return a vector with the IDs of the selected PDB structures.
pdb.informative <- function(pdb.table) {

	protsPerStruc <- lapply(strsplit(pdb.table$partners,","),function(x) unique(gsub("^\\w{1}=|/.+","",x)))
	names(protsPerStruc) <- pdb.table$pdb
	#reverse, so later structures are listed first
	protsPerStruc <- rev(protsPerStruc)
	unique.prots <- unique(do.call(c,protsPerStruc))
	numPerStruc <- sapply(protsPerStruc,function(x) if (length(x)==1 && is.na(x)) 0 else length(x))
	#use greedy approach to find minimized subset
	selected <- character()
	protsLeft <- unique.prots
	while(length(protsLeft) > 1) {
		curr <- names(which.max(numPerStruc))
		selected <- c(selected,curr)
		toRemove <- protsPerStruc[[curr]]
		protsPerStruc <- lapply(protsPerStruc,setdiff,toRemove)
		protsLeft <- setdiff(protsLeft,toRemove)
		numPerStruc <- sapply(protsPerStruc,function(x) if (length(x)==1 && is.na(x)) 0 else length(x))
	}

	return(selected)

}

#' Calculate position-wise conservation from a Uniprot accession
#' 
#' Retrieves the 90% most similar proteins from Uniprot, runs ClustalO to 
#' calculate a multiple sequence alignment and uses the AMAS algorithm to 
#' derive the position-wise sequence conservation. If the given accession
#' has been used as an input before, a cached sequence alignment will be
#' used instead.
#' 
#' @param acc the Uniprot accession
#' @return a numerical vector with the position-wise conservation.
#' @export
calc.conservation <- function(acc,overrideCache=FALSE) {
	library("httr")
	library("RJSONIO")
	httr::set_config(config(ssl_verifypeer = 0L))

	# uniprot.base <- "https://www.uniprot.org/uniprot/"
	# uniref90.base <- "https://www.uniprot.org/uniref/UniRef90_"
	uniref90.base <- "https://rest.uniprot.org/uniref/"
	# uniparc.base <- "https://www.uniprot.org/uniparc/"
	batch.base <- "https://rest.uniprot.org/idmapping/"

	#Get Orthologs
	alignment.file <- getCacheFile(paste0(acc,"_alignment.fasta"))
	if (!file.exists(alignment.file) || overrideCache) {

		cat("Querying UniRef...")

		#Find the appropriate UniRef cluster entry
		htr <- httr::POST(
			paste0(batch.base,"run"), 
			body=list(
				ids=acc, from="UniProtKB_AC-ID", to="UniRef90"
			),
			encode="form",
			#Workaround: Uniprot can't deal with more complex accept headers
			accept("*/*")
		)
		if (http_status(htr)$category != "Success") {
			stop("Unable to access UniprotKB!\n",http_status(htr)$message)
		}
		jobId <- RJSONIO::fromJSON(content(htr,"text",encoding="UTF-8"))

		#Poll the job status
		jobStatus <- c(jobStatus="UNKNOWN")
		while (!("results" %in% names(jobStatus) || jobStatus %in% c("FINISHED"))) {
			Sys.sleep(.2)
			htr <- httr::GET(
				paste0(batch.base,"status/",jobId),accept("*/*")
			)
			if (http_status(htr)$category != "Success") {
				stop("Unable to access UniprotKB!\n",http_status(htr)$message)
			}
			jobStatus <- RJSONIO::fromJSON(content(htr,"text",encoding="UTF-8"))
		}

		#retrieve result
		Sys.sleep(.2)
		if ("results" %in% names(jobStatus)) {
			results <- jobStatus
		} else {
			htr <- httr::GET(
				paste0(batch.base,"uniref/results/",jobId),accept("*/*")
			)
			if (http_status(htr)$category != "Success") {
				stop("Unable to access UniprotKB!\n",http_status(htr)$message)
			}
			results <- fromJSON(content(htr,"text",encoding="UTF-8"))
		} 
		unirefAccs <- sapply(results$results,function(x)x$to$id)

		# unirefAccs <- strsplit(content(htr,"text",encoding="UTF-8"),"\n")[[1]]
		if (length(unirefAccs) == 0){
			stop("No Uniref entry exists for ",acc)
		} else if (length(unirefAccs) > 1) {
			warning("Multiple Uniref entries for ",acc)
			unirefAccs <- unirefAccs[[1]]
		} else {
			cat("success\n")
		}

		#Resolve the member IDs for the Uniref cluster
		ref.url <- paste0(uniref90.base,unirefAccs,"/members?format=list&size=500")
		htr <- GET(ref.url)
		if (http_status(htr)$category != "Success") {
			stop("Unable to access Uniref!\n",http_status(htr)$message)
		}
		xrefs <- strsplit(content(htr,"text",encoding="UTF-8"),"\n")[[1]]
		# #double-check that this is the correct cluster
		# this doesn't work anymore because the IDs are not necessarily accessions :P
		# if (!any(grepl(acc,xrefs))) {
		# 	stop("UniRef Cluster does not contain query protein!")
		# }

		cat("Retrieving sequences...")
		#use batch-service to download multi-fasta file for the set of sequences

		htr <- httr::POST(
			paste0(batch.base,"run"), 
			body=list(
				ids=paste(xrefs,collapse=","), from="UniProtKB_AC-ID", to="UniProtKB"
			),
			encode="form", accept("*/*")
		)
		if (http_status(htr)$category != "Success") {
			stop("Unable to access UniprotKB!\n",http_status(htr)$message)
		}
		jobId <- RJSONIO::fromJSON(content(htr,"text",encoding="UTF-8"))

		#Poll the job status
		jobStatus <- c(jobStatus="UNKNOWN")
		while (!("results" %in% names(jobStatus) || jobStatus %in% c("FINISHED"))) {
			Sys.sleep(.2)
			htr <- httr::GET(
				paste0(batch.base,"status/",jobId),accept("*/*")
			)
			if (http_status(htr)$category != "Success") {
				stop("Unable to access UniprotKB!\n",http_status(htr)$message)
			}
			jobStatus <- RJSONIO::fromJSON(content(htr,"text",encoding="UTF-8"))
		}

		#retrieve result
		Sys.sleep(.2)
		htr <- httr::GET(
			paste0(batch.base,"uniprotkb/results/",jobId,"?format=fasta"),accept("*/*")
		)
		if (http_status(htr)$category != "Success") {
			stop("Unable to access UniprotKB!\n",http_status(htr)$message)
		}
		multifasta <- content(htr,"text",encoding="UTF-8")
		# results <- fromJSON(content(htr,"text",encoding="UTF-8"))
		
		cat("success\n")

		#!! make sure query protein is first in the list !!
		#!! this is not guaranteed to be the case, as they are returned in random order !!
		fastacon <- textConnection(multifasta,open="r")
		fastalines <- scan(fastacon,what="character",sep="\n")
		close(fastacon)
		header.idx <- grep("^>",fastalines)
		headerlines <- fastalines[header.idx]
		query.idx <- grep(acc,headerlines)
		if (length(query.idx) == 0) {
			stop("Uniref cluster FASTA file does not contain query sequence!")
		}
		if (query.idx[[1]] != 1) {
			query.start <- header.idx[[query.idx]]
			query.end <- c(header.idx,length(fastalines)+1)[[query.idx+1]]-1
			multifasta <- c(fastalines[query.start:query.end],fastalines[-(query.start:query.end)])
		}

		#export to fasta file
		fasta.file <- getCacheFile(paste0(acc,"_orthologs.fasta"))
		con <- file(fasta.file,open="w")
		writeLines(multifasta,con)
		close(con)

		#Run ClustalOmega on the sequences
		cat("Aligning sequences...")
		retVal <- system(paste(
			"clustalo -i",fasta.file,"--force -o",alignment.file
		),wait=TRUE)
		if (retVal != 0) stop("ClustalOmega failed!")
		cat("success\n")
	} else cat("Using archived alignment.\n")

	amas <- new.amasLite()
	conservation <- amas$run(alignment.file)

	return(conservation)
}
