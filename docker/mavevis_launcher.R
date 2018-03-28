#!/usr/bin/Rscript

#########################################################################
## mavevis_launcher.R
##
## This script allows execution of the mavevis dashboard() function 
## from the command line. 
## 
## Usage: Rscript mavevis_launcher.R scoresetID=<ssid> uniprot=<uniprot>
##     pdb=<pdb-ids> mainChain=<chains> [WT=<seq> | seqOffset=<num> |
##     synMed=<num> | stopMed=<num> | PngRes=<num> | outFormats={png|pdf}]
############################################################################

library("mavevis")
library("yogitools")

#get jobID and setup log file
jobId <- getArg("job",required=TRUE)

log.dir <- Sys.getenv("MAVEVIS_CACHE",unset=paste0(Sys.getenv("HOME"),"/.mavecache/"))
con <- file(paste0(log.dir,"progress_",jobId,".log"))
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")


#make sure NULL is not interpreted as string
keepNull <- function(x) if (is.null(x) || x=="NULL") NULL else if (is.na(x)) NA  else x

#Retrieve arguments
ssid <- keepNull(getArg("scoresetID",required=TRUE))
uniprotId <- keepNull(getArg("uniprot",required=TRUE))

pdbArg <- keepNull(getArg("pdb",required=TRUE))
pdbs <- strsplit(pdbArg,",")[[1]]

mainChainArg <- keepNull(getArg("mainChain",required=TRUE))
mainChains <- strsplit(mainChainArg,",")[[1]]

overrideCache <- as.logical(keepNull(getArg("overrideCache",default=FALSE)))
wt.seq <- keepNull(getArg("WT",default=NULL))
seq.offset <- as.integer(keepNull(getArg("seqOffset",default=0)))
syn.med <- as.numeric(keepNull(getArg("synMed",default=NULL)))
stop.med <- as.numeric(keepNull(getArg("stopMed",default=NULL)))
outFormats <- strsplit(keepNull(getArg("outFormats",default="pdf,png")),",")[[1]]
pngRes <- as.numeric(keepNull(getArg("pngRes",default=100)))

cat(
	"Starting job",jobId,"with parameters:\n",
	"ssid =",ssid,"\n",
	"uniprotId =",uniprotId,"\n",
	"pdbs =",pdbs,"\n",
	"mainChains =",mainChains,"\n",
	"wt.seq =",wt.seq,"\n",
	"seq.offset =",seq.offset,"\n",
	"syn.med =",syn.med,"\n",
	"stop.med =",stop.med,"\n",
	"overrideCache =",overrideCache,"\n",
	"outFormats =",outFormats,"\n",
	"pngRes =",pngRes,"\n"
)

#start dashboard function
dashboard(
	ssid=ssid,uniprotId=uniprotId,pdbs=pdbs,mainChains=mainChains,
	wt.seq=wt.seq,seq.offset=seq.offset,syn.med=syn.med,stop.med=stop.med,
	overrideCache=overrideCache,outFormats=outFormats,pngRes=pngRes,outID=jobId
)

cat("\nJob completed successfully!\n")