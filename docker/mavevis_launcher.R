#!/usr/bin/Rscript

library("mavevis")
library("yogitools")

#get jobID and setup log file
jobId <- getArg("job",required=TRUE)
# logfile <- getCacheFile(paste0(jobId,".msg"))
# con <- file(logfile,open="wt")
# sink(con,type="message")
# on.exit(close(con))

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

#start dashboard function
dashboard(
	ssid=ssid,uniprotId=uniprotId,pdbs=pdbs,mainChains=mainChains,
	wt.seq=wt.seq,seq.offset=seq.offset,syn.med=syn.med,stop.med=stop.med,
	overrideCache=overrideCache,outFormats=outFormats,pngRes=pngRes,outID=jobId
)

