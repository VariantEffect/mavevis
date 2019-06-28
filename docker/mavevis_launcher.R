#!/usr/bin/Rscript

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

#########################################################################
## mavevis_launcher.R
##
## This script allows execution of the mavevis dashboard() function 
## from the command line. 
## 
## Usage: Rscript mavevis_launcher.R scoresetID=<ssid> [ uniprot=<uniprot>
##     pdb=<pdb-ids> mainChain=<chains> WT=<seq> | seqOffset=<num> |
##     synMed=<num> | stopMed=<num> | PngRes=<num> | outFormats={png|pdf} ]
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
uniprotId <- keepNull(getArg("uniprot",default=NULL))
# uniprotId <- keepNull(getArg("uniprot",required=TRUE))

pdbArg <- keepNull(getArg("pdb",default=NULL))
# pdbArg <- keepNull(getArg("pdb",required=TRUE))
if (inherits(pdbArg,"character")) {
	pdbs <- strsplit(pdbArg,",")[[1]]
} else {
	pdbs <- NULL
}

mainChainArg <- keepNull(getArg("mainChain",default=NULL))
# mainChainArg <- keepNull(getArg("mainChain",required=TRUE))
if (inherits(mainChainArg,"character")) {
	mainChains <- strsplit(mainChainArg,",")[[1]]
} else {
	mainChains <- NULL
}

overrideCache <- as.logical(keepNull(getArg("overrideCache",default=FALSE)))
wt.seq <- keepNull(getArg("WT",default=NULL))
seq.offset <- keepNull(getArg("seqOffset",default=0))
if (!is.null(seq.offset)) seq.offset <- as.integer(seq.offset)
syn.med <- keepNull(getArg("synMed",default=NULL))
if (!is.null(syn.med)) syn.med <- as.numeric(syn.med)
stop.med <- keepNull(getArg("stopMed",default=NULL))
if (!is.null(stop.med)) stop.med <- as.numeric(stop.med)
outFormats <- strsplit(keepNull(getArg("outFormats",default="pdf,png,svg")),",")[[1]]
pngRes <- as.numeric(keepNull(getArg("pngRes",default=100)))

cat(
	"Starting job",jobId,"with parameters:\n",
	"ssid =",ssid,"\n",
	"uniprotId =",uniprotId,"\n",
	"pdbs =",pdbArg,"\n",
	"mainChains =",mainChainArg,"\n",
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
# close(con)
