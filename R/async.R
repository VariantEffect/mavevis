
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

dashboard.async.run <- function(ssid,uniprotId,pdbs,mainChains,
		wt.seq=NULL,seq.offset=0,syn.med=NULL,stop.med=NULL,
		overrideCache=FALSE,outFormats=c("pdf","png"),pngRes=100) {

	jobID <- makeUUID()

	system("Rscript /setup/mavevis_launcher.R",wait=FALSE)

	return(jobID)
}
