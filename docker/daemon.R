#!/usr/bin/Rscript

library(RJSONIO)
options(stringsAsFactors=FALSE)

#Caching directory
cache.dir <- Sys.getenv("MAVEVIS_CACHE",unset="/var/www/mavevis/cache/")
if (!file.exists(cache.dir)) {
	stop("Cache directory does not exist!")
}

#a file that stores the job status table
statusDB.file <- paste0(cache.dir,"statusDB.csv")
#a file that records past job history once they are retired
history.file <- paste0(cache.dir,"statusHistory.csv")
#a log file to record events
logfile <- paste0(cache.dir,"daemon.log")

#the location of the launcher app
launcher.loc <- "/var/www/html/mavevis/mavevis_launcher.R"
if (!file.exists(launcher.loc)) {
	stop("Launcher does not exist!")
}

#setup the status table as a global variable
status <- data.frame()
#load a previous existing status table
if (file.exists(statusDB.file)) {
	status <- read.csv(statusDB.file)
}

#function to make log entries in the log file
logger <- function(msg) {
	con <- file(logfile,open="a")
	writeLines(paste(Sys.time(),msg),con)
	close(con)
}

#persist the current job status table to the filesystem
exportStatus <- function() {
	write.table(status,statusDB.file,sep=",",row.names=FALSE)
}

#handler for any potential errors
handleError <- function(ex) {
	#eventually this should send an email via system("mail ...")
	#but this is apparently very difficult to do on a google instance
	#as port 25 is blocked to prevent spamming.
	#For now we just make a log entry
	logger(as.character(ex))
}

#starts the daemon. 
#infinitely checks for new ids and processes them every two seconds
#also retires jobs older than one week
daemon <- function() {
	#TODO: Try launching apache from here directly and monitor during patrol().
	#catch any errors remaining errors and handle them
	tryCatch(
		{
			logger("INFO: Daemon started.")
			#infinite loop 
			while(TRUE) {
				#patrol the directory for new jobs
				patrol()
				#sleep for two seconds until next patrol
				Sys.sleep(2)
			}
		},
		error=handleError
	)
}

#check the directory for jobs and deal with them
patrol <- function() {
	#catch any errors and handle them
	tryCatch(
		{
			#list all job input files
			files <- list.files(cache.dir,pattern="^input_")
			#for each input file
			invisible(lapply(files, function(f) {
				#full path to input file
				path <- paste0(cache.dir,f)
				#extract job ID
				id <- substr(f,7,42)
				#calculate age of the job
				age <- difftime(Sys.time(), file.info(path)[,"ctime"], units = "weeks")
				#find the corresponding row in the status table
				i <- which(status$id==id)

				if (!(id %in% status$id)) {
					#if it's new, send it to processing
					logger(paste("INFO: Processing job",id))
					process(id)
				} else if (age >= 1) {
					#if it's older than 1 week, retire it
					logger(paste("INFO: Retiring old job",id))
					retire(id)
				} else if (file.exists(paste0(cache.dir,"progress_",id,".log")) 
						&& status[i,"status"] == "Accepted") {
					#if it has been accepted and a log file exists, mark it as running
					status[i,"status"] <<- "Running"
					exportStatus()
				} else if (file.exists(paste0(cache.dir,"result_",id,".png")) 
						&& status[i,"status"] == "Running") {
					#if it's marked running and a result exists, mark it as complete
					status[i,"status"] <<- "Complete"
					exportStatus()
				}
				#TODO: may want to check for errors in the log file to update status
				return(NULL)
			}))
		},
		error=handleError
	)
	
}

#start processing a given id
process <- function(id) {
	#check if an input file exists
	input.file <- paste0(cache.dir,"input_",id,".json",sep="")
	if (file.exists(input.file)) {
		#read the input file
		input.json <- paste(scan(input.file,what="character",sep="\n"),collapse="\n")
		input <- fromJSON(input.json)
		#start a launcher job
		system(
			paste("Rscript", launcher.loc, inputToArgs(input)),
			wait=FALSE
		)
		#set status to "accepted"
		status <<- rbind(status,data.frame(id=id,time=Sys.time(),status="Accepted"))
	} else {
		status <<- rbind(status,data.frame(id=id,time=Sys.time(),status="Error"))
		logger(paste("ERROR: No input file for",id))
	}
	exportStatus()
}

#helper function to turn list data into command line arguments
inputToArgs <- function(input) {
	#turn entries with multiple values into comma-separated string
	temp <- lapply(input,function(x) if (length(x) > 1) paste(x,collapse=",") else x)
	#construct argument string
	paste(sapply(names(input),function(n)paste0(n,"=",temp[[n]])),collapse=" ")
}

#delete associated data and move the status entry to the history file.
retire <- function(id) {
	files <- list.files(cache.dir,pattern=id,full.names=TRUE)
	success <- sapply(files,file.remove)
	if (!all(success)) {
		logger(paste("WARNING: Failed to delete all files for",id))
	} 

	i <- which(status$id==id)

	oldEntry <- status[i,]
	con <- file(history.file,open="a")
	write.table(oldEntry,con,sep=",",row.names=FALSE,col.names=FALSE)
	close(con)

	status <<- status[-i,,drop=FALSE]
	exportStatus()
}

#start the daemon
daemon()

