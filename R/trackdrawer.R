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


#' New Trackdrawer object
#' 
#' This constructor creates a new Trackdrawer object. The object can be used to draw
#' a plot containing amino-acid resolution structural information tracks, such as 
#' conservation, secondary structure, solvent accessibility or burial.
#' 
#' The object has the following methods:
#' \itemize{
#'   \item \code{add.track(values,label,col,minVal=0,maxVal=max(values))} adds a new numerical
#'     track, which will be visualized in heatmap style. \code{values} are the numerical
#'     values for the heatmap. \code{label} is the label that will be shown on the y-axis
#'     for this track. \code{col} is the color that will be used to generate the color
#'     ramp (from white to \code{col}). \code{minVal} is optional and is the minimum value that
#'     will be mapped to the color white (default 0). \code{maxVal} is also optional and
#'     is the maximum value in the scale, which will be mapped to \code{col}.
#'   \item \code{add.constrack(values)} adds a new conservation track, which will be 
#'     visualized as a barplot. \code{values} are the conservation values to use.
#'   \item \code{add.ss.track(values)} adds a new secondary structure track, which 
#'     will be visualized using spriral and arrow symbols. \code{values} are the 
#'     character values representing the secondary structure type for each amino acid.
#'     Permissible values are: "AlphaHelix","H","G","I" for alpha helices, "Strand","E"
#'     for beta strands and "Disorder" for disordered regions. Any other string is 
#'     interpreted as no specific secondary structure.
#'   \item \code{draw()} draws the plot
#'   \item \code{num.tracks()} returns the number of tracks currently added to the plot.
#' }
#' @param l the length of the tracks (i.e. the number of amino acids)
#' @param nox no x-axis. Removes the x-axis and decreases the bottom margin to 0. This
#'   option is for use with the \code{layout()} function, to support adding tracks on top
#'   of an existing graph that uses amino acid postion as the x-axis.
#' 
#' @return an object of type TrackDrawer.
#' @export
#' @examples
#' \dontrun{
#' td <- new.trackdrawer(50)
#' td$add.track(runif(50,0,1),"SASA","blue")
#' td$add.constrack(runif(50,0,11))
#' td$add.ss.track(sample(c("AlphaHelix","Strand",""),50,replace=TRUE))
#' td$draw()
#' }
new.trackdrawer <- function(l,nox=FALSE) {

	applyCol <- function(vals, cols) sapply(vals,function(x) if(is.na(x)) "gray" else cols[round(x*10+1)])

	.tracks <- list()
	.labels <- character()
	.types <- character()

	add.track <- function(values, label, col, minVal=0,maxVal=max(values,na.rm=TRUE)) {
		colRamp <- colorRampPalette(c("white",col))(11)
		normvals <- (values - minVal)/(maxVal-minVal)
		colVals <- applyCol(normvals,colRamp)
		.tracks[[length(.tracks)+1]] <<- colVals
		.labels[[length(.labels)+1]] <<- label
		.types[[length(.types)+1]] <<- "heat"
	}

	add.constrack <- function(values) {
		.tracks[[length(.tracks)+1]] <<- values
		.labels[[length(.labels)+1]] <<- "Cons."
		.types[[length(.types)+1]] <<- "cons"
	}

	draw.helix <- function(xs,y) {
		loop <- cbind(
			x=c(0,.1,.2,.3,.4,.5,.6,.7,.75,.7,.6,.5,.4,.3,.25,.3,.4,.5,.6,.7,.8,.9,1),
			y=c(.1,.11,.13,.15,.17,.19,.24,.32,.5,.75,.87,.9,.87,.75,.5,.32,.24,.19,.17,.15,.13,.11,.1)
		)
		draw.loop <- function(x,y) {
			offset <- cbind(rep(x,nrow(loop)),rep(y,nrow(loop)))
			lines(loop+offset,lwd=1,col="gray30")
		}
		for (x in min(xs):max(xs)) draw.loop(x-.5,y)
	}

	draw.sheet <- function(xs,y) {
		start <- min(xs)
		end <- max(xs)
		tipend <- end-.8
		polygon(
			x=c(start,tipend,tipend,end,tipend,tipend,start),
			y=y+c(.2,.2,0,.5,1,.8,.8),
			lwd=1,col="gray",border="gray30"
		)
	}

	draw.dis <- function(xs,y) {
		start <- min(xs)
		end <- max(xs)
		arrows(start,y+.5,end,y+.5,code=0,lty="dashed",col="gray30")
	}


# Code	Description
# H	Alpha helix
# B	Beta bridge
# E	Strand
# G	Helix-3
# I	Helix-5
# T	Turn
# S	Bend

	add.ss.track <- function(values) {
		start <- -1
		track <- list()
		for (i in 1:length(values)) {
			if (!is.na(values[[i]]) && values[[i]] %in% c("AlphaHelix","H","G","I")) {
				#if this is the start
				if (i==1 || is.na(values[[i-1]]) || !(values[[i-1]] %in% c("AlphaHelix","H","G","I"))) {
					#if previous was strand, then end strand
					if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] %in% c("Strand","E")) {
						track[[length(track)+1]] <- list(f=draw.sheet,xs=c(start,i-1))
					#if previous was disorder, then end disorder
					} else if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] == "Disorder") {
						track[[length(track)+1]] <- list(f=draw.dis,xs=c(start,i-1))
					}
					start <- i
				}
			} else if (!is.na(values[[i]]) && values[[i]] %in% c("Strand","E")) {
				#if this is the start
				if (i==1 || is.na(values[[i-1]]) || !(values[[i-1]] %in% c("Strand","E"))) {
					#if previous was helix, then end strand
					if (i > 1 && !is.na(values[[i-1]]) && !is.na(values[[i-1]]) && values[[i-1]] %in% c("AlphaHelix","H","G","I")) {
						track[[length(track)+1]] <- list(f=draw.helix,xs=c(start,i-1))
					#if previous was disorder, then end strand
					} else if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] == "Disorder") {
						track[[length(track)+1]] <- list(f=draw.dis,xs=c(start,i-1))
					}
					start <- i
				}
			} else if (!is.na(values[[i]]) && values[[i]] == "Disorder") {
				#if this is the start
				if (i==1 || is.na(values[[i-1]]) || values[[i-1]] != "Disorder") {
					#if previous was helix, then end strand
					if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] %in% c("AlphaHelix","H","G","I")) {
						track[[length(track)+1]] <- list(f=draw.helix,xs=c(start,i-1))
					#if previous was strand, then end strand
					} else if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] %in% c("Strand","E")) {
						track[[length(track)+1]] <- list(f=draw.sheet,xs=c(start,i-1))
					}
					start <- i
				}
			} else {
				if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] %in% c("AlphaHelix","H","G","I")) {
					track[[length(track)+1]] <- list(f=draw.helix,xs=c(start,i-1))
				} else if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] %in% c("Strand","E")) {
					track[[length(track)+1]] <- list(f=draw.sheet,xs=c(start,i-1))
				} else if (i > 1 && !is.na(values[[i-1]]) && values[[i-1]] == "Disorder") {
					track[[length(track)+1]] <- list(f=draw.dis,xs=c(start,i-1))
				}
			}
		}
		.tracks[[length(.tracks)+1]] <<- track
		.labels[[length(.labels)+1]] <<- "Sec.Struc."
		.types[[length(.types)+1]] <<- "ss"
	}

	draw <- function() {
		if (length(.tracks) < 1) stop("no tracks!")
		h <- length(.tracks)
		if (nox) {
			op <- par(las=1,mar=c(0,5,1,0)+.1)
		} else {
			op <- par(las=1,mar=c(5,7,4,2)+.1)
		}
		plot(NA,type="n",xlim=c(-3,l+1),ylim=c(0,h),
			xlab="AA position",axes=FALSE,ylab="",xaxs="i"
		)
		if (!nox) {
			axis(1,at=c(1,seq(5,l,5)),labels=c(1,seq(5,l,5)))
		}
		axis(2,at=h:1-.5,labels=.labels)

		for (i in 1:h) {
			if(.types[[i]] == "ss") {
				for (elem in .tracks[[i]]) {
					elem$f(elem$xs,y=h-i)
				}
			} else if (.types[[i]] == "heat") {
				pos <- 1:l
				rect(pos-.5,h-i,pos+.5,h-i+1,border=NA,col=.tracks[[i]])
			} else if (.types[[i]] == "cons") {
				pal <- colorRampPalette(c("saddlebrown","gold"))(12)
				pos <- 1:l
				bh <- .tracks[[i]]/12
				rect(pos-.5,h-i,pos+.5,h-i+bh,border=NA,col=pal[.tracks[[i]]+1])
			}
		}
		par(op)
	}

	num.tracks <- function() {
		return(length(.tracks))
	}

	debug <- function() {
		cat("Tracks:\n")
		print(.tracks)
		cat("Labels:\n")
		print(.labels)
		cat("Types:\n")
		print(.types)
	}

	list(
		add.track=add.track,
		add.constrack=add.constrack,
		add.ss.track=add.ss.track,
		draw=draw,
		num.tracks=num.tracks,
		debug=debug
	)
}
