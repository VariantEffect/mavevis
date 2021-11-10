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


#' Helper function to draw staggered oversized labels on the y-axis
#' @param at positions of tick marks
#' @param labels tick mark labels
#' @param cex character expansion factor (relative font size)
#' @return NULL
axisHack <- function(at,labels,cex=1) {
  zero <- par("usr")[[1]]
  marginSize <- grconvertX(par("mar")[[2]],"lines","device")
  unitSize <- grconvertX(zero+1,"user","device")-grconvertX(zero,"user","device")
  m2u <- function(x) zero-x*marginSize/unitSize

  op <- par(xpd=TRUE)
  segments(m2u(.1),min(at),m2u(.1),max(at))
  segments(m2u(.2),at,m2u(.1),at)
  text(rep(c(m2u(.4),m2u(.6)),length(labels)/2), at, labels,cex=cex)
  par(op)

  return(invisible(NULL))
}

#' Draw genophenogram plot 
#' 
#' Draws a genophenogram plot from given data
#' 
#' @param wt.aa wildtype amino acid sequence as a vector of single characters.
#' @param pos vector of amino acid positions
#' @param mut.aa vector of mutant AAs
#' @param score vector of scores
#' @param error vector of stderr values
#' @param a bezier transformation intensity (with -0.5 <= a <= 0.5)
#' @param grayBack draw a gray background for incomplete maps
#' @param img.width optional parameter to inform the drawing function of the chosen image width,
#'      allowing it to adjust the size of the legend
#' @param tracks an optional trackdrawer object to add structural information to the plot
#' @param pixelMap boolean flag, whether or not to return a pixel coordinate map object
#' @export
#' @return NULL, or a pixel coordinate map, i.e. a list with two data frames
genophenogram <- function(wt.aa, pos, mut.aa, score, syn.med, stop.med, 
	error=NULL, a=0, grayBack=FALSE, img.width=12, tracks=NULL, pixelMap=FALSE) {

	library("yogitools")

	#determine if wt.aas covers position range exactly or describes full ORF
	coverLength <- max(pos,na.rm=TRUE)-min(pos,na.rm=TRUE)+1
	if (length(wt.aa) > coverLength) {
		#a longer wt seq indicates the user wants the full maps (despite small coverage)
		#make sure that the wt.aa still ranges to the end
		if (max(pos,na.rm=TRUE) <= length(wt.aa)) {
			#the position where we start drawing
			startPos <- 1
			#and end drawing
			endPos <- length(wt.aa)
		} else {
			stop("wt.aa does not match position range!")
		}
	} else {
		#if it's equal or less we just draw the coverage range
		startPos <- min(pos,na.rm=TRUE)
		endPos <- max(pos,na.rm=TRUE)
		if (length(wt.aa) < coverLength) {
			#also if the WT sequence isn't long enough we need to warn the user
			warning("wt.aa does not match position range! Supplementing...")
			#and supplement the sequence
			wt.aa <- c(wt.aa,rep("*",coverLength-length(wt.aa)))
		}
	}

	#define bezier transformation function
	bend <- function(x,a=0) {
		if (is.na(x)) return(NA)
		if (a == 0) return(x)
		if (abs(a) > 0.5) stop("parameter a must be between -0.5 and 0.5")
		if (x < 0) x <- 0
		if (x > 1) return(x)
		q <- (1 - 2*a) / (4*a)
		t <- ifelse(a >= 0,1,-1) * sqrt(x/(2*a) + q^2) - q
		(1+2*a)*t - 2*a*t^2
	}
	#if desired, apply the transformation to the scores
	if (a != 0) {
		score <- sapply(score,bend,a=a)
	}

	#calculate the relative space in the plot to be allocated for legends and feature tracks.
	legend.share <- 1.8/img.width
	# layout(cbind(c(3,1),c(4,2)),widths=c(9.5,.5),heights=c(2,9))
	if (is.null(tracks)) {
		layout(cbind(c(3,1),c(4,2)),widths=c(1-legend.share,legend.share),heights=c(2,9))
	} else {
		track.share <- tracks$num.tracks()/2.5
		layout(cbind(c(4,3,1),c(5,5,2)),widths=c(1-legend.share,legend.share),heights=c(track.share,2,9))
	}

	#Main plot
	###########
	aas <- toChars("AVLIMFYWRHKDESTNQGCP*")
	#set up the empty plot space for the main panel
	op <- par(cex=.8,las=1,mar=c(5,5,0,0)+.1)
	plot(NA,type="n",
		xlim=c(startPos-4.5,endPos+1),ylim=c(0,length(aas)+1),axes=FALSE,
		xaxs="i",xlab="AA position",ylab="",main=""
	)
	#add x and y axes
	# axis(1,c(1,seq(5,length(wt.aa),5)))
	axis(1,seq((startPos %/% 5)*5,(endPos%/%5)*5,5),cex.axis=1.5)
	# axis(2,at=1:21,labels=rev(aas))
	axisHack(at=1:21,labels=rev(aas),cex=1.5)
	mtext("AA residue",side=2,line=4,las=3)

	# #add amino acid group labels
	# text(-1,c(17.5,9),c("hydrophobic","polar"),srt=90)
	# text(-2.33,c(9.5,12),c("-","+"),srt=90)
	# arrows(-1.66,c(4.6,13.6),-1.66,c(13.4,21.4),length=.02,angle=90,code=3)
	# arrows(-3,c(8.6,10.6),-3,c(10.4,13.4),length=.02,angle=90,code=3)
	text(startPos-2,c(17.5,9),c("hydrophobic","polar"),srt=90)
	text(startPos-3.33,c(9.5,12),c("-","+"),srt=90)
	arrows(startPos-2.66,c(4.6,13.6),startPos-2.66,c(13.4,21.4),length=.02,angle=90,code=3)
	arrows(startPos-4,c(8.6,10.6),startPos-4,c(10.4,13.4),length=.02,angle=90,code=3)

	#draw gray background if desired
	if (grayBack) {
		rect(startPos-.5,.5,endPos+.5,length(aas)+.5,col="gray",border=NA)
	}

	#supplement missing wt-positions
	for (i in startPos:endPos) {
		aa <- wt.aa[[i-startPos+1]]
		if (!any(pos==i & mut.aa==aa,na.rm=TRUE)) {
			pos <- c(pos,i)
			mut.aa <- c(mut.aa,aa)
			score <- c(score,NA)
			if (!is.null(error)) {
				error <- c(error,NA)
			}
		}
	}

	#calculate coordinates and colors for the main heatmap rectangles
	x <- pos
	y <- length(aas) - sapply(mut.aa,function(a) if (is.na(a)) NA else which(aas==a)) + 1
	neutral.bottom <- stop.med+(syn.med-stop.med)*0.8
	neutral.top <- stop.med+(syn.med-stop.med)*1.2
	syn.top <- syn.med+(syn.med-stop.med)
	cm <- colmap(
		valStops=c(stop.med, neutral.bottom, neutral.top, syn.top), 
		colStops=c("royalblue3","white","white","firebrick3")
	)
	cols <- cm(score)
	# colRamp <- colorRampPalette(c("royalblue3","white","firebrick3"))(11)
	# scoreToColIdx <- function(score) sapply(score,function(s) {
	# 	if (is.na(s)) {
	# 		NA
	# 	} else if (s < stop.med) {
	# 		1
	# 	} else if (s < syn.med) {
	# 		round(5*(s-stop.med)/(syn.med-stop.med))+1
	# 	} else if (s < 2*syn.med-stop.med) {
	# 		round(5*(s-syn.med)/(syn.med-stop.med))+6
	# 	} else {
	# 		11
	# 	}
	# })
	# colIdx <- scoreToColIdx(score)
	# cols <- colRamp[colIdx]

	#change wt positions to gold color
	cols[which(mut.aa==wt.aa[pos-startPos+1])] <- "lightgoldenrod1"

	#draw the heatmap rectangles
	rect(x-.5,y-.5,x+.5,y+.5,col=cols,border=NA)

	#if user requested a pixel map of the plot, generate one
	if (pixelMap) {
		pxmap <- list()
		pxmap$main <- data.frame(
			pos=x, wt=wt.aa[x-startPos+1], aa=mut.aa,
			score=score, error=if(!is.null(error)) error else NA,
			x0=round(grconvertX(x-0.5,"user","device")),
			x1=round(grconvertX(x+0.5,"user","device")),
			y0=round(grconvertY(y-0.5,"user","device")),
			y1=round(grconvertY(y+0.5,"user","device"))
		)
	}

	#draw error bars (if provided)
	if (!is.null(error)) {
		#normalize to distance between synonymous and nonsense
		e <- .5 * error / (syn.med-stop.med)
		#cut-off anything with error greater than the full distance
		#(will be crossed out by a second bar below)
		e[which(e>.5)] <- .5
		#remove bars from wt, as they are just yellow anyway
		e[which(mut.aa==wt.aa[pos])] <- NA
		#draw the segements
		segments(x-e,y-e,x+e,y+e)
		#indices of boxes with excessive (>1 error)
		e2i <- which(error > (syn.med-stop.med))
		#cross out those boxes
		segments(x[e2i]-.5,y[e2i]+.5,x[e2i]+.5,y[e2i]-.5)
	}

	par(op)

	#####Legend
	###########
	op <- par(cex=.8,mar=c(5,3,0,4)+.1)
	plot(NA,type="n",xlim=c(-1,1),ylim=c(0,13),axes=FALSE,xlab="",ylab="")
	#autodetect above wt-level scores and draw appropriate legend
	if (any(score > syn.med, na.rm=TRUE)) {#with red colors
		# rect(0,0:11,1,1:12,col=c(colRamp[1:6],colRamp[6:11]),border=NA)
		rect(0,0:11,1,1:12,col=cm(seq(stop.med,syn.top,length.out=12)),border=NA)
		rect(0,12,1,13,col="lightgoldenrod1",border=NA)
		axis(4,at=c(.5,6,11.5,12.5),labels=c("stop","syn","hyper","wt"))
	} else {#without red colors
		# rect(0,seq(0,10,2),1,seq(2,12,2),col=colRamp[1:6],border=NA)
		rect(0,seq(0,10,2),1,seq(2,12,2),col=cm(seq(stop.med,syn.med,length.out=6)),border=NA)
		rect(0,12,1,13,col="lightgoldenrod1",border=NA)
		axis(4,at=c(1,11,12.5),labels=c("stop","syn","wt"))
	}
	mtext("score",side=4,line=2,las=3,cex=0.7)
	if(!is.null(error)) {
		es <- seq(0,0.5,length.out=13)/2
		ys <- 0:12+0.5
		segments(-.5-es,ys-es,-.5+es,ys+es)
		me <- syn.med-stop.med
		axis(2,at=c(.5,6.5,12.5),labels=signif(c(0,me/2,me),2))
		mtext("stderr",side=2,line=2,las=3,cex=0.7)
	}
	par(op)

	#Summary bars
	###########

	#barvals is a matrix with n rows (for each position) and 11 columns (for 11 score bins)
	#storing the relative shares of each bin for each position
	breaks <- seq(stop.med,syn.top,length.out=12)
	breaks[[1]] <- -Inf; breaks[[12]] <- Inf
	binMids <- sapply(1:11,function(i)(breaks[[i+1]]+breaks[[i]])/2)
	barvals <- do.call(rbind,tapply(score,x,function(spp) {
		hist(spp,breaks=breaks,plot=FALSE)$counts
	}))
	# barvals <- do.call(rbind,tapply(colIdx,x,function(idxs) {
	# 	table(factor(idxs,levels=1:11))
	# }))
	# barvals <- apply(barvals,2,`/`,table(pos))
	barvals <- t(apply(barvals,1,function(xs)xs/sum(xs)))
	barcums <- cbind(0,t(apply(barvals,1,function(x)sapply(1:11,function(i)sum(x[1:i])))))

	par(cex=.8,mar=c(0,5,1,0)+.1)
	plot(
		0,type="n",
		xlim=c(startPos-4.5,endPos+1),
		# xlim=c(-3.5,length(wt.aa)+1),
		ylim=c(0,1),
		axes=FALSE,xlab="",xaxs="i",
		ylab="census"
	)
	# n <- length(wt.aa)
	n <- nrow(barcums)
	# if (n != length(wt.aa)) {
	# 	warning("WT sequence does not match length of matrix!")
	# }

	#draw gray background if desired
	if (grayBack) {
		rect(.5,0,length(wt.aa)+.5,1,col="gray",border=NA)
	}

	xs <- as.integer(rownames(barcums))

	for (i in 1:11) {
		# rect(1:n-.5,barcums[,i],1:n+.5,barcums[,i]+barvals[,i],col=colRamp[[i]],border=NA)	
		rect(xs-.5,barcums[,i],xs+.5,barcums[,i+1],col=cm(binMids[[i]]),border=NA)	
	}

	#if user requested a pixel map of the plot, generate one for summary bar as well
	if (pixelMap) {
		pxmap$summary <- cbind(
			data.frame(pos=xs, wt=wt.aa[xs-startPos+1]),
			do.call(rbind,tapply(score,x,summary,na.rm=TRUE)),
			data.frame(
				x0=round(grconvertX(xs-0.5,"user","device")),
				x1=round(grconvertX(xs+0.5,"user","device")),
				y0=round(grconvertY(0,"user","device")),
				y1=round(grconvertY(1,"user","device"))
			)
		)
	}

	axis(2,at=0:1,labels=0:1)
	par(op)


	#####Tracks
	#############
	if (!is.null(tracks)) {

		tracks$draw()

		### Track legend
		op <- par(cex=.8,mar=c(0,3,1,4)+.1)
		plot(NA,type="n",xlim=c(-1,1),ylim=c(0,11),axes=FALSE,xlab="",ylab="")
		orangeRamp <- colorRampPalette(c("white","orange"))(11)
		blueRamp <- colorRampPalette(c("white","steelblue3"))(11)
		rect(0,0:10,1,1:11,col=orangeRamp,border=NA)
		rect(-1,0:10,0,1:11,col=blueRamp,border=NA)

		axis(4,at=c(.5,5.5,10.5),labels=c("0%","50%","100%"))
		mtext("interface burial",side=4,line=2,las=3,cex=0.7)
		axis(2,at=c(.5,5.5,10.5),labels=c("0%","50%","100%"))
		mtext("surface accessibility",side=2,line=2,las=3,cex=0.7)
		par(op)

	}
	
	if (pixelMap) {
		return(pxmap)
	} else {
		return(invisible(NULL))
	}
}




#' Draw genophenogram plot for nucleotide sequences
#' 
#' Draws a genophenogram plot from given data
#' 
#' @param wt.nc wildtype amino acid sequence as a vector of single characters.
#' @param pos vector of amino acid positions
#' @param mut.nc vector of mutant AAs
#' @param score vector of scores
#' @param error vector of stderr values
#' @param a bezier transformation intensity (with -0.5 <= a <= 0.5)
#' @param grayBack draw a gray background for incomplete maps
#' @param img.width optional parameter to inform the drawing function of the chosen image width,
#'      allowing it to adjust the size of the legend
#' @param tracks an optional trackdrawer object to add structural information to the plot
#' @export
#' @return NULL
genophenogram.nc <- function(wt.nc, pos, mut.nc, score, syn.med, stop.med, 
	error=NULL, a=0, grayBack=FALSE, img.width=12, tracks=NULL) {

	library("yogitools")

	bend <- function(x,a=0) {
		if (is.na(x)) return(NA)
		if (a == 0) return(x)
		if (abs(a) > 0.5) stop("parameter a must be between -0.5 and 0.5")
		if (x < 0) x <- 0
		if (x > 1) return(x)
		q <- (1 - 2*a) / (4*a)
		t <- ifelse(a >= 0,1,-1) * sqrt(x/(2*a) + q^2) - q
		(1+2*a)*t - 2*a*t^2
	}
	if (a != 0) {
		score <- sapply(score,bend,a=a)
	}

	legend.share <- 1.4/img.width
	# layout(cbind(c(3,1),c(4,2)),widths=c(9.5,.5),heights=c(2,9))
	if (is.null(tracks)) {
		layout(cbind(c(3,1),c(4,2)),widths=c(1-legend.share,legend.share),heights=c(2,9))
	} else {
		track.share <- tracks$num.tracks()/2.5
		layout(cbind(c(4,3,1),c(5,5,2)),widths=c(1-legend.share,legend.share),heights=c(track.share,2,9))
	}
	#Main plot
	###########
	# aas <- c("A","V","L","I","M","F","Y","W","R","H","K","D","E","S","T","N","Q","G","C","P","*")
	triplets <- c(
		"A--","C--","G--","T--",
		"-A-","-C-","-G-","-T-",
		"--A","--C","--G","--T"
	)
	op <- par(cex=.6,las=1,mar=c(5,5,0,0)+.1)
	plot(NA,type="n",
		xlim=c(-3.5,length(wt.nc)/3+1),ylim=c(0,length(triplets)+1),axes=FALSE,
		xaxs="i",xlab="Codon position",ylab="mutation",main=""
	)
	axis(1,c(1,seq(5,length(wt.nc)/3+1,5)))
	axis(2,at=1:length(triplets),labels=rev(triplets))

	# text(-1,c(17.5,9),c("hydrophobic","polar"),srt=90)
	# text(-2.33,c(9.5,12),c("-","+"),srt=90)
	# arrows(-1.66,c(4.6,13.6),-1.66,c(13.4,21.4),length=.02,angle=90,code=3)
	# arrows(-3,c(8.6,10.6),-3,c(10.4,13.4),length=.02,angle=90,code=3)

	text(-1,c(2.5,6.5,10.5),c("3rd","2nd","1st"),srt=90)
	arrows(-1.66,c(.6,4.6,8.6),-1.66,c(4.4,8.4,12.4),length=.02,angle=90,code=3)

	#draw gray background if desired
	if (grayBack) {
		rect(.5,.5,length(wt.nc)/3+.5,length(triplets)+.5,col="gray",border=NA)
	}

	#supplement missing wt-positions
	for (i in 1:length(wt.nc)) {
		base <- wt.nc[[i]]
		if (!any(pos==i & mut.nc==base,na.rm=TRUE)) {
			pos <- c(pos,i)
			mut.nc <- c(mut.nc,base)
			score <- c(score,NA)
			if (!is.null(error)) {
				error <- c(error,NA)
			}
		}
	}

	x <- (pos-1) %/% 3 + 1
	bases <- c("A","C","G","T")
	y <- 13 - (sapply(mut.nc,function(base) which(bases==base)) + ((pos-1) %% 3)*4)
	# y <- length(aas) - sapply(mut.aa,function(a) if (is.na(a)) NA else which(aas==a)) + 1
	neutral.bottom <- stop.med+(syn.med-stop.med)*0.8
	neutral.top <- stop.med+(syn.med-stop.med)*1.2
	syn.top <- syn.med+(syn.med-stop.med)
	cm <- colmap(
		valStops=c(stop.med, neutral.bottom, neutral.top, syn.top), 
		colStops=c("royalblue3","white","white","firebrick3")
	)
	cols <- cm(score)
	
	# colRamp <- colorRampPalette(c("royalblue3","white","firebrick3"))(11)
	# scoreToColIdx <- function(score) sapply(score,function(s) {
	# 	if (is.na(s)) {
	# 		NA
	# 	} else if (s < stop.med) {
	# 		1
	# 	} else if (s < syn.med) {
	# 		round(5*(s-stop.med)/(syn.med-stop.med))+1
	# 	} else if (s < 2*syn.med-stop.med) {
	# 		round(5*(s-syn.med)/(syn.med-stop.med))+6
	# 	} else {
	# 		11
	# 	}
	# })
	# colIdx <- scoreToColIdx(score)
	# cols <- colRamp[colIdx]

	#change wt positions to gold color
	cols[which(mut.nc==wt.nc[pos])] <- "lightgoldenrod1"

	rect(x-.5,y-.5,x+.5,y+.5,col=cols,border=NA)

	segments(-.5,c(4.5,8.5),length(wt.nc)/3+1,c(4.5,8.5),lty="dashed")

	if (!is.null(error)) {
		# e <- .5 * error / max(error)
		e <- .5 * error / (syn.med-stop.med)
		e[which(mut.nc==wt.nc[pos])] <- NA
		segments(x-e,y-e,x+e,y+e)
	}

	par(op)

	#####Legend
	###########
	op <- par(cex=.6,mar=c(5,3,0,4)+.1)
	plot(NA,type="n",xlim=c(-1,1),ylim=c(0,13),axes=FALSE,xlab="",ylab="")
	if (any(score > syn.med, na.rm=TRUE)) {#with red colors
		# rect(0,0:11,1,1:12,col=c(colRamp[1:6],colRamp[6:11]),border=NA)
		rect(0,0:11,1,1:12,col=cm(seq(stop.med,syn.top,length.out=12)),border=NA)
		rect(0,12,1,13,col="lightgoldenrod1",border=NA)
		axis(4,at=c(.5,6,11.5,12.5),labels=c("stop","syn","hyper","wt"))
	} else {#without red colors
		# rect(0,seq(0,10,2),1,seq(2,12,2),col=colRamp[1:6],border=NA)
		rect(0,seq(0,10,2),1,seq(2,12,2),col=cm(seq(stop.med,syn.med,length.out=6)),border=NA)
		rect(0,12,1,13,col="lightgoldenrod1",border=NA)
		axis(4,at=c(1,11,12.5),labels=c("stop","syn","wt"))
	}
	mtext("score",side=4,line=2,las=3,cex=0.7)
	if(!is.null(error)) {
		es <- seq(0,0.5,length.out=13)/2
		ys <- 0:12+0.5
		segments(-.5-es,ys-es,-.5+es,ys+es)
		me <- syn.med-stop.med
		axis(2,at=c(.5,6.5,12.5),labels=signif(c(0,me/2,me),2))
		mtext("stderr",side=2,line=2,las=3,cex=0.7)
	}
	par(op)

	#Summary bars
	###########
	#barvals is a matrix with n rows (for each position) and 11 columns (for 11 score bins)
	#storing the relative shares of each bin for each position
	breaks <- seq(stop.med,syn.top,length.out=12)
	breaks[[1]] <- -Inf; breaks[[12]] <- Inf
	binMids <- sapply(1:11,function(i)(breaks[[i+1]]+breaks[[i]])/2)
	barvals <- do.call(rbind,tapply(score,x,function(spp) {
		hist(spp,breaks=breaks,plot=FALSE)$counts
	}))
	
	# barvals <- do.call(rbind,tapply(colIdx,x,function(idxs) {
	# 	table(factor(idxs,levels=1:11))
	# }))
	# barvals <- apply(barvals,2,`/`,table(pos))
	barvals <- t(apply(barvals,1,function(xs)xs/sum(xs)))
	barcums <- cbind(0,t(apply(barvals,1,function(x)sapply(1:11,function(i)sum(x[1:i])))))

	par(cex=.6,mar=c(0,5,1,0)+.1)
	plot(
		0,type="n",
		xlim=c(-3.5,length(wt.nc)/3+1),
		ylim=c(0,1),
		axes=FALSE,xlab="",xaxs="i",
		ylab="pos/neutral/neg"
	)
	# n <- length(wt.nc)/3
	n <- nrow(baracums)
	if (n != length(wt.nc)/3) {
		warning("WT sequence does not match length of matrix!")
	}

	#draw gray background if desired
	if (grayBack) {
		rect(.5,0,length(wt.nc)/3+.5,1,col="gray",border=NA)
	}

	for (i in 1:11) {
		# rect(1:n-.5,barcums[,i],1:n+.5,barcums[,i]+barvals[,i],col=colRamp[[i]],border=NA)
		rect(1:n-.5,barcums[,i],1:n+.5,barcums[,i]+barvals[,i],col=colRamp[[i]],border=NA)
	}
	axis(2)
	par(op)


	#####Tracks
	#############
	if (!is.null(tracks)) {

		tracks$draw()

		### Track legend
		op <- par(cex=.6,mar=c(0,3,1,4)+.1)
		plot(NA,type="n",xlim=c(-1,1),ylim=c(0,11),axes=FALSE,xlab="",ylab="")
		orangeRamp <- colorRampPalette(c("white","orange"))(11)
		blueRamp <- colorRampPalette(c("white","steelblue3"))(11)
		rect(0,0:10,1,1:11,col=orangeRamp,border=NA)
		rect(-1,0:10,0,1:11,col=blueRamp,border=NA)

		axis(4,at=c(.5,5.5,10.5),labels=c("0%","50%","100%"))
		mtext("relative interface burial",side=4,line=2,las=3,cex=0.7)
		axis(2,at=c(.5,5.5,10.5),labels=c("0%","50%","100%"))
		mtext("relative surface accessibility",side=2,line=2,las=3,cex=0.7)
		par(op)

	}
	
	return(invisible(NULL))
}
