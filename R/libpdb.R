suppressMessages(library("gdata"))


#' Create new Structure object
#' 
#' Constructor for a PDB structure object.
#' 
#' The resulting object offers the following methods:
#' \itemize{
#' \item get.sequence(chain) Returns the amino acid sequence of the given chain
#' \item get.aa(chain,pos) Returns a data.frame with the atomic information for 
#'    the amino acid determined by the given chain and position.
#' \item get.chains() Returns a vector detailing the chain indentifiers in the structure
#' \item get.atom(chain,pos,name) Returns the atomic information associated with the 
#'    given chain, amino acid position, and atom name.
#' \item subcomplex.combos(chain) !Deprecated! Does not work with NMR structures
#' \item get.chain.info() Returns a dataframe listing which chain corresponds to 
#'     which protein.
#' }
#' 
#' @param pdb.file The PDB structure file to use
#' @return a PDB structure object
#' @export
new.structure <- function(pdb.file) {

	.pdb.file <- pdb.file

	#fixed-width definition of atom entries
	widths <- c(
		tag=6,num=5,sep1=2,
		atom.name=3,aloc=1,
		aa=3,sep2=1,chain=1,seq.pos=4,
		ins=1,sep3=3,x=8,y=8,z=8,
		occ=6,temp=6,sep4=6,segment=4,
		element=2,charge=2
	)
	#dummy cryst1 entry for pdb files
	cryst1 <- "CRYST1    1.000    1.000    1.000  90.00  90.00  90.00 P 1           1   "

	#read file
	atoms <- read.fwf(pipe(paste("grep -P ^ATOM",.pdb.file)),
		widths,
		strip.white=TRUE,
		stringsAsFactors=TRUE
	)
	colnames(atoms) <- names(widths)

	cwidths <- c(
		tag=5,pdbId=6,chain=2,chain.start=5,chain.end=6,
		db=5,xref=10,name=16,ref.start=5,ref.end=7
	)          
	chainIds <- read.fwf(pipe(paste("grep -P ^DBREF",.pdb.file)),
		cwidths,
		strip.white=TRUE,
		stringsAsFactors=TRUE
	)
	colnames(chainIds) <- names(cwidths)

	eucl.dist <- function(x,y) sqrt(sum((x-y)^2))

	get.sequence <- function(chain) {
		subset <- atoms[atoms$chain == chain,]
		s <- tapply(subset$aa,subset$seq.pos, function(x) levels(x)[[unique(x)]])
		s[order(as.integer(names(s)))]
	}

	get.aa <- function(chain,pos) {
		atoms[atoms$chain == chain & atoms$seq.pos == pos,]
	}

	get.chains <- function() {
		levels(atoms$chain)
	}

	get.atom <- function(chain,pos,name) {
		atoms[atoms$chain == chain & atoms$seq.pos == pos & atoms$atom.name==name,]
	}

	subcomplex.combos <- function(chain) {
		other.chains <- setdiff(get.chains(),chain)
		chain.sets <- c(chain,lapply(other.chains,c,chain))
		lapply(chain.sets,function(selchains) {
			pdb.file <- tempfile()
			con <- file(pdb.file,open="w")
			writeLines(cryst1,con)
			write.fwf(atoms[atoms$chain %in% selchains,], con, colnames=FALSE, width=widths, sep="",append=TRUE)
			writeLines("END",con)
			close(con)
			pdb.file
		})
	}

	get.chain.info <- function() {
		chainIds
	}

	# dssp <- function(pdb.file=.pdb.file) {
	# 	dssp.file <- tempfile()
	# 	cols <- c(num=5,residue=5,chain=2,aa=3,structure=10,
	# 		bp1=5,bp2=4,acc=4,no1=12,on1=11,no2=11,on2=11,tco=8,
	# 		kappa=6,alpha=6,phi=6,psi=6,x.ca=7,y.ca=7,z.ca=7)
	# 	system(paste("dssp",pdb.file,"-o",dssp.file))
	# 	dssp.out <- read.fwf(
	# 		pipe(paste("tail -n+$((`grep -n \"  #  RESIDUE\"",dssp.file,"|cut -f1 -d\":\"`+1))",dssp.file,"")),
	# 		widths=cols,
	# 		stringsAsFactors=FALSE,
	# 		strip.white=TRUE
	# 	)
	# 	colnames(dssp.out) <- names(cols)
	# 	dssp.out
	# }

	# surface.acc <- function(pdb.file=.pdb.file) {
	# 	dssp.out <- dssp(pdb.file)
	# 	chains <- unique(dssp.out$chain)
	# 	out <- lapply(chains, function(chain) {
	# 		acc <- dssp.out[dssp.out$chain==chain,"acc"]
	# 		names(acc) <- dssp.out[dssp.out$chain==chain,"residue"]
	# 		acc
	# 	})
	# 	names(out) <- chains
	# 	out
	# }

	# interface.burial <- function() {
	# 	chain.sets <- append(list(get.chains()),get.chains())
	# 	accs <- lapply(chain.sets, function(chains) {
	# 		pdb.file <- tempfile()
	# 		con <- file(pdb.file,open="a")
	# 		writeLines(cryst1,con)
	# 		write.fwf(atoms[atoms$chain %in% chains,], con, colnames=FALSE, width=widths, sep="",append=TRUE)
	# 		writeLines("END",con)
	# 		close(con)
	# 		surface.acc(pdb.file)
	# 	})
	# 	names(accs) <- c("both",get.chains())
	# 	burial <- lapply(get.chains(), function(chain) {
	# 		(accs[[chain]][[chain]]-accs[["both"]][[chain]]) / accs[[chain]][[chain]]
	# 	})
	# 	names(burial) <- get.chains()
	# 	burial
	# }

	# interface.contacts <- function(max.dist=3) {
	# 	chains <- get.chains()
	# 	distances <- apply(atoms[atoms$chain==chains[[1]],c("x","y","z")],1, function(a) {
	# 		apply(atoms[atoms$chain==chains[[2]],c("x","y","z")],1, eucl.dist, a)
	# 	})
	# 	a.atoms <- atoms[atoms$chain==chains[[1]],c("chain","seq.pos","aa","atom.name")]
	# 	b.atoms <- atoms[atoms$chain==chains[[2]],c("chain","seq.pos","aa","atom.name")]
	# 	hits <- which(distances < max.dist, arr.ind=TRUE)
	# 	cbind(
	# 		b.atoms[hits[,1],],
	# 		a.atoms[hits[,2],],
	# 		dist=apply(hits,1,function(idx) distances[idx[[1]],idx[[2]]])
	# 	)
	# }

	structure(
		list(
			get.sequence=get.sequence,
			get.aa=get.aa,
			get.chains=get.chains,
			get.atom=get.atom,
			# surface.acc=surface.acc,
			# interface.burial=interface.burial,
			# interface.contacts=interface.contacts
			subcomplex.combos=subcomplex.combos,
			get.chain.info=get.chain.info
		),
		class="yogi.struc"
	)	
}













