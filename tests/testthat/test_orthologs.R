library("mavevis")

context("orthologs")

test_that("find PDBs HTTP request",{
	
	# alFile <- system.file("testdata","test_alignment.fasta",package="mavevis")
	
	result <- find.pdbs("P63279")

	expect_length(result,7)

	expect_gt(nrow(result),1)

	expect_true(file.exists(paste0(Sys.getenv("HOME"),"/.mavecache/P63279_pdbs.csv")))

})


test_that("informative PDB selection works",{
	
	# alFile <- system.file("testdata","test_alignment.fasta",package="mavevis")
	
	result <- find.pdbs("P63279")

	inform <- pdb.informative(result)

	expected <- c("5D2M","3UIP","5FQ2","4Y1L","3A4S","2XWU","2O25","1KPS")

	expect_equal(inform,expected)

})


test_that("find PDBs cache recovery",{
		
	cacheFile <- paste0(Sys.getenv("HOME"),"/.mavecache/P63279_pdbs.csv")

	expect_true(file.exists(cacheFile))

	result <- find.pdbs("P63279")

	expect_length(result,7)

	expect_gt(nrow(result),1)

	file.remove(cacheFile)

})

context("Uniprot")

test_that("getUniprotSeq works",{
	
	result <- getUniprotSeq("P63279")

	expect_equal(nchar(result),158)

	expect_match(result,"^[ACDEFGHIKLMNPQRSTVWXY]+[\\*]?$")

})

context("conservation")

test_that("calcConservation works",{
	
	cons <- calc.conservation("P63279")

	expect_length(cons,158)

	expect_true(file.exists(paste0(Sys.getenv("HOME"),"/.mavecache/P63279_alignment.fasta")))

})


test_that("calcConservation cache retrieval works",{
	
	cons <- calc.conservation("P63279")

	expect_length(cons,158)

	file.remove(paste0(Sys.getenv("HOME"),"/.mavecache/P63279_alignment.fasta"))
	file.remove(paste0(Sys.getenv("HOME"),"/.mavecache/P63279_orthologs.fasta"))


})

