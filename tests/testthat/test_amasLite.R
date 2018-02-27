library("mavevis")

context("amasLite")

test_that("amasLite works",{
	
	alFile <- system.file("testdata","test_alignment.fasta",package="mavevis")
	
	amas <- new.amasLite()
	cons <- amas$run(alFile)

	expect_length(cons,101)
})