library("mavevis")

context("caching")

test_that("getCacheFile works",{
	
	result <- getCacheFile("test")

	expect_equal(result,paste0(Sys.getenv("HOME"),"/.mavecache/test"))

})


test_that("calc.strucfeats works",{
		
	result <- calc.strucfeats("3UIP","A")

	expect_length(result,21)

	expect_gt(nrow(result),1)

	expect_true(file.exists(getCacheFile("3UIP.pdb")))

})

