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

