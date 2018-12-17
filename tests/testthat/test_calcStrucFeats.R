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

