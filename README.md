# MaveVis
Query data from MaveDB (https://www.mavedb.org/) and visualize as genophenograms with added tracks for structure information.

MaveVis is deployed at http://vis.varianteffect.org . The webtool is encapsulated in a Docker container, which can be built using the makefile in the docker/ directory. MaveVis also works as an R-package, which can be installed via devtools, but requires multiple dependencies:

## Requires:
 * R 3.1.2 or higher
 * An installation of DSSP
 * An installation of OpenSASA
 * An installation of ClustalOmega
 * hgvsParseR (see https://github.com/VariantEffect/hgvsParseR)
 * rapimave (see https://github.com/VariantEffect/rapimave)
 * yogitools (see https://github.com/jweile/yogitools)
 * httr
 * gdata
 * hash
 
## Recommended:
 * devtools
 * testthat
 * roxygen2
 * cgir (see https://github.com/jweile/cgir)

