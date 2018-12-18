# MaveVis: Visualization for MaveDB
A tool to visualize variant effect maps from MaveDB [https://www.mavedb.org/](https://www.mavedb.org/) as genophenograms with added tracks for structure information. MaveVis is deployed as a webtool at [http://vis.varianteffect.org](http://vis.varianteffect.org).

Mavevis can be used in multiple different modes:

* A [webtool]($ Webtool), deployed as a docker container
* A [RestFul webservice](# RestFul service) available from the same docker deployment
* An [R-package](# R-package)
* A [command line tool](CLI script)

## Webtool
The webtool can be found at [http://vis.varianteffect.org](http://vis.varianteffect.org). But if you'd like to deploy it yourself you can get the built docker image at [https://hub.docker.com/r/jweile/mavevis](https://hub.docker.com/r/jweile/mavevis). You will of course need an installation of [Docker](https://www.docker.com/get-started). Then you can pull the image and deploy it as a container:

```bash
$ docker pull jweile/mavevis:latest

#Assuming we want to map the web interface to port 80:
$ docker run -t -p 80:80 --name mavevis jweile/mavevis:latest
```

If you insist on building the image from scratch, you can check out the git repo, and use the provided make file:

```bash
$ git clone https://github.com/VariantEffect/mavevis.git
$ cd docker
$ make build
```

## RestFul service
A detailed manual of the webservice can be found [here](http://vis.varianteffect.org/help.html).

## R-package
At the most basic level, MaveVis is available as an R-package, with the following dependencies:

### Requires:
 * R 3.1.2 or higher
 * An installation of [DSSP](https://github.com/cmbi/xssp/releases) (tested with v3.0.0)
 * An installation of [FreeSASA](https://freesasa.github.io/) (tested with v2.0.2)
 * An installation of [ClustalOmega](http://www.clustal.org/omega/) (tested with v1.2.4)
 * [hgvsParseR](https://github.com/VariantEffect/hgvsParseR)
 * [rapimave](https://github.com/VariantEffect/rapimave)
 * [yogitools](https://github.com/jweile/yogitools)
 * httr
 * gdata
 * hash

### Recommended:
 * devtools
 * testthat
 * roxygen2
 * [cgir](https://github.com/jweile/cgir)

###Installation of the R-package:
#### Ubuntu/Debian/etc:

```bash
#Use apt to install meta-dependencies
$ sudo apt install r-base wget g++ make libcurl4-openssl-dev libssl-dev \\
    libxml2 libxml2-dev libjson-c-dev dssp clustalo

#Download and build FreeSASA
$ wget https://github.com/mittinatten/freesasa/releases/download/2.0.2/freesasa-2.0.2.tar.gz
$ tar xzf freesasa-2.0.2.tar.gz
$ cd freesasa-2.0.2
$ ./configure --disable-xml
$ make
$ sudo make install

#Clean-up build directory
$ cd .. &&\
$ rm -r freesasa*

#Install R-packages
$ R
> install.packages("devtools")
> library(devtools)
> install_github("jweile/yogitools")
> install_github("jweile/cgir")
> install_github("VariantEffect/hgvsParseR")
> install_github("VariantEffect/rapimave")
> install_github("VariantEffect/mavevis")
> q()

```
###Using the R-package
A full manual for all functions included in the R-package can be found [here](manual.pdf).

## CLI script
The main dashboard() function in the R-package can also be accessed as a CLI script, to avoid having to open an interactive session. You need to install the R-package as described above to use the script. You can then download the [mavevis_launcher.R](docker/mavevis_launcher.R) script from the docker directory and use it as follows: 

```bash
Rscript mavevis_launcher.R scoresetID=<ssid> uniprot=<uniprot>
     pdb=<pdb-ids> mainChain=<chains> [WT=<seq> | seqOffset=<num> |
     synMed=<num> | stopMed=<num> | pngRes=<num> | outFormats={png|pdf|svg}]
```

The following parameters are mandatory: `ssid` is the URI of a score set in MaveDB, `uniprot` is the uniprot accession corresponding to the target of the scoreset, `pdb` is a comma-separated list of PDB accessions to use for the structure tracks, `mainChain` is a comma-separated list of PDB chain identifiers (e.g. A)

Depending on the scoreset, the following parameters may also be required: `seqOffset` the offset of the scoreset position indexes with respect to the uniprot sequence; `synMed` the expected score of a synonymous or wt-like variant; `stopMed` the expected score of a null-like or stop variant.

Finally, the following parameters are optional: `WT` is the WT sequence, which defaults to the one provided by MaveDB; `outFormats` is a comma-separated list of output file formats, with allowed values png, pdf, and svg; `pngRes` is the resolution (in DPI) to be used for any PNG output file.

