#!/bin/bash

Rscript /setup/daemon.R &

apachectl -DFOREGROUND
