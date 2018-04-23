#!/bin/bash

convert favicon64.png -resize 16x16 favicon16.png
convert favicon64.png -resize 32x32 favicon32.png
convert favicon16.png favicon32.png favicon64.png favicon.ico
identify favicon.ico
mv favicon.ico ../cgi/
