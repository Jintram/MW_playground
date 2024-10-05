#!/bin/bash

files=$(ls *.bla | sed 's/\.bla//g')

for f in $files
do
 echo "Processing $f"
 cat ${f}.bla
 # do something on $f
done


