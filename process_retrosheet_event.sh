#!/bin/bash

export LD_LIBRARY_PATH=/usr/local/lib
cd download.folder/unzipped/
cwevent -y 2018 -f 0-96 2018*.EV* > all2018.csv

