#!/bin/bash

## https://github.com/gnab/remark

INPUT=$1
TEMPLATE=$2

awk -v F="$INPUT" '/##MARKDOWN##/ { system ( "cat " F ) } !/##MARKDOWN##/ { print; }' $TEMPLATE
