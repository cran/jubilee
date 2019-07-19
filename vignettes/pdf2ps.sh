#!/bin/bash

cd "$(dirname "$0")"
echo $(pwd)

for filename in ./*.pdf; do
  echo $filename
  pdf2ps $filename
done
