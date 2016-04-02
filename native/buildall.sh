#!/bin/sh

cd compress
./build.sh
cd ../algorithm
./build.sh
cd ..
echo "DONE!"
