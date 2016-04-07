#!/bin/sh

cd compress
./build.sh
cd ../algorithm
./build.sh
cd ../blacktech
./build.sh
cd ../daemon
./build.sh
cd ..
echo "DONE!"
