#!/bin/bash
cabal build hockey-worker
while true
do
    ./dist/build/hockey-worker/hockey-worker
done
