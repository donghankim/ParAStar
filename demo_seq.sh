#!/bin/sh

cd astar && stack exec seq 
cd .. && ./graph.py -p "Columbia University"
