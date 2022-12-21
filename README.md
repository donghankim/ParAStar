# Parallel A*
Parallel, functional A* pathfinder on OpenStreetMap

### Local Run
```bash
cd astar/

# create python vevn & install dependencies
python3 -m virtualenv venv
pip3 install -r requirements.txt

# to run astar
stack exec astar-exe

# start repl
stack repl

# to time execution + no. core
stack exec aseq +RTS -N2
stack exec apar +RTS -N12

```

### Resources

A* search
https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2

OpenStreetMap
https://www.openstreetmap.org/search?whereami=1&query=40.80863%2C-73.96156#map=18/40.80863/-73.96156

OSMnx
https://github.com/gboeing/osmnx

Faster file processing with bystestring and regex
https://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html

Parallel computation review:
https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch02.html

Vincenty Formula
https://nathanrooy.github.io/posts/2016-12-18/vincenty-formula-with-python/

GHCi Debug
https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/ghci.html#the-ghci-debugger
