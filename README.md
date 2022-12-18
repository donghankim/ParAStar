# Parallel A*
Parallel, functional A* pathfinder on OpenStreetMap

### Local Run
```bash
cd astar/

# to run astar
stack exec astar-exe

# start repl
stack repl

# to time execution + no. core
stack exec aseq +RTS -N2
stack exec apar +RTS -N12

```

### Resources

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
