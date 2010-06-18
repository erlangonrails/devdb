#! /bin/bash
# Program:
#    analyze source code
 dialyzer --src -I ./include/ -c src/node/ src/mochiweb src/client src/manager
