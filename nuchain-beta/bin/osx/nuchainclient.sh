#!/bin/bash

OS='osx'

rlwrap -A ./bin/$OS/nuchainclient -c "conf/$(ls conf | grep -m 1 client)" +RTS -N2
