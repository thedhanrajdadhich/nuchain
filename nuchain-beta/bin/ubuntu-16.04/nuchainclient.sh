#!/bin/bash

OS='ubuntu-16.04'

rlwrap -A ./bin/$OS/nuchainclient -c "conf/$(ls conf | grep -m 1 client)" +RTS -N2
