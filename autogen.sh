#!/bin/sh

## autogen.sh - generate all the twisty little files.

gnulib-tool --import havelib
autoreconf -i
