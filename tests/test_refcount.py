#!/usr/bin/env python3

import rmn
import os

def local_file():
    filename = 'xyz.std'
    if os.path.isfile(filename):
        os.remove(filename)
    f = rmn.fst24_file(filename=filename, options="R/W+RSF")

def local_file_with_query():
    filename = 'xyz.std'
    if os.path.isfile(filename):
        os.remove(filename)
    f = rmn.fst24_file(filename=filename, options="R/W+RSF")
    q = f.new_query(nomvar='TT')

def line_local_file():
    filename = 'xyz.std'
    if os.path.isfile(filename):
        os.remove(filename)
    q = rmn.fst24_file(filename=filename, options="R/W+RSF").new_query()
    return q

def ephemeral():
    filename = 'xyz.std'
    if os.path.isfile(filename):
        os.remove(filename)
    rmn.fst24_file(filename=filename, options="R/W+RSF").new_query()
    print("ephemeral: last line")


print("calling local_file")
local_file()
print("Calling local_file_with_query()")
local_file_with_query()
print("Calling line_local_file")
q = line_local_file()
print("Calling ephemeral")
ephemeral()


print("END")


