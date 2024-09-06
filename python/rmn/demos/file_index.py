#!/usr/bin/env python3

import rmn
import os
import pandas
import re

import argparse

p = argparse.ArgumentParser()
p.add_argument('-d', '--dir', help="Directory to search", default="/fs/site5/eccc/prod/ops/suites/gdps_20240611/g1/gridpt.usr/prog/pres")
p.add_argument('-p', '--pattern', help="Use only files in DIR matching PATTERN")
p.add_argument('-m', '--max', type=int, help="Cap the number of files to index")
args = p.parse_args()

def get_index(filenames):
    # C extension creates a dictionary whose keys are column names
    # and whose values are numpy arrays uniform type.
    # This dictionary can be given to the constructor of pandas.DataFrame
    # once we are back in the Python world
    columns = rmn.get_index_columns(filenames)
    return pandas.DataFrame(columns)


if args.pattern:
    pattern = re.compile(args.pattern)
    filenames = [f"{args.dir}/{f}" for f in os.listdir(args.dir) if pattern.search(f)]
else:
    filenames = [f"{args.dir}/{f}" for f in os.listdir(args.dir)]

if args.max:
    filenames = filenames[:args.max]

print(f"Number of files: {len(filenames)}")
index = get_index(filenames)
print(f"Indexed {len(filenames)} files and found a total of {len(index)} records")

print(f"DataFrame resulting from indexing {len(filenames)} files:")
print(index)
print(f"Single column from that DataFrame")
print(index['etiket'])
