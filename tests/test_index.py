#!/usr/bin/env python3
import rmn
import os
import pandas
import timeit
import pathlib
print("done importing (cause importing pandas is the longest part of this script)")

filenames = list(pathlib.Path("/home/sici000/ci_data/libgeoref/grids/").glob('*.fstd'))

index_df = rmn.get_index_data_frame(filenames)
print(index_df)

print(index_df['path'])
for p in index_df['path']:
    print(p)
