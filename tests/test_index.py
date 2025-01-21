#!/usr/bin/env python3
import rmn
import os
import pandas
import timeit
import pathlib
print("done importing (cause importing pandas is the longest part of this script)")


# def get_index(filenames):
#     columns = rmn.get_index_columns(filenames)
#     return pandas.DataFrame(columns)
# 
filenames = list(pathlib.Path("/home/sici000/ci_data/libgeoref/grids/").glob('*.fstd'))

index_df = rmn.get_index_data_frame(filenames)
print(index_df)
