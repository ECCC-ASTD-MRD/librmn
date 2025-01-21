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


test_row = index_df.iloc[1]

records = list(rmn.fst24_file.get_records_with_data(test_row['path'], [test_row['file_index']]))
print(records[0].data[:,:,0])

one_file = index_df[index_df['path'] == str(filenames[0])]
one_file_indices = one_file['file_index']
print(one_file_indices)

for rec in rmn.fst24_file.get_records_with_data(str(filenames[0]), one_file_indices):
    print(rec)
    print(rec.data[:,:,0])
    

