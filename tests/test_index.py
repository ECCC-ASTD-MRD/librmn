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
indices = one_file['file_index']

for rec in rmn.fst24_file.get_records_with_data(str(filenames[0]), indices):
    print(rec)
    print(rec.data[:,:,0])

with rmn.fst24_file(str(filenames[0])) as f:
    for rec in f.get_records_by_index(indices):
        print(rec)
        print(rec.data[:,:,0]) # Access to python property data causes data to # be read

with rmn.fst24_file(str(filenames[0])) as f:
    for rec in f.get_records_by_index_with_data(indices):
        print(rec)
        print(rec.data[:,:,0]) # get with data already caused the data to be
                               # read, accessing it does not cause it to be read
                               # a second time

with rmn.fst24_file(str(filenames[0])) as f:
    # the get_records* functions are generators.  We can create a list from them
    # if we want to iterate over them multiple times.
    records = list(f.get_records_by_index_with_data(indices))
    


print(rmn.get_opdict_metadata("I8"))
